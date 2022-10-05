package gql.interpreter

import cats.mtl._
import gql.resolver._
import cats.data._
import gql.PreparedQuery._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import io.circe._
import io.circe.syntax._
import cats.effect.std.Supervisor
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.FiniteDuration
import cats._
import fs2.concurrent.Signal
import cats.effect.std.Queue
import fs2.Chunk
import gql._

sealed trait Interpreter[F[_]] {
  type W[A] = WriterT[F, Chain[EvalFailure], A]

  def runEdge(
      inputs: Chain[EvalNode[Any]],
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  ): W[Chain[EvalNode[Json]]]

  def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]]

  def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]]

  def runDataField(df: PreparedDataField[F, Any, Any], input: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]]
}

object Interpreter {
  def stitchInto(oldTree: Json, subTree: Json, path: Cursor): Json =
    path.uncons match {
      case None => subTree
      case Some((p, tl)) =>
        p match {
          case GraphArc.Field(_, name) =>
            val oldObj = oldTree.asObject.get
            val oldValue = oldObj(name).get
            val newSubTree = stitchInto(oldValue, subTree, tl)
            oldObj.add(name, newSubTree).asJson
          case GraphArc.Index(index) =>
            val oldArr = oldTree.asArray.get
            oldArr.updated(index, stitchInto(oldArr(index), subTree, tl)).asJson
          case GraphArc.Fragment(_, _) =>
            stitchInto(oldTree, subTree, tl)
        }
    }

  def combineSplit(fails: Chain[EvalFailure], succs: Chain[EvalNode[Json]]): Chain[(CursorGroup, Json)] =
    fails.flatMap(_.paths).map(m => (m, Json.Null)) ++ succs.map(n => (n.cursorGroup, n.value))

  final case class StreamMetadata[F[_]](
      cursor: Cursor,
      initialValue: Any,
      df: PreparedDataField[F, Any, Any],
      edges: List[PreparedEdge[F]] = null,
      cont: Prepared[F, Any] = null
  )

  def constructStream[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F],
      openTails: Boolean
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    StreamSupervisor[F, IorNec[String, Any]](openTails).flatMap { implicit streamSup =>
      val changeStream = streamSup.changes
        .map(_.toList.reverse.distinctBy { case (tok, _, _) => tok }.toNel)
        .unNone

      def runStreamIt(
          root: NonEmptyList[(PreparedField[F, Any], Chain[EvalNode[Any]])]
      ): F[(Chain[EvalFailure], Chain[EvalNode[Json]], Batching[F], Map[Unique.Token, StreamMetadata[F]])] =
        StreamMetadataAccumulator[F, StreamMetadata[F], IorNec[String, Any]].flatMap { sma =>
          val rootFields = root.map { case (k, _) => k }
          for {
            costTree <- Planner.costTree[F](rootFields)
            plan = Planner.plan(costTree)
            executionDeps = Batching.plan[F](rootFields, plan)
            (fails, succs) <- interpret[F](root, executionDeps, schemaState, sma).run
            smaState <- sma.getState
          } yield (fails, succs, executionDeps, smaState)
        }

      final case class RunInput(
          edges: List[PreparedEdge[F]],
          cont: Prepared[F, Any],
          inputValue: Any
      )

      def evaluate(metas: NonEmptyList[RunInput]): F[(Chain[EvalFailure], NonEmptyList[Json], Map[Unique.Token, StreamMetadata[F]])] =
        StreamMetadataAccumulator[F, StreamMetadata[F], IorNec[String, Any]].flatMap { sma =>
          for {
            _ <- F.unit
            costTree <- metas.toList
              .flatTraverse { ri =>
                NonEmptyChain.fromSeq(ri.edges) match {
                  case None      => Planner.costForPrepared[F](ri.cont, 0d)
                  case Some(nec) => Planner.costForEdges[F](nec, ri.cont, 0d).map(_.toList)
                }
              }
              .map(Planner.NodeTree(_))
            planned = Planner.plan2(costTree)
            accumulator <- BatchAccumulator[F](schemaState, planned)
            res <- Supervisor[F].use { sup =>
              val interpreter = new InterpreterImpl[F](sma, accumulator, sup)
              metas.parTraverse { ri =>
                interpreter
                  .runEdge(Chain(EvalNode.empty(ri.inputValue, 42)), ri.edges, ri.cont)
                  .run
                  .map { case (fail, succs) =>
                    val comb = combineSplit(fail, succs)
                    val j = reconstructField[F](ri.cont, comb.toList.map { case (cg, j) => (cg.relativePath, j) })
                    (fail, j)
                  }
              }
            }
            smaState <- sma.getState
            bes <- accumulator.getErrors
            allErrors = Chain.fromSeq(res.toList).flatMap { case (errs, _) => errs } ++ Chain.fromSeq(bes)
          } yield (allErrors, res.map { case (_, j) => j }, smaState)
        }

      val inital = RunInput(Nil, PreparedQuery.Selection(rootSel), rootInput)

      fs2.Stream
        .eval(evaluate(NonEmptyList.one(inital)))
        .flatMap { case (initialFails, initialSuccs, initialSM) =>
          val jo: JsonObject = initialSuccs.reduceLeft(_ deepMerge _).asObject.get

          fs2.Stream(jo).flatMap { initialOutput =>
            fs2.Stream.emit((initialFails, initialOutput)) ++
              changeStream
                .evalMapAccumulate((initialOutput, initialSM)) { case ((prevOutput, activeStreams), changes) =>
                  changes
                    .map { case (k, rt, v) => activeStreams.get(k).map((k, rt, v, _)) }
                    .collect { case Some(x) => x }
                    .toNel
                    .flatTraverse { activeChanges =>
                      val s = activeChanges.toList.map { case (k, _, _, _) => k }.toSet

                      val allSigNodes = activeStreams.toList.map { case (k, sm) =>
                        // The cursor is to the parent, traversing the arc for df.name will get us to the changed node
                        (sm.cursor, k)
                      }

                      val meta = recompute(allSigNodes, s)

                      // The root nodes are the highest common signal ancestors
                      val rootNodes = activeChanges.filter { case (k, _, _, _) => meta.hcsa.contains(k) }

                      val preparedRoots =
                        rootNodes.map { case (st, rt, in, sm) =>
                          in match {
                            case Left(ex) =>
                              (Chain(EvalFailure.StreamTailResolution( /*sm.cursor*/ null, Left(ex))), None, sm.cursor)
                            case Right(nec) =>
                              (
                                Chain.fromOption(nec.left).flatMap(_.toChain).map { msg =>
                                  EvalFailure.StreamTailResolution( /*sm.cursor*/ null, Right(msg))
                                },
                                nec.right.map(RunInput(sm.edges, sm.cont, _)),
                                sm.cursor
                              )
                          }
                        }

                      preparedRoots.toNel
                        .traverse { xs =>
                          val paddedErrors = xs.toList.mapFilter {
                            case (_, None, c)    => Some((Json.Null, c))
                            case (_, Some(_), _) => None
                          }

                          val defined = xs.collect { case (_, Some(x), c) => (x, c) }

                          val evalled =
                            defined
                              .collect { case (x, _) => x }
                              .toNel
                              .traverse(evaluate)
                              .flatMap[(List[(Json, Cursor)], Chain[EvalFailure], Map[Unique.Token, StreamMetadata[F]])] {
                                case None => F.pure((Nil, Chain.empty, activeStreams))
                                case Some((newFails, newSuccs, newStreams)) =>
                                  val succWithInfo = newSuccs.toList zip defined.map { case (_, c) => c }

                                  val withNew = newStreams ++ activeStreams
                                  // All nodes that occured in the tree but are not in the HCSA are dead
                                  val garbageCollected = withNew -- meta.toRemove

                                  // Also remove the subscriptions and dead resources
                                  val releasesF = streamSup.release(meta.toRemove.toSet)

                                  val freeF = activeChanges
                                    .collect { case (k, rt, _, _) if meta.hcsa.contains(k) => (k, rt) }
                                    .traverse_ { case (k, rt) => streamSup.freeUnused(k, rt) }

                                  releasesF >> freeF as (succWithInfo, newFails, garbageCollected)
                              }

                          evalled.map { case (jsons, errs, finalStreams) =>
                            val allJsons = jsons ++ paddedErrors
                            val allErrs = errs ++ Chain.fromSeq(xs.toList).flatMap { case (es, _, _) => es }

                            val stitched = allJsons.foldLeft(prevOutput) { case (accum, (patch, pos)) =>
                              stitchInto(accum.asJson, patch, pos).asObject.get
                            }

                            ((stitched, finalStreams), Some((allErrs, stitched)))
                          }
                        }
                    }
                    .map(_.getOrElse(((initialOutput, activeStreams), None)))
                }
                .map { case (_, x) => x }
                .unNone
          }
        }

//       // first iteration
//       fs2.Stream
//         .eval(runStreamIt(rootSel.map((_, Chain(EvalNode.empty(rootInput, BigInt(1)))))))
//         .flatMap { case (initialFails, initialSuccs, deps, initialSM) =>
//           val c = combineSplit(initialFails, initialSuccs).toList.map { case (m, v) => m.absolutePath -> v }

//           fs2.Stream(reconstructSelection(c, rootSel)).flatMap { initialOutput =>
//             fs2.Stream.emit((initialFails, initialOutput)) ++
//               changeStream
//                 .evalMapAccumulate((initialOutput, initialSM)) { case ((prevOutput, activeStreams), changes) =>
//                   changes
//                     .map { case (k, rt, v) => activeStreams.get(k).map((k, rt, v, _)) }
//                     .collect { case Some(x) => x }
//                     .toNel
//                     .flatTraverse { activeChanges =>
//                       val s = activeChanges.toList.map { case (k, _, _, _) => k }.toSet
//                       val allSigNodes = activeStreams.toList.map { case (k, StreamMetadata(cursor, _, df, _, _)) =>
//                         // The cursor is to the parent, traversing the arc for df.name will get us to the changed node
//                         (cursor.field(df.id, df.name), k)
//                       }
//                       val meta = recompute(allSigNodes, s)

//                       // The root nodes are the highest common signal ancestors
//                       val rootNodes = activeChanges.filter { case (k, _, _, _) => meta.hcsa.contains(k) }

//                       // Partition every HCSA node into it's own cursor group (for efficient re-association later)
//                       // We cannot use the field name to group them, since two changed fields can have the same name
//                       // The property of field uniqueness is lost since we are starting at n different non-root nodes
//                       val prepaedRoots =
//                         rootNodes.mapWithIndex { case ((id, _, input, _), idx) =>
//                           val StreamMetadata(cursor, _, field, _, _) = activeStreams(id)
//                           val cursorGroup = BigInt(idx)
//                           val cg = CursorGroup.startAt(cursorGroup, cursor)
//                           val (errs, succs) =
//                             input match {
//                               case Left(ex) =>
//                                 (Chain(EvalFailure.StreamTailResolution(cg, Left(ex))), Chain.empty)
//                               case Right(nec) =>
//                                 (
//                                   Chain.fromOption(nec.left).flatMap(_.toChain).map { msg =>
//                                     EvalFailure.StreamTailResolution(cg, Right(msg))
//                                   },
//                                   Chain.fromOption(nec.right).map(in => EvalNode(cg, in))
//                                 )
//                             }
//                           (field, cursor, succs, errs, cursorGroup)
//                         }

//                       prepaedRoots.toNel
//                         .traverse { newRootSel =>
//                           runStreamIt(newRootSel.map { case (df, _, inputs, _, _) => (df, inputs) })
//                             .flatMap { case (newFails, newSuccs, _, newStreams) =>
//                               val groupIdMapping =
//                                 newRootSel.toList.map { case (df, rootCursor, _, _, idx) => idx -> (rootCursor, df) }.toMap

//                               val allFails = newFails ++ Chain.fromSeq(newRootSel.toList).flatMap { case (_, _, _, errs, _) => errs }

//                               val all = combineSplit(allFails, newSuccs)
//                               // group -> results
//                               val l = all.toList.groupBy { case (m, _) => m.groupId }.toList

//                               val prepped =
//                                 l.map { case (group, results) =>
//                                   val (rootCursor, df) = groupIdMapping(group.toInt)
//                                   // Before re-constructing, drop the head of the cursor
//                                   // Since that edge will be where the result should be stitched into
//                                   val tailResults = results.map { case (m, x) => m.relativePath.tail -> x }
//                                   val rec = reconstructField(df.selection, tailResults)

//                                   // Left is the object, right is the position to replace
//                                   (rec, rootCursor.field(df.id, df.name))
//                                 }

//                               // fold every group's result together
//                               // basecase is previous output
//                               val recombined =
//                                 prepped
//                                   .foldLeft(prevOutput) { case (accum, (patch, pos)) =>
//                                     stitchInto(accum.asJson, patch, pos).asObject.get
//                                   }

//                               val withNew = newStreams ++ activeStreams
//                               // All nodes that occured in the tree but are not in the HCSA are dead
//                               val garbageCollected = withNew -- meta.toRemove

//                               val o = ((recombined, garbageCollected), Some((allFails, recombined)))

//                               // Also remove the subscriptions and dead resources
//                               val releasesF =
//                                 streamSup
//                                   .release(meta.toRemove.toSet)

//                               val freeF = activeChanges
//                                 .collect { case (k, rt, _, _) if meta.hcsa.contains(k) => (k, rt) }
//                                 .traverse_ { case (k, rt) => streamSup.freeUnused(k, rt) }

//                               releasesF >> freeF as o
//                             }
//                         }
//                     }
//                     .map(_.getOrElse(((initialOutput, activeStreams), None)))
//                 }
//                 .map { case (_, x) => x }
//                 .unNone
//           }
//         }
    }

  def runStreamed[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    constructStream[F](rootInput, rootSel, schemaState, true)

  def runSync[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  ): F[(Chain[EvalFailure], JsonObject)] =
    constructStream[F](rootInput, rootSel, schemaState, false).take(1).compile.lastOrError

  def findToRemove[A](nodes: List[(Cursor, A)], s: Set[A]): Set[A] = {
    import Chain._
    val (children, nodeHere) = nodes
      .partitionEither {
        case (xs, y) if xs.path.isEmpty => Right(y)
        case (xs, y)                    => Left((xs, y))
      }

    lazy val msg =
      s"something went terribly wrong s=$s, nodeHere:${nodeHere}\niterates:\n${children.mkString("\n")}\nall nodes:\n${nodes.mkString("\n")}"

    nodeHere match {
      case x :: Nil if s.contains(x) => children.map { case (_, v) => v }.toSet
      case _ :: Nil | Nil            => groupNodeValues(children).flatMap { case (_, v) => findToRemove(v, s) }.toSet
      case _                         => throw new Exception(msg)
    }
  }

  final case class StreamRecompute[A](
      toRemove: Set[A],
      hcsa: Set[A]
  )

  def recompute[A](nodes: List[(Cursor, A)], s: Set[A]): StreamRecompute[A] = {
    val tr = findToRemove(nodes, s)
    val hcsa = s -- tr
    StreamRecompute(tr, hcsa)
  }

  def groupNodeValues[A](nvs: List[(Cursor, A)]): Map[GraphArc, List[(Cursor, A)]] =
    nvs.groupMap { case (c, _) => c.head } { case (c, v) => (c.tail, v) }

  def groupNodeValues2[A](nvs: List[(Cursor, A)]): Map[Option[GraphArc], List[(Cursor, A)]] =
    nvs.groupMap { case (c, _) => c.headOption } { case (c, v) => (c.tail, v) }

  final case class ExecutionDeps[F[_]](
      batching: Batching[F],
      batchExeutionState: Map[Int, Ref[F, BatchExecutionState]],
      schemaState: SchemaState[F]
  )

  def interpret[F[_]](
      rootSel: NonEmptyList[(PreparedField[F, Any], Chain[EvalNode[Any]])],
      executionDeps: Batching[F],
      schemaState: SchemaState[F],
      streamAccumulator: StreamMetadataAccumulator[F, StreamMetadata[F], IorNec[String, Any]],
      batchAccumulator: BatchAccumulator[F] = null
  )(implicit F: Async[F], stats: Statistics[F]): WriterT[F, Chain[EvalFailure], Chain[EvalNode[Json]]] = {
    type W[A] = WriterT[F, Chain[EvalFailure], A]
    val W = Async[W]
    type E = Chain[EvalNode[Any]]
    val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]

    def failM[A](e: EvalFailure)(implicit M: Monoid[A]): W[A] = WriterT.put(M.empty)(Chain.one(e))

    def attemptUser[A](fa: F[IorNec[String, A]], constructor: Either[Throwable, String] => EvalFailure)(implicit
        M: Monoid[A]
    ): W[A] =
      lift(fa).attempt.flatMap {
        case Left(ex)              => WriterT.put(M.empty)(Chain(constructor(Left(ex))))
        case Right(Ior.Both(l, r)) => WriterT.put(r)(l.toChain.map(x => constructor(Right(x))))
        case Right(Ior.Left(l))    => WriterT.put(M.empty)(l.toChain.map(x => constructor(Right(x))))
        case Right(Ior.Right(r))   => WriterT.put(r)(Chain.empty)
      }

    def attemptUserE(fa: F[IorNec[String, E]], constructor: Either[Throwable, String] => EvalFailure): W[E] =
      attemptUser[E](fa, constructor)

    Supervisor[F].mapK(lift).use { sup =>
      executionDeps.batchExecutionState[W].flatMap { batchStateMap =>
        sealed trait StateSubmissionOutcome
        final case class FinalSubmission(accumulatedInputs: Map[Int, Chain[EvalNode[Any]]]) extends StateSubmissionOutcome
        case object NoState extends StateSubmissionOutcome
        case object NotFinalSubmission extends StateSubmissionOutcome

        def submitAndMaybeStart(nodeId: Int, input: Chain[EvalNode[Any]]): W[StateSubmissionOutcome] =
          batchStateMap
            .get(nodeId)
            .traverse(_.modify { s =>
              val newSet = s.remainingInputs - nodeId
              val newMap = s.inputMap + (nodeId -> input)
              val newState = BatchExecutionState(newSet, newMap)
              (newState, if (newSet.isEmpty && s.remainingInputs.nonEmpty) FinalSubmission(newMap) else NotFinalSubmission)
            })
            .map(_.getOrElse(NoState))

        def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
          sup.supervise(stats.updateStats(name, duration, size)).void

        type Reassociate = Map[BatchKey, BatchValue] => F[IorNec[String, BatchResult]]
        final case class Batch(
            batcher: Int,
            keys: Chain[(CursorGroup, (Set[BatchKey], Reassociate))]
        )

        def runEdge(
            inputs: Chain[EvalNode[Any]],
            edges: List[PreparedEdge[F]],
            cont: Prepared[F, Any]
        ): W[Chain[EvalNode[Json]]] =
          edges match {
            case Nil => startNext(cont, inputs)
            case edge :: xs =>
              val edgeRes: WriterT[F, Chain[EvalFailure], Chain[EvalNode[Any]]] =
                edge.resolver match {
                  case EffectResolver(resolve) =>
                    inputs.parFlatTraverse { in =>
                      attemptUser(
                        IorT(resolve(in.value)).timed
                          .semiflatMap { case (dur, v) =>
                            val out = Chain(in.setValue(v))
                            submit(edge.statisticsName, dur, 1) as out
                          }
                          .value
                          .map(_.leftMap(NonEmptyChain.one)),
                        EvalFailure.EffectResolution(in.cursorGroup, _, in.value)
                      )
                    }
                  case CompositionResolver(_, _) =>
                    W.raiseError(new IllegalStateException("composition resolvers cannot appear in edge interpreter"))
                  case StreamResolver(_, stream) =>
                    inputs.parFlatTraverse { in =>
                      attemptUserE(
                        streamAccumulator
                          .add(
                            StreamMetadata(in.cursorGroup.absolutePath, null, null, xs, cont),
                            stream(in.value)
                          )
                          .map { case (_, fb) => fb }
                          .rethrow
                          .map(_.map(hd => Chain(in.setValue((in.value, hd))))),
                        EvalFailure.StreamHeadResolution(in.cursorGroup, _, in.value)
                      )
                    }
                  case BatchResolver(_, run) =>
                    val resolveds =
                      inputs.parFlatTraverse { en =>
                        attemptUser(
                          run(en.value).map(_.map(res => Chain((res, en.cursorGroup)))),
                          EvalFailure.BatchPartitioning(en.cursorGroup, _, en.value)
                        )
                      }

                    resolveds.flatMap { xs =>
                      val allKeys = xs.map { case ((keys, _), cg) => (cg, keys) }

                      lift(batchAccumulator.submit(edge.id, allKeys))
                        .flatMap {
                          case None => W.pure(Chain.empty[EvalNode[Any]])
                          case Some(resultMap) =>
                            xs.parFlatTraverse { case ((keys, reassoc), cg) =>
                              val missingKeys =
                                keys.toList.collect { case k if !resultMap.contains(k) => k }.toSet
                              if (missingKeys.nonEmpty) {
                                failM[E](EvalFailure.BatchMissingKey(cg, resultMap, keys, missingKeys))
                              } else {
                                attemptUserE(
                                  reassoc(resultMap).map(_.map(res => Chain(EvalNode(cg, res)))),
                                  EvalFailure.BatchPostProcessing(cg, _, resultMap)
                                )
                              }
                            }
                        }
                    }
                }

              edgeRes.flatMap(runEdge(_, xs, cont))
          }

        def runInputs(
            df: PreparedDataField[F, Any, Any],
            inputs: Chain[EvalNode[Any]]
        ): W[Either[Chain[EvalNode[Any]], Batch]] = {
          val n = executionDeps.nodeMap(df.id)

          df.resolve match {
            case EffectResolver(resolve) =>
              inputs
                .parFlatTraverse { in =>
                  val next = in.cursorGroup.field(df.id, df.name)

                  attemptUser(
                    IorT(resolve(in.value)).timed
                      .semiflatMap { case (dur, v) =>
                        val out = Chain(EvalNode(next, v))
                        submit(n.name, dur, 1) as out
                      }
                      .value
                      .map(_.leftMap(NonEmptyChain.one)),
                    EvalFailure.EffectResolution(next, _, in.value)
                  )
                }
                .map(Left(_))
            case BatchResolver(id, run) =>
              val partitioned =
                inputs.parFlatTraverse { in =>
                  val next = in.cursorGroup.field(df.id, df.name)

                  attemptUser(
                    run(in.value).map(_.map(b => Chain((next, b)))),
                    EvalFailure.BatchPartitioning(next, _, in.value)
                  )
                }

              partitioned.map(p => Right(Batch(id.id, p)))
            case StreamResolver(resolver, stream) =>
              val fb: W[E] =
                inputs.parFlatTraverse { in =>
                  val df2 = df.copy(resolve = resolver.contramap[Any]((in.value, _)))

                  attemptUserE(
                    streamAccumulator
                      .add(
                        // TODO add stream as a path to distinguish between different streams
                        StreamMetadata(in.cursorGroup.absolutePath, in.value, df2),
                        stream(in.value)
                      )
                      .map { case (_, fb) => fb }
                      .rethrow
                      .map(_.map(hd => Chain(in.setValue((in.value, hd))))),
                    EvalFailure.StreamHeadResolution(in.cursorGroup, _, in.value)
                  )
                }

              fb.flatMap(runInputs(df.copy(resolve = resolver.contramap[Any](_.asInstanceOf[(Any, Any)])), _))
          }
        }

        def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] =
          Chain.fromSeq(dfs.toList).parFlatTraverse {
            case PreparedFragField(id, typename, specify, selection) =>
              runFields(
                selection.fields,
                in.flatMap(x => Chain.fromOption(specify(x.value)).map(y => x.succeed(y, _.fragment(id, typename))))
              )
            case df @ PreparedDataField(id, name, _, _, _, _, _, _) => runDataField(df, in)
          }

        def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = W.defer {
          s match {
            case PreparedLeaf(name, enc) => W.pure(in.map(en => en.setValue(enc(en.value))))
            case Selection(fields)       => runFields(fields, in)
            case PreparedList(of) =>
              val (emties, continuations) =
                in.partitionEither { nv =>
                  val inner = Chain.fromSeq(nv.value.asInstanceOf[Seq[Any]])

                  NonEmptyChain.fromChain(inner) match {
                    case None      => Left(nv.setValue(Json.arr()))
                    case Some(nec) => Right(nec.mapWithIndex { case (v, i) => nv.succeed(v, _.index(i)) })
                  }
                }

              startNext(of, continuations.flatMap(_.toChain)).map(_ ++ emties)
            case PreparedOption(of) =>
              val (nulls, continuations) =
                in.partitionEither { nv =>
                  val inner = nv.value.asInstanceOf[Option[Any]]

                  inner match {
                    case None    => Left(nv.setValue(Json.Null))
                    case Some(v) => Right(nv.setValue(v))
                  }
                }
              startNext(of, continuations).map(_ ++ nulls)
          }
        }

        /*
         * 1. Partitions the inputs into batch and trivial (non-batch)
         *
         * 2. Then runs the batch efficiently and the trivial in parallel
         *
         * 3. Then reconstructs every batch field's output from
         *    the batched results and calls the batch fields continuations in parallel
         *
         * 4. Finally, combines the results in parallel.
         */
        def runDataInputs(datas: Chain[(PreparedDataField[F, Any, Any], Chain[EvalNode[Any]])]): W[Chain[EvalNode[Json]]] = {
          val inputsResolved =
            datas.parTraverse { case (df, v) => runInputs(df, v).map(df -> _) }

          val partitioned =
            inputsResolved.map(_.partitionEither { case (df, e) => e.bimap((df, _), (df, _)) })

          partitioned
            .flatMap { case (trivial, batched) =>
              val trivialF = trivial.parFlatTraverse { case (df, fas) => startNext(df.selection, fas) }

              val batchedF =
                NonEmptyChain
                  .fromChain(batched)
                  .traverse { nec =>
                    val (_, b) = nec.head

                    val allKeys: Set[BatchKey] = nec.toChain
                      .flatMap { case (_, bn) => bn.keys.flatMap { case (_, (ks, _)) => Chain.fromSeq(ks.toSeq) } }
                      .toIterable
                      .toSet

                    val resolvedF: F[WriterT[F, Chain[EvalFailure], Chain[EvalNode[Json]]]] =
                      schemaState
                        .batchers(BatchResolver.ResolverKey(b.batcher))(allKeys)
                        .timed
                        .flatTap { case (dur, m) => submit(Planner.makeBatchName[F](b.batcher), dur, allKeys.size) }
                        .map { case (dur, resultLookup: Map[BatchKey, BatchValue]) =>
                          nec.toChain.parFlatTraverse { case (df, bn) =>
                            val mappedValuesF: W[Chain[EvalNode[Any]]] =
                              bn.keys.flatTraverse { case (cg, (ks, reassoc)) =>
                                val (missing, found) =
                                  ks.toList.partitionMap[BatchKey, (BatchKey, BatchValue)](k =>
                                    resultLookup.get(k) match {
                                      case None    => Left(k)
                                      case Some(x) => Right((k -> x))
                                    }
                                  )

                                missing.toNel match {
                                  case Some(nel) =>
                                    failM[Chain[EvalNode[Any]]](EvalFailure.BatchMissingKey(cg, resultLookup, ks, nel.toList.toSet))
                                  case None =>
                                    lift(submit(executionDeps.nodeMap(df.id).name, dur, allKeys.size)) >>
                                      attemptUserE(
                                        reassoc(found.toMap).map(_.map(res => Chain(EvalNode(cg, res)))),
                                        EvalFailure.BatchPostProcessing(cg, _, found.toMap)
                                      )
                                }
                              }

                            mappedValuesF.flatMap(startNext(df.selection, _))
                          }
                        }

                    lift(resolvedF).attempt
                      .flatMap {
                        case Left(ex) =>
                          val posses =
                            nec.toChain.flatMap { case (_, bn) => bn.keys.map { case (cg, (_, _)) => cg } }
                          WriterT.put(Chain.empty[EvalNode[Json]])(
                            Chain(EvalFailure.BatchResolution(posses, ex, allKeys))
                          )
                        case Right(fa) => fa
                      }
                  }
                  .map(Chain.fromOption)
                  .map(_.flatten)

              // fork each node's continuation
              // in parallel, start every node's computation
              (trivialF, batchedF).parMapN(_ ++ _)
            }
        }

        def runDataField(df: PreparedDataField[F, Any, Any], input: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = W.defer {
          // maybe join
          submitAndMaybeStart(df.id, input).flatMap {
            // No batching state, so just start
            case NoState => runDataInputs(Chain(df -> input))
            // join
            // There is a batch state, but we didn't add the final input
            // Stop here, someone else will start the batch
            case NotFinalSubmission => W.pure(Chain.empty)
            // join
            // We are the final submitter, start the computation
            case FinalSubmission(inputs) =>
              val in = Chain.fromSeq(inputs.toList)

              val inputLst = in

              val withDfs = inputLst.map { case (k, v) => executionDeps.dataFieldMap(k) -> v }

              runDataInputs(withDfs)
          }
        }

        Chain.fromSeq(rootSel.toList).parFlatTraverse { case (field, inputs) =>
          runFields(NonEmptyList.one(field), inputs)
        }
      }
    }
  }

  def reconstructField[F[_]](p: Prepared[F, Any], cursors: List[(Cursor, Json)]): Json = {
    val m = groupNodeValues2(cursors)

    m.get(None) match {
      case Some(t) if m.size == 1 && t.forall { case (_, o) => o.isNull } => Json.Null
      case _ =>
        p match {
          case PreparedLeaf(name, _) =>
            cursors.collectFirst { case (_, x) if !x.isNull => x }.get
          case PreparedList(of) =>
            m.toVector
              .collect { case (Some(GraphArc.Index(i)), tl) => i -> tl }
              .map { case (i, tl) => i -> reconstructField[F](of, tl) }
              .sortBy { case (i, _) => i }
              .map { case (_, v) => v }
              .asJson
          case PreparedOption(of) =>
            reconstructField[F](of, cursors)
          case Selection(fields) =>
            _reconstructSelection(fields, m).asJson
        }
    }
  }

  def _reconstructSelection[F[_]](
      sel: NonEmptyList[PreparedField[F, Any]],
      m: Map[Option[GraphArc], List[(Cursor, Json)]]
  ): JsonObject = {
    sel
      .map { pf =>
        pf match {
          case PreparedFragField(id, typename, _, selection) =>
            m.get(Some(GraphArc.Fragment(id, typename)))
              .map { lc =>
                val m2 = groupNodeValues2(lc)
                _reconstructSelection(selection.fields, m2)
              }
              .getOrElse(JsonObject.empty)
          case df @ PreparedDataField(id, name, _, selection, _, alias, _, _) =>
            JsonObject(
              alias.getOrElse(name) -> reconstructField(selection, m.get(Some(GraphArc.Field(id, name))).toList.flatten)
            )
        }
      }
      .reduceLeft(_ deepMerge _)
  }

  def reconstructSelection[F[_]](
      levelCursors: List[(Cursor, Json)],
      sel: NonEmptyList[PreparedField[F, Any]]
  ): JsonObject =
    _reconstructSelection(sel, groupNodeValues2(levelCursors))
}

class InterpreterImpl[F[_]](
    streamAccumulator: StreamMetadataAccumulator[F, Interpreter.StreamMetadata[F], IorNec[String, Any]],
    batchAccumulator: BatchAccumulator[F],
    sup: Supervisor[F]
)(implicit F: Async[F], stats: Statistics[F])
    extends Interpreter[F] {
  val W = Async[W]
  type E = Chain[EvalNode[Any]]
  val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]

  def failM[A](e: EvalFailure)(implicit M: Monoid[A]): W[A] = WriterT.put(M.empty)(Chain.one(e))

  def attemptUser[A](fa: F[IorNec[String, A]], constructor: Either[Throwable, String] => EvalFailure)(implicit
      M: Monoid[A]
  ): W[A] =
    lift(fa).attempt.flatMap {
      case Left(ex)              => WriterT.put(M.empty)(Chain(constructor(Left(ex))))
      case Right(Ior.Both(l, r)) => WriterT.put(r)(l.toChain.map(x => constructor(Right(x))))
      case Right(Ior.Left(l))    => WriterT.put(M.empty)(l.toChain.map(x => constructor(Right(x))))
      case Right(Ior.Right(r))   => WriterT.put(r)(Chain.empty)
    }

  def attemptUserE(fa: F[IorNec[String, E]], constructor: Either[Throwable, String] => EvalFailure): W[E] =
    attemptUser[E](fa, constructor)

  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def runEdge(
      inputs: Chain[EvalNode[Any]],
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  ): W[Chain[EvalNode[Json]]] =
    edges match {
      case Nil => startNext(cont, inputs)
      case edge :: xs =>
        val edgeRes: WriterT[F, Chain[EvalFailure], Chain[EvalNode[Any]]] =
          edge.resolver match {
            case EffectResolver(resolve) =>
              inputs.parFlatTraverse { in =>
                attemptUser(
                  IorT(resolve(in.value)).timed
                    .semiflatMap { case (dur, v) =>
                      val out = Chain(in.setValue(v))
                      submit(edge.statisticsName, dur, 1) as out
                    }
                    .value
                    .map(_.leftMap(NonEmptyChain.one)),
                  EvalFailure.EffectResolution(in.cursorGroup, _, in.value)
                )
              }
            case CompositionResolver(_, _) =>
              W.raiseError(new IllegalStateException("composition resolvers cannot appear in edge interpreter"))
            case StreamResolver(_, stream) =>
              inputs.parFlatTraverse { in =>
                attemptUserE(
                  streamAccumulator
                    .add(
                      Interpreter.StreamMetadata(in.cursorGroup.absolutePath, null, null, xs, cont),
                      stream(in.value)
                    )
                    .timed
                    .map { case (dur, (_, fb)) => fb.tupleLeft(dur) }
                    .rethrow
                    .flatMap { case (dur, xs) =>
                      submit(edge.statisticsName, dur, 1) as
                        xs.map(hd => Chain(in.setValue((in.value, hd))))
                    },
                  EvalFailure.StreamHeadResolution(in.cursorGroup, _, in.value)
                )
              }
            case BatchResolver(_, run) =>
              println(edge.statisticsName)
              val resolveds =
                inputs.parFlatTraverse { en =>
                  attemptUser(
                    run(en.value).map(_.map(res => Chain((res, en.cursorGroup)))),
                    EvalFailure.BatchPartitioning(en.cursorGroup, _, en.value)
                  )
                }

              resolveds.flatMap { xs =>
                val allKeys = xs.map { case ((keys, _), cg) => (cg, keys) }

                lift(batchAccumulator.submit(edge.id, allKeys))
                  .flatMap {
                    case None => W.pure(Chain.empty[EvalNode[Any]])
                    case Some(resultMap) =>
                      xs.parFlatTraverse { case ((keys, reassoc), cg) =>
                        val missingKeys =
                          keys.toList.collect { case k if !resultMap.contains(k) => k }.toSet
                        if (missingKeys.nonEmpty) {
                          failM[E](EvalFailure.BatchMissingKey(cg, resultMap, keys, missingKeys))
                        } else {
                          attemptUserE(
                            reassoc(resultMap).map(_.map(res => Chain(EvalNode(cg, res)))),
                            EvalFailure.BatchPostProcessing(cg, _, resultMap)
                          )
                        }
                      }
                  }
              }
          }

        edgeRes.flatMap(runEdge(_, xs, cont))
    }

  def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] =
    Chain.fromSeq(dfs.toList).parFlatTraverse {
      case PreparedFragField(id, typename, specify, selection) =>
        runFields(
          selection.fields,
          in.flatMap(x => Chain.fromOption(specify(x.value)).map(y => x.succeed(y, _.fragment(id, typename))))
        )
      case df @ PreparedDataField(id, _, _, _, _, _, _, _) => runDataField(df, in)
    }

  def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = W.defer {
    s match {
      case PreparedLeaf(name, enc) => W.pure(in.map(en => en.setValue(enc(en.value))))
      case Selection(fields)       => runFields(fields, in)
      case PreparedList(of) =>
        val (emties, continuations) =
          in.partitionEither { nv =>
            val inner = Chain.fromSeq(nv.value.asInstanceOf[Seq[Any]])

            NonEmptyChain.fromChain(inner) match {
              case None      => Left(nv.setValue(Json.arr()))
              case Some(nec) => Right(nec.mapWithIndex { case (v, i) => nv.succeed(v, _.index(i)) })
            }
          }

        startNext(of, continuations.flatMap(_.toChain)).map(_ ++ emties)
      case PreparedOption(of) =>
        val (nulls, continuations) =
          in.partitionEither { nv =>
            val inner = nv.value.asInstanceOf[Option[Any]]

            inner match {
              case None    => Left(nv.setValue(Json.Null))
              case Some(v) => Right(nv.setValue(v))
            }
          }
        startNext(of, continuations).map(_ ++ nulls)
    }
  }

  def runDataField(df: PreparedDataField[F, Any, Any], input: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] =
    runEdge(input.map(_.modify(_.field(df.id, df.name))), df.cont.edges.toList, df.cont.cont)
}
