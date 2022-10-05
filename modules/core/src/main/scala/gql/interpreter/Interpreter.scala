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

  def combineSplit(fails: Chain[EvalFailure], succs: Chain[EvalNode[Json]]): Chain[(Cursor, Json)] =
    fails.flatMap(_.paths).map(m => (m, Json.Null)) ++ succs.map(n => (n.cursor, n.value))

  final case class StreamMetadata[F[_]](
      cursor: Cursor,
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
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
            planned = Planner.plan(costTree)
            accumulator <- BatchAccumulator[F](schemaState, planned)
            res <- Supervisor[F].use { sup =>
              val interpreter = new InterpreterImpl[F](sma, accumulator, sup)
              metas.parTraverse { ri =>
                interpreter
                  .runEdge(Chain(EvalNode.empty(ri.inputValue)), ri.edges, ri.cont)
                  .run
                  .map { case (fail, succs) =>
                    val comb = combineSplit(fail, succs)
                    val j = reconstructField[F](ri.cont, comb.toList)
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
                  EvalFailure.EffectResolution(in.cursor, _, in.value)
                )
              }
            case CompositionResolver(_, _) =>
              W.raiseError(new IllegalStateException("composition resolvers cannot appear in edge interpreter"))
            case StreamResolver(stream) =>
              inputs.parFlatTraverse { in =>
                attemptUserE(
                  streamAccumulator
                    .add(
                      Interpreter.StreamMetadata(in.cursor, xs, cont),
                      stream(in.value)
                    )
                    .timed
                    .map { case (dur, (_, fb)) => fb.tupleLeft(dur) }
                    .rethrow
                    .flatMap { case (dur, xs) =>
                      submit(edge.statisticsName, dur, 1) as
                        xs.map(hd => Chain(in.setValue(hd)))
                    },
                  EvalFailure.StreamHeadResolution(in.cursor, _, in.value)
                )
              }
            case BatchResolver(_, run) =>
              val resolveds =
                inputs.parFlatTraverse { en =>
                  attemptUser(
                    run(en.value).map(_.map(res => Chain((res, en.cursor)))),
                    EvalFailure.BatchPartitioning(en.cursor, _, en.value)
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
