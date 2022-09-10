package gql.interpreter

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
import gql.out._

object Interpreter {
  def run[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      plan: NonEmptyList[Planner.Node],
      schemaState: SchemaState[F]
  ): F[JsonObject] =
    interpret[F](rootSel.map((_, Chain(EvalNode.empty(rootInput, BigInt(0))))), Batching.plan[F](rootSel, plan), schemaState).run
      .map { case (f, s) => combineSplit(f, s) }
      .map(xs => reconstructSelection(xs.map { case (nm, x) => nm.relativePath -> x }.toList, rootSel))

  def stitchInto(
      oldTree: Json,
      subTree: Json,
      path: Cursor
  ): Json =
    path.uncons match {
      case None => subTree
      case Some((p, tl)) =>
        p match {
          case GraphArc.Field(name) =>
            val oldObj = oldTree.asObject.get
            val oldValue = oldObj(name).get
            val newSubTree = stitchInto(oldValue, subTree, tl)
            // println(oldValue)
            // println(s"adding $name>>>>>>>>>>")
            // println(newSubTree)
            oldObj.add(name, newSubTree).asJson
          case GraphArc.Index(index) =>
            val oldArr = oldTree.asArray.get
            oldArr.updated(index, stitchInto(oldArr(index), subTree, tl)).asJson
        }
    }

  def combineSplit(fails: Chain[EvalFailure], succs: Chain[EvalNode[Json]]): Chain[(CursorGroup, Option[Json])] =
    fails.flatMap(_.paths).map(m => (m, None)) ++ succs.map(n => (n.cursorGroup, Some(n.value)))

  def constructStream[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F],
      streamResourceAlg: Option[SubscriptionSupervisor[F]]
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] = {
    val changeLog = fs2.Stream.fromOption(streamResourceAlg).flatMap(_.changeLog)

    val accumulatorF = streamResourceAlg.traverse(implicit alg => SignalMetadataAccumulator[F])

    def runStreamIt(
        root: NonEmptyList[(PreparedField[F, Any], Chain[EvalNode[Any]])]
    ): F[(Chain[EvalFailure], Chain[EvalNode[Json]], Batching[F], Map[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])])] =
      accumulatorF.flatMap { accumulatorOpt =>
        val rootFields = root.map { case (k, _) => k }
        for {
          costTree <- Planner.costTree[F](rootFields)
          plan = Planner.plan(costTree)
          executionDeps = Batching.plan[F](rootFields, plan)
          (fails, succs) <- interpret[F](root, executionDeps, schemaState, accumulatorOpt).run
          s <- accumulatorOpt match {
            case Some(x) => x.getState
            case None    => F.pure(Map.empty[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])])
          }
        } yield (fails, succs, executionDeps, s)
      }

    // first iteration
    fs2.Stream
      .eval(runStreamIt(rootSel.map((_, Chain(EvalNode.empty(rootInput, BigInt(1)))))))
      .flatMap { case (initialFails, initialSuccs, deps, initialSignals) =>
        val c = combineSplit(initialFails, initialSuccs).toList.map { case (m, v) => m.absolutePath -> v }

        fs2.Stream(reconstructSelection(c, rootSel)).flatMap { initialOutput =>
          fs2.Stream.emit((initialFails, initialOutput)) ++
            changeLog
              .evalMapAccumulate((initialOutput, initialSignals)) { case ((prevOutput, activeSigs), changes) =>
                // remove dead nodes (concurrent access can cause dead updates to linger)
                changes
                  .filter { case (k, _) => activeSigs.contains(k) }
                  .toNel
                  .flatTraverse { activeChanges =>
                    // The hcsa algorithm can most likely be optimized
                    // It takes atleast n time currently
                    val s = activeChanges.toList.map { case (k, _) => k }.toSet
                    val allSigNodes = activeSigs.toList.map { case (k, (cursor, _, df)) =>
                      // The cursor is to the parent, traversing the arc for df.name will get us to the changed node
                      (cursor.field(df.name), k)
                    }
                    val meta = computeMetadata(allSigNodes, s)

                    // The root nodes are the highest common signal ancestors
                    val rootNodes: List[(BigInt, Any)] = activeChanges.filter { case (k, _) => meta.hcsa.contains(k) }

                    // Partition every HCSA node into it's own cursor group (for efficient re-association later)
                    // We cannot use the field name to group them, since two changed fields can have the same name
                    // The property of field uniqueness is lost since we are starting at n different non-root nodes
                    val prepaedRoots =
                      rootNodes.mapWithIndex { case ((id, input), idx) =>
                        val (cursor, _, field) = activeSigs(id)
                        val cursorGroup = BigInt(idx)
                        (field, cursor, Chain(EvalNode.startAt(input, cursorGroup, cursor)), cursorGroup)
                      }

                    prepaedRoots.toNel
                      .traverse { newRootSel =>
                        runStreamIt(newRootSel.map { case (df, _, inputs, _) => (df, inputs) })
                          .flatMap { case (newFails, newSuccs, _, newSigs) =>
                            // Mapping from group to start cursor and data field
                            val groupIdMapping =
                              newRootSel.toList.map { case (df, rootCursor, _, idx) => idx -> (rootCursor, df) }.toMap

                            val all = combineSplit(newFails, newSuccs)
                            // group -> results
                            val l: List[(BigInt, List[(CursorGroup, Option[Json])])] =
                              all.toList.groupBy { case (m, _) => m.id }.toList

                            val prepped =
                              l.map { case (group, results) =>
                                val (rootCursor, df) = groupIdMapping(group.toInt)
                                // Before re-constructing, drop the head of the cursor
                                // Since that edge will be where the result should be stitched into
                                val tailResults = results.map { case (m, x) => m.relativePath.tail -> x }
                                val rec = reconstructField(df.selection, tailResults)

                                // Left is the object, right is the position to replace
                                (rec, rootCursor.field(df.name))
                              }

                            // fold every group's result together
                            // basecase is previous output
                            val recombined =
                              prepped
                                .foldLeft(prevOutput) { case (accum, (patch, pos)) =>
                                  stitchInto(accum.asJson, patch, pos).asObject.get
                                }

                            val withNew = newSigs ++ activeSigs
                            // All nodes that occured in the tree but are not in the HCSA are dead
                            val garbageCollected = withNew -- meta.toRemove

                            val o = ((recombined, garbageCollected), Some((newFails, recombined)))

                            // Also remove the subscriptions
                            meta.toRemove.toList
                              .traverse(x => streamResourceAlg.traverse_(_.remove(x)))
                              .as(o)
                          }
                      }
                  }
                  .map(_.getOrElse(((initialOutput, activeSigs), None)))
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
    SubscriptionSupervisor[F](schemaState).flatMap { streamResouceAlg =>
      constructStream[F](rootInput, rootSel, schemaState, Some(streamResouceAlg))
    }

  def runSync[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  ): F[(Chain[EvalFailure], JsonObject)] =
    constructStream[F](rootInput, rootSel, schemaState, None).take(1).compile.lastOrError

  def computeToRemove(nodes: List[(Cursor, BigInt)], s: Set[BigInt]): Set[BigInt] = {
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
      case _ :: Nil | Nil            => groupNodeValues(children).flatMap { case (_, v) => computeToRemove(v, s) }.toSet
      case _                         => throw new Exception(msg)
    }
  }

  final case class SignalRecompute(
      toRemove: Set[BigInt],
      hcsa: Set[BigInt]
  )

  def computeMetadata(nodes: List[(Cursor, BigInt)], s: Set[BigInt]): SignalRecompute = {
    val tr = computeToRemove(nodes, s)
    val hcsa = s -- tr
    SignalRecompute(tr, hcsa)
  }

  def groupNodeValues[A](nvs: List[(Cursor, A)]): Map[GraphArc, List[(Cursor, A)]] =
    nvs.groupMap { case (c, _) => c.head } { case (c, v) => (c.tail, v) }

  def groupNodeValues2[A](nvs: List[(Cursor, A)]): Map[Option[GraphArc], List[(Cursor, A)]] =
    nvs.groupMap { case (c, _) => c.headOption } { case (c, v) => (c.tail, v) }

  def interpret[F[_]](
      rootSel: NonEmptyList[(PreparedField[F, Any], Chain[EvalNode[Any]])],
      executionDeps: Batching[F],
      schemaState: SchemaState[F],
      signalSubmission: Option[SignalMetadataAccumulator[F]] = None
  )(implicit F: Async[F], stats: Statistics[F]): WriterT[F, Chain[EvalFailure], Chain[EvalNode[Json]]] = {
    type W[A] = WriterT[F, Chain[EvalFailure], A]
    val W = Async[W]
    type E = Chain[EvalNode[Any]]
    val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]

    def failM[A](e: EvalFailure)(implicit M: Monoid[A]): W[A] = WriterT.put(M.empty)(Chain.one(e))

    def attemptUser[A](fa: IorT[F, String, A], constructor: Either[Throwable, String] => EvalFailure)(implicit
        M: Monoid[A]
    ): W[A] =
      lift(fa.value).attempt.flatMap {
        case Left(ex)              => WriterT.put(M.empty)(Chain(constructor(Left(ex))))
        case Right(Ior.Both(l, r)) => WriterT.put(r)(Chain(constructor(Right(l))))
        case Right(Ior.Left(l))    => WriterT.put(M.empty)(Chain(constructor(Right(l))))
        case Right(Ior.Right(r))   => WriterT.put(r)(Chain.empty)
      }

    def attemptUserE(fa: IorT[F, String, E], constructor: Either[Throwable, String] => EvalFailure): W[E] =
      attemptUser[E](fa, constructor)

    Supervisor[F].mapK(lift).use { sup =>
      executionDeps.batchExecutionState[W].flatMap { batchStateMap =>
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

        final case class NodeBatch(
            inputs: Chain[(CursorGroup, Batch[F, BatchKey, BatchValue, BatchResult])],
            resolve: Set[BatchKey] => F[Map[BatchKey, BatchValue]]
        )

        def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
          sup.supervise(stats.updateStats(name, duration, size)).void

        def runSignal(df: PreparedDataField[F, Any, Any], sf: SignalResolver[F, Any, Any, Any, Any], inputs: Chain[EvalNode[Any]]) =
          inputs
            .parFlatTraverse { in =>
              val hdF: IorT[F, String, EvalNode[Any]] =
                sf.head(in.value).map(v => in.copy(value = (in.value, v).asInstanceOf[Any]))

              val headChainF = hdF.map(Chain(_))

              val debugCursor = in.cursorGroup.field(df.name)

              signalSubmission match {
                case None =>
                  attemptUserE(
                    headChainF,
                    EvalFailure.SignalHeadResolution(debugCursor, _, in.value)
                  )
                case Some(ss) =>
                  attemptUser(
                    sf.tail(in.value).map(Chain(_)),
                    EvalFailure.SignalTailResolution(debugCursor, _, in.value)
                  )
                    .flatMap(_.flatTraverse { dst =>
                      val df2 = df.copy(resolve = sf.resolver.contramap[Any]((in.value, _)))
                      lift(ss.add(in.cursorGroup.absolutePath, in.value, df2, dst.ref, dst.key)).flatMap { id =>
                        // if hd fails, then unsubscribe again
                        // note that if any parent changes, this (unsubscription) will happen automatically, so this
                        // can be viewed as an optimization
                        val handledHeadF =
                          IorT {
                            hdF.value
                              .attemptTap {
                                case Left(_)            => ss.remove(id)
                                case Right(Ior.Left(_)) => ss.remove(id)
                                case _                  => F.unit
                              }
                          }

                        attemptUserE(
                          handledHeadF.map(Chain(_)),
                          EvalFailure.SignalHeadResolution(debugCursor, _, in.value)
                        )
                      }
                    })
              }
            }
            .flatMap { nvs =>
              // we tupled the initial into nvs (head), cast since expects (Any, Any) but EvalNode contains Any
              val df2 = df.copy(resolve = sf.resolver.asInstanceOf[Resolver[F, Any, Any]])
              runInputs(df2, nvs)
            }

        def runInputs(
            df: PreparedDataField[F, Any, Any],
            inputs: Chain[EvalNode[Any]]
        ): W[Either[Chain[EvalNode[Any]], NodeBatch]] = {
          val n = executionDeps.nodeMap(df.id)

          df.resolve match {
            case EffectResolver(resolve) =>
              inputs
                .parFlatTraverse { in =>
                  val next = in.cursorGroup.field(df.name)

                  attemptUser(
                    resolve(in.value).timed.semiflatMap { case (dur, v) =>
                      val out = Chain(EvalNode(next, v))
                      submit(n.name, dur, 1) as out
                    },
                    EvalFailure.EffectResolution(next, _, in.value)
                  )
                }
                .map(Left(_))
            case BatchResolver(batcher, partition) =>
              val impl = schemaState.batchers(batcher.id)

              val partitioned: W[Chain[(CursorGroup, Batch[F, Any, Any, Any])]] = inputs.parFlatTraverse { in =>
                val next = in.cursorGroup.field(df.name)

                attemptUser[Chain[(CursorGroup, Batch[F, Any, Any, Any])]](
                  partition(in.value).map(b => Chain((next, b))),
                  EvalFailure.BatchPartitioning(next, _, in.value)
                )
              }

              partitioned
                .map { zs =>
                  Right(
                    NodeBatch(
                      zs,
                      xs =>
                        impl(xs).timed
                          .flatMap { case (dur, value) =>
                            submit(Planner.makeBatchName(batcher), dur, xs.size) >> submit(n.name, dur, xs.size).as(value)
                          }
                    )
                  )
                }
            case sr @ SignalResolver(_, _, _) => runSignal(df, sr, inputs)
          }
        }

        def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] =
          Chain.fromSeq(dfs.toList).parFlatTraverse {
            case PreparedFragField(id, specify, selection) =>
              runFields(selection.fields, in.flatMap(x => Chain.fromOption(specify(x.value)).map(y => x.setValue(y))))
            case df @ PreparedDataField(id, name, _, _, _) => runDataField(df, in)
          }

        def startNext(df: PreparedDataField[F, Any, Any], outputs: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = {
          def evalSel(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = W.defer {
            s match {
              case PreparedLeaf(name, enc) => W.pure(in.map(en => en.setValue(enc(en.value))))
              case Selection(fields)       => runFields(fields, in)
              case PreparedList(of) =>
                val partitioned =
                  in.flatMap { nv =>
                    val inner = Chain.fromSeq(nv.value.asInstanceOf[Seq[Any]])

                    inner.mapWithIndex { case (v, i) => nv.succeed(v, _.index(i)) }
                  }
                evalSel(of, partitioned)
            }
          }

          evalSel(df.selection, outputs)
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
              val trivialF = trivial.parFlatTraverse { case (df, fas) => startNext(df, fas) }

              val batchedF =
                NonEmptyChain
                  .fromChain(batched)
                  .traverse { nec =>
                    val (_, b) = nec.head

                    val keys: Set[BatchKey] = nec.toChain
                      .flatMap { case (_, bn) => bn.inputs.flatMap { case (_, b) => Chain.fromSeq(b.keys) } }
                      .toIterable
                      .toSet

                    val resolvedF: F[WriterT[F, Chain[EvalFailure], Chain[EvalNode[Json]]]] =
                      b.resolve(keys)
                        .map { (resultLookup: Map[BatchKey, BatchValue]) =>
                          nec.toChain.parFlatTraverse { case (df, bn) =>
                            val mappedValuesF: W[Chain[EvalNode[Any]]] = bn.inputs.parFlatTraverse { case (c, b) =>
                              // find all results for this key
                              val lookupsF: W[Chain[(BatchKey, BatchValue)]] = Chain.fromSeq(b.keys).flatTraverse { (k: BatchKey) =>
                                resultLookup.get(k) match {
                                  case None =>
                                    failM[Chain[(BatchKey, BatchValue)]](
                                      EvalFailure.BatchMissingKey(c, resultLookup, b.keys, k)
                                    )
                                  case Some(x) => W.pure(Chain(k -> x))
                                }
                              }

                              lookupsF.flatMap { keys =>
                                attemptUserE(
                                  b.post(keys.toList).map(res => Chain(EvalNode(c, res))),
                                  EvalFailure.BatchPostProcessing(c, _, keys)
                                )
                              }
                            }

                            mappedValuesF.flatMap(startNext(df, _))
                          }
                        }

                    lift(resolvedF).attempt
                      .flatMap {
                        case Left(ex) =>
                          val posses =
                            nec.toChain.flatMap { case (_, bn) => bn.inputs.map { case (nm, _) => nm } }
                          WriterT.put(Chain.empty[EvalNode[Json]])(
                            Chain(EvalFailure.BatchResolution(posses, ex, keys))
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

  def reconstructField[F[_]](p: Prepared[F, Any], cursors: List[(Cursor, Option[Json])]): Json = {
    // We first need to determine if the result is just null
    // To do so, there must be one path, and it must be empty
    val m = groupNodeValues2(cursors)
    val terminal = m.get(None)
    terminal match {
      case Some(t) if m.size == 1 && t.size == 1 && t.exists { case (_, o) => o.isEmpty } => Json.Null
      case _ =>
        p match {
          case PreparedLeaf(_, _) =>
            cursors
              .collectFirst { case (_, Some(x)) => x }
              .getOrElse(Json.Null)
          case PreparedList(of) =>
            Json.fromValues(
              m.toList
                .collect { case (Some(GraphArc.Index(i)), tl) => i -> tl }
                .sortBy { case (i, _) => i }
                .map { case (_, tl) => reconstructField[F](of, tl) }
            )
          case Selection(fields) => _reconstructSelection(cursors, fields, m).asJson
        }
    }
  }

  def _reconstructSelection[F[_]](
      levelCursors: List[(Cursor, Option[Json])],
      sel: NonEmptyList[PreparedField[F, Any]],
      m: Map[Option[GraphArc], List[(Cursor, Option[Json])]]
  ): JsonObject = {
    sel
      .map { pf =>
        pf match {
          case PreparedFragField(id, specify, selection) =>
            _reconstructSelection(levelCursors, selection.fields, m)
          case df @ PreparedDataField(_, name, _, selection, _) =>
            JsonObject(
              name -> m.get(Some(GraphArc.Field(name))).map(reconstructField(selection, _)).getOrElse(Json.Null)
            )
        }
      }
      .reduceLeft(_ deepMerge _)
  }

  def reconstructSelection[F[_]](
      levelCursors: List[(Cursor, Option[Json])],
      sel: NonEmptyList[PreparedField[F, Any]]
  ): JsonObject =
    _reconstructSelection(levelCursors, sel, groupNodeValues2(levelCursors))

  // def reconstructField[F[_]](p: Prepared[F, Any], cursors: List[(Cursor, Json)]): Json = {
  //   // If there is nothing else on this path, then return the json
  //   // This handles arbitary null's in the tree
  //   if (cursors.size == 1 && cursors.exists { case (c, j) => c.path.size == 1 }) cursors.head._2
  //   else {
  //     p match {
  //       case PreparedLeaf(_, _) =>
  //         cursors
  //           .collectFirst { case (_, v) if !v.isNull => v }
  //           .getOrElse(Json.Null)
  //       case PreparedList(of) =>
  //         Json.fromValues(
  //           groupNodeValues(cursors).toList
  //             .map {
  //               case (GraphArc.Index(i), tl) => i -> tl
  //               case _                       => ???
  //             }
  //             .sortBy { case (i, _) => i }
  //             .map { case (_, tl) => reconstructField[F](of, tl) }
  //         )
  //       case Selection(fields) => reconstructSelection(cursors, fields).asJson
  //     }
  //   }
  // }

  // def reconstructSelection[F[_]](
  //     levelCursors: List[(Cursor, Json)],
  //     sel: NonEmptyList[PreparedField[F, Any]]
  // ): JsonObject = {
  //   val m = groupNodeValues(levelCursors)

  //   sel
  //     .map { pf =>
  //       pf match {
  //         case PreparedFragField(id, specify, selection) =>
  //           reconstructSelection(levelCursors, selection.fields)
  //         case df @ PreparedDataField(_, name, _, selection, _) =>
  //           JsonObject(name -> reconstructField(selection, m(GraphArc.Field(name))))
  //       }
  //     }
  //     .reduceLeft(_ deepMerge _)
  // }
}
