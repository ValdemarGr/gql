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
                      (cursor.field(df.id, df.name), k)
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
                            val l = all.toList.groupBy { case (m, _) => m.groupId }.toList

                            val prepped =
                              l.map { case (group, results) =>
                                val (rootCursor, df) = groupIdMapping(group.toInt)
                                // Before re-constructing, drop the head of the cursor
                                // Since that edge will be where the result should be stitched into
                                val tailResults = results.map { case (m, x) => m.relativePath.tail -> x }
                                val rec = reconstructField(df.selection, tailResults)

                                // Left is the object, right is the position to replace
                                (rec, rootCursor.field(df.id, df.name))
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

  // final case class ArcGrouping[A](
  //     field: Map[Int, List[(Cursor, A)]],
  //     arr: Map[Int, List[(Cursor, A)]],
  //     frag: Map[Int, List[(Cursor, A)]],
  //     terminal: List[A]
  // )
  // def groupNodeValues3[A](nvs: List[(Cursor, A)]) = {
  //   nvs.foldLeft(ArcGrouping(Map.empty, Map.empty, Map.empty, List.empty)) { case (acc, (c, v)) =>
  //     c.headOption match {
  //       case Some(GraphArc.Field(id, _))    => acc.copy(field = acc.field + (id -> acc)))
  //       // case Some(GraphArc.Index(idx))      => acc.copy(arr = acc.arr.updated(idx, (c.tail, v) :: acc.arr.getOrElse(idx, Nil)))
  //       // case Some(GraphArc.Fragment(id, _)) => acc.copy(frag = acc.frag.updated(0, (c.tail, v) :: acc.frag.getOrElse(0, Nil)))
  //       // case None                           => acc.copy(terminal = v :: acc.terminal)
  //     }
  //   }
  // }

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

              val debugCursor = in.cursorGroup.field(df.id, df.name)

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
                  val next = in.cursorGroup.field(df.id, df.name)

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
                val next = in.cursorGroup.field(df.id, df.name)

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
            case PreparedFragField(id, typename, specify, selection) =>
              runFields(
                selection.fields,
                in.flatMap(x => Chain.fromOption(specify(x.value)).map(y => x.succeed(y, _.fragment(id, typename))))
              )
            case df @ PreparedDataField(id, name, _, _, _, _) => runDataField(df, in)
          }

        def startNext(df: PreparedDataField[F, Any, Any], outputs: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = {
          def evalSel(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = W.defer {
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

                evalSel(of, continuations.flatMap(_.toChain)).map(_ ++ emties)
              case PreparedOption(of) =>
                val (nulls, continuations) =
                  in.partitionEither { nv =>
                    val inner = nv.value.asInstanceOf[Option[Any]]

                    inner match {
                      case None    => Left(nv.setValue(Json.Null))
                      case Some(v) => Right(nv.setValue(v))
                    }
                  }
                evalSel(of, continuations).map(_ ++ nulls)
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
              .map{ case (_, v) => v }
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
          case df @ PreparedDataField(id, name, _, selection, _, alias) =>
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

// object InInterpretationRebuilding {
//   final case class BatchExecution[F[_]](
//       remainingInputs: Set[Int],
//       inputMap: Map[Int, Chain[BatchKey]],
//       awaiters: Chain[Deferred[F, Map[BatchKey, BatchValue]]]
//   )

//   final case class BatchMap[F[_]](m: Map[Int, Ref[F, BatchExecution[F]]])

//   final case class BatchGroups[F[_]](
//       nodeMap: Map[Int, Planner.Node],
//       dataFieldMap: Map[Int, PreparedDataField[F, Any, Any]],
//       batches: List[NonEmptyList[Int]]
//   ) {
//     def batchExecutionState[F[_]](implicit F: Concurrent[F]): F[BatchMap[F]] =
//       batches
//         .flatTraverse { batch =>
//           val l = batch.toList
//           F.ref(BatchExecution[F](l.toSet, Map.empty[Int, Chain[EvalNode[Any]]], Chain.empty)).map(s => l.map(_ -> s))
//         }
//         .map(_.toMap)
//         .map(BatchMap(_))
//   }
//   object BatchGroups {
//     def plan[F[_]](rootSel: NonEmptyList[PreparedField[F, Any]], plan: Planner.NodeTree): BatchGroups[F] = {
//       val flat = plan.flattened

//       def unpackPrep(prep: Prepared[F, Any]): Eval[List[(Int, PreparedDataField[F, Any, Any])]] = Eval.defer {
//         prep match {
//           case PreparedLeaf(_, _) => Eval.now(Nil)
//           case PreparedList(of)   => unpackPrep(of)
//           case PreparedOption(of) => unpackPrep(of)
//           case Selection(fields)  => flattenDataFieldMap(fields).map(_.toList)
//         }
//       }

//       def flattenDataFieldMap(sel: NonEmptyList[PreparedField[F, Any]]): Eval[NonEmptyList[(Int, PreparedDataField[F, Any, Any])]] =
//         Eval.defer {
//           sel.flatTraverse { pf =>
//             pf match {
//               case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
//                 val hd = id -> df
//                 unpackPrep(selection).map(tl => NonEmptyList(hd, tl))
//               case PreparedFragField(_, _, _, selection) => flattenDataFieldMap(selection.fields)
//             }
//           }
//         }

//       val nodeMap: Map[Int, Planner.Node] = flat.toList.map(x => x.id -> x).toMap

//       val dataFieldMap: Map[Int, PreparedDataField[F, Any, Any]] = flattenDataFieldMap(rootSel).value.toList.toMap

//       val batches: List[NonEmptyList[Int]] =
//         flat
//           .groupBy(_.end)
//           .toList
//           .zipWithIndex
//           .flatMap { case ((_, group), idx) =>
//             group
//               .groupBy(_.batchName)
//               .filter { case (o, nodes) => nodes.size > 1 && o.isDefined }
//               .toList
//               .map { case (nodeType, nodes) => nodes.map(_.id) }
//           }

//       BatchGroups(nodeMap, dataFieldMap, batches)
//     }
//   }

//   def interpretField[F[_]](
//       pf: PreparedField[F, Any],
//       inputs: Chain[(Int, EvalNode[Any])]
//   )(implicit F: Async[F], B: BatchMap[F], SS: SchemaState[F]): F[Chain[(Int, (String, EvalNode[Json]))]] = pf match {
//     case PreparedFragField(id, _, specify, sel) =>
//       val reduced = inputs.flatMap { case (grp, x) =>
//         Chain.fromOption(specify(x.value).map(y => (grp, x.setValue(y))))
//       }
//       Chain.fromSeq(sel.fields.toList).parFlatTraverse(f => interpretField[F](f, reduced))
//     case PreparedDataField(id, name, BatchResolver(batcher, partition), prep, _) =>
//       sealed trait BatchSubmissionOutcome
//       object BatchSubmissionOutcome {
//         final case class Final(be: BatchExecution[F]) extends BatchSubmissionOutcome
//         case object Empty extends BatchSubmissionOutcome
//         final case class Await(fa: F[Map[BatchKey, BatchValue]]) extends BatchSubmissionOutcome
//       }

//       // TODO error handling
//       val partitionedF =
//         inputs
//           .map{ case (_, x) => x.value }
//           .parTraverse(partition)

//       val fa: F[BatchSubmissionOutcome] =
//         B.m.get(id) match {
//           case None => F.pure(BatchSubmissionOutcome.Empty)
//           case Some(b) =>
//             F.deferred[Map[BatchKey, BatchValue]].flatMap { result =>
//               b.modify { s =>
//                 val newSet = s.remainingInputs - id
//                 val newMap = s.inputMap + (id -> inputs.map { case (_, x) => x.value })
//                 val newState = BatchExecution[F](newSet, newMap, s.awaiters append result)
//                 val outcome =
//                   if (newSet.isEmpty && s.remainingInputs.nonEmpty) BatchSubmissionOutcome.Final(newState)
//                   else BatchSubmissionOutcome.Await(result.get)
//                 (newState, outcome)
//               }
//             }
//         }

//       val fb: F[Chain[EvalNode[Json]]] =
//         fa.flatMap {
//           // No batching state, so just start
//           case BatchSubmissionOutcome.Empty => ??? //runDataInputs(Chain(df -> input))
//           // join
//           // There is a batch state, but we didn't add the final input
//           // Stop here, someone else will start the batch
//           // Some awaiting semantics
//           case BatchSubmissionOutcome.Await(fa) => ??? //W.pure(Chain.empty)
//           // join
//           // We are the final submitter, start the computation
//           case BatchSubmissionOutcome.Final(be) =>
//             // TODO exception handling
//             val res: F[Map[Any, Any]] =
//               SS.batchers(batcher.id)(be.inputMap.values.toList.flatMap(_.toList).toSet)

//             val completeAll: F[Unit] =
//               res.flatMap { m =>
//                 val completeOthers = be.awaiters.traverse_(_.complete(m))

//                 val completeSelf =
//                   be
//               }
//             ???
//           /*
//           val in = Chain.fromSeq(inputs.toList)

//           val inputLst = in

//           val withDfs = inputLst.map { case (k, v) => executionDeps.dataFieldMap(k) -> v }

//           runDataInputs(withDfs)*/
//         }

//       fb.map(_.zipWith(inputs)((_, _)).map { case (res, (grp, _)) => (grp, (name, res)) })
//   }

//   def interpretContinuation[F[_]](
//       cont: Prepared[F, Any],
//       results: Chain[EvalNode[Any]]
//   )(implicit F: Async[F]): F[Chain[EvalNode[Json]]] =
//     F.defer {
//       cont match {
//         case PreparedLeaf(name, encode) => F.pure(results.map(x => x.setValue(encode(x.value))))
//         case Selection(fields)          => ???
//         case PreparedList(of) =>
//           val (mappings, outs) =
//             results.zipWithIndex.partitionBifold { case (en, group) =>
//               val chunk = Chain.fromSeq(en.value.asInstanceOf[Seq[Any]])

//               val mapping = group -> en.cursorGroup
//               val out = chunk.mapWithIndex { case (v, i) => en.succeed(v, _.index(i)) }
//               (mapping, out)
//             }

//           val lookup = mappings.toList.toMap
//           interpretContinuation(of, outs.flatten).map { out =>
//             val grps =
//               out.toList.groupMap(_.cursorGroup.relativePath.last)(x =>
//                 x.copy(cursorGroup = x.cursorGroup.copy(relativePath = x.cursorGroup.relativePath.dropInit))
//               )
//             Chain.fromIterableOnce(grps.values).mapWithIndex { case (grp, i) => EvalNode(lookup(i), grp.map(_.value).asJson) }
//           }
//         case PreparedOption(of) =>
//           val (here, conts) =
//             results.partitionEither { en =>
//               val oa = en.value.asInstanceOf[Option[Any]]

//               oa match {
//                 case None    => Left(en.setValue(Json.Null))
//                 case Some(x) => Right(en.setValue(x))
//               }
//             }

//           interpretContinuation(of, conts).map(_ ++ here)
//       }
//     }
// }
