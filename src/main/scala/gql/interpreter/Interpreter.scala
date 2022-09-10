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
  type BatchKey = Any
  type BatchValue = Any
  type BatchResult = Any

  sealed trait ErrorPath
  object ErrorPath {
    final case class Field(name: String) extends ErrorPath
    final case class Index(index: Int) extends ErrorPath
  }

  sealed trait ErrorType
  object ErrorType {
    final case class BatchResolutionError(
        paths: Chain[ErrorPath],
        exception: Throwable,
        keys: Set[Any]
    ) extends ErrorType
    final case class BatchReassociationError(
        path: ErrorPath,
        exception: Throwable,
        resultMap: Map[BatchKey, BatchValue]
    ) extends ErrorType
    final case class BatchMissingKeyError(
        path: ErrorPath,
        resultMap: Map[BatchKey, BatchValue],
        key: BatchKey
    ) extends ErrorType
    final case class BatchPartitioningError(
        path: ErrorPath,
        error: Either[Throwable, String],
        input: Any
    ) extends ErrorType
    final case class EffectResolutionError(
        path: ErrorPath,
        error: Either[Throwable, String],
        input: Any
    ) extends ErrorType
  }

  final case class Error(
      paths: Chain[ErrorPath],
      userMessage: String
  )

  final case class Result(
      errors: Chain[EvalFailure],
      data: Json
  )

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
      path: Cursor,
      nameMap: Map[Int, String]
  ): Json =
    path.uncons match {
      case None => subTree
      case Some((p, tl)) =>
        p match {
          case Ided(id) =>
            val oldObj = oldTree.asObject.get
            val name = nameMap(id)
            val oldValue = oldObj(name).get
            val newSubTree = stitchInto(oldValue, subTree, tl, nameMap)
            // println(oldValue)
            // println(s"adding $name>>>>>>>>>>")
            // println(newSubTree)
            oldObj.add(name, newSubTree).asJson
          case Index(index) =>
            val oldArr = oldTree.asArray.get
            oldArr.updated(index, stitchInto(oldArr(index), subTree, tl, nameMap)).asJson
        }
    }

  // TODO also handle the rest of the EvalFailure structure
  def combineSplit(fails: Chain[EvalFailure], succs: Chain[EvalNode[Json]]): Chain[(NodeMeta, Json)] =
    fails.flatMap(_.meta).map(m => (m, Json.Null)) ++ succs.map(n => (n.meta, n.value))

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
        val nameMap = deps.dataFieldMap.view.mapValues(_.name).toMap

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
                    val s = activeChanges.toList.map { case (k, _) => k }.toSet
                    val allSigNodes = activeSigs.toList.map { case (k, (cursor, _, df)) => (cursor.ided(df.id), k) }
                    val meta = computeMetadata(allSigNodes, s)
                    val rootNodes: List[(BigInt, Any)] = activeChanges.filter { case (k, _) => meta.hcsa.contains(k) }
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
                            val groupIdMapping =
                              newRootSel.toList.map { case (df, rootCursor, _, idx) => idx -> (rootCursor, df) }.toMap

                            val all = combineSplit(newFails, newSuccs)
                            val l: List[(BigInt, List[(NodeMeta, Json)])] =
                              all.toList.groupBy { case (m, _) => m.cursorGroup }.toList

                            // println("performing new stitch $$$$$$$$$$$$$$$$$$")
                            val recombined: JsonObject =
                              l
                                .map { case (group, results) =>
                                  val (rootCursor, df) = groupIdMapping(group.toInt)
                                  // TODO
                                  // reconstructField(df.selection, results)
                                  val rc =
                                    reconstructSelection(results.map { case (m, x) => m.relativePath -> x }, NonEmptyList.one(df))

                                  (rc, rootCursor.ided(df.id))
                                }
                                .foldLeft(prevOutput) { case (accum, (obj, pos)) =>
                                  // println("stitch iteration ###################")
                                  // println(accum)
                                  // this object has one entry, the resolved field itself
                                  val hack = obj.toMap.head._2
                                  // println(s"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ${pos.path.map(_.asInstanceOf[Ided].id).map(nameMap.apply).mkString_("->")}")
                                  // println(hack)
                                  stitchInto(accum.asJson, hack, pos, nameMap).asObject.get
                                }

                            val withNew = newSigs ++ activeSigs
                            val garbageCollected = withNew -- meta.toRemove

                            val o = ((recombined, garbageCollected), Some((newFails, recombined)))

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

  def interpret[F[_]](
      rootSel: NonEmptyList[(PreparedField[F, Any], Chain[EvalNode[Any]])],
      executionDeps: Batching[F],
      schemaState: SchemaState[F],
      signalSubmission: Option[SignalMetadataAccumulator[F]] = None
  )(implicit F: Async[F], stats: Statistics[F]): WriterT[F, Chain[ErrorType], Chain[EvalNode[Json]]] = {
    type W[A] = WriterT[F, Chain[ErrorType], A]
    val W = Async[W]
    type E = Chain[EvalNode[Any]]
    val lift: F ~> W = WriterT.liftK[F, Chain[ErrorType]]
    def fail[A](e: ErrorType): W[Chain[A]] = WriterT.put(Chain.empty[A])(Chain.one(e))
    def failM[A](e: ErrorType)(implicit M: Monoid[A]): W[A] = WriterT.put(M.empty)(Chain.one(e))

    def liftAttemptUser[A](fa: EitherT[F, String, A]): W[Either[Either[Throwable, String], A]] =
      lift(fa.value).attempt
        .map {
          case Left(err)        => Left(Left(err))
          case Right(Left(err)) => Left(Right(err))
          case Right(Right(x))  => Right(x)
        }

    def liftUser[A](fa: EitherT[F, String, A]): W[Either[String, A]] =
      lift(fa.value)

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
            inputs: Chain[(NodeMeta, Batch[F, BatchKey, BatchValue, BatchResult])],
            resolve: Set[BatchKey] => F[Map[BatchKey, BatchValue]]
        )

        def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
          sup.supervise(stats.updateStats(name, duration, size)).void

        def submitW(name: String, duration: FiniteDuration, size: Int): W[Unit] =
          lift(submit(name, duration, size))

        def runSignal(df: PreparedDataField[F, Any, Any], sf: SignalResolver[F, Any, Any, Any, Any], inputs: Chain[EvalNode[Any]]) =
          inputs
            .parFlatTraverse { in =>
              val hdF =
                lift(sf.head(in.value)).map(v => Chain(in.copy(value = (in.value, v).asInstanceOf[Any])))

              def failHeadF[A](e: Throwable) =
                fail[EvalNode[A]](EvalFailure(Chain(in.meta), None, "during signal head resolution", Some(e)))

              signalSubmission match {
                case None => hdF.handleErrorWith(failHeadF)
                case Some(ss) =>
                  lift(sf.tail(in.value)).attempt
                    .flatMap {
                      case Left(e) =>
                        fail[EvalNode[Any]](EvalFailure(Chain(in.meta), None, "during signal tail resolution", Some(e)))
                      case Right(dst) =>
                        // close in the initial value for tail Resolver[F, (I, T), A]
                        val df2 = df.copy(resolve = sf.resolver.contramap[Any]((in.value, _)))
                        lift(ss.add(in.meta.absolutePath, in.value, df2, dst.ref, dst.key)).flatMap { id =>
                          // if hd fails, then unsubscribe again
                          hdF.handleErrorWith(e => lift(ss.remove(id)) >> failHeadF(e))
                        }
                    }
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
                  val next = in.meta.ided(df.id)

                  liftAttemptUser(resolve(in.value)).timed
                    .flatMap[E] {
                      case (_, Left(e)) =>
                        failM[E](
                          ErrorType.EffectResolutionError(
                            null,
                            e,
                            in.value
                          )
                        )
                      case (dur, Right(v)) =>
                        val out = Chain(EvalNode(next, v))
                        submitW(n.name, dur, 1) as out
                    }
                }
                .map(Left(_))
            case BatchResolver(batcher, partition) =>
              val impl = schemaState.batchers(batcher.id)

              val partitioned: W[Chain[(NodeMeta, Batch[F, Any, Any, Any])]] = inputs.parFlatTraverse { in =>
                val next = in.meta.ided(df.id)

                liftAttemptUser(partition(in.value))
                  .flatMap {
                    case Left(e) =>
                      failM[Chain[(NodeMeta, Batch[F, Any, Any, Any])]](
                        ErrorType.BatchPartitioningError(
                          null,
                          e,
                          in.value
                        )
                      )
                    case Right(b) => W.pure(Chain((next, b)))
                  }
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

                    b
                      .resolve(keys)
                      .flatMap { (resultLookup: Map[BatchKey, BatchValue]) =>
                        nec.toChain.parFlatTraverse { case (df, bn) =>
                          val mappedValuesF: W[Chain[EvalNode[Any]]] = bn.inputs.parFlatTraverse { case (c, b) =>
                            // find all results for this key
                            val lookupsF: W[Chain[(BatchKey, BatchValue)]] = Chain.fromSeq(b.keys).flatTraverse { (k: BatchKey) =>
                              resultLookup.get(k) match {
                                case None =>
                                  ??? // fail[(BatchKey, BatchValue)](EvalFailure(Chain(c), None, "during batch resolution", None))
                                case Some(x) => W.pure(Chain(k -> x))
                              }
                            }

                            lookupsF.flatMap { keys =>
                              ???
                            // lift(b.post(keys.toList).map(res => Chain(EvalNode(c, res))).value)
                            //   .handleErrorWith(e =>
                            //     fail[EvalNode[Any]](EvalFailure(Chain(c), None, "during batch post-resolution", Some(e)))
                            //   )
                            }

                          }

                          mappedValuesF.flatMap(startNext(df, _))
                        }
                      }
                      .handleErrorWith { e =>
                        val nms = nec.toChain.flatMap { case (_, bn) => bn.inputs.map { case (nm, _) => nm } }
                        fail(EvalFailure(nms, None, "during batched resolve", Some(e)))
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

  def reconstructField[F[_]](p: Prepared[F, Any], cursors: List[(Cursor, Json)]): Json =
    p match {
      case PreparedLeaf(_, _) =>
        cursors match {
          case (_, x) :: Nil => x
          case _             => ???
        }
      case PreparedList(of) =>
        Json.fromValues(
          groupNodeValues(cursors).toList
            .map {
              case (Index(i), tl) => i -> tl
              case _              => ???
            }
            .sortBy { case (i, _) => i }
            .map { case (_, tl) => reconstructField[F](of, tl) }
        )
      case Selection(fields) => reconstructSelection(cursors, fields).asJson
    }

  def reconstructSelection[F[_]](
      levelCursors: List[(Cursor, Json)],
      sel: NonEmptyList[PreparedField[F, Any]]
  ): JsonObject = {
    val m = groupNodeValues(levelCursors)

    sel
      .map { pf =>
        pf match {
          case PreparedFragField(id, specify, selection) =>
            reconstructSelection(levelCursors, selection.fields)
          case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
            JsonObject(name -> reconstructField(selection, m(Ided(id))))
        }
      }
      .reduceLeft(_ deepMerge _)
  }
}
