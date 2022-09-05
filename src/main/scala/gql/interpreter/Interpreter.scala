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
  def planAndRun[F[_]: Async: Statistics](
      rootSel: NonEmptyList[(PreparedField[F, Any], List[NodeValue])],
      schemaState: SchemaState[F],
      signalAccum: Option[SignalMetadataAccumulator[F]] = None
  ): F[(List[NodeValue], Batching[F])] = {
    val rootFields = rootSel.map { case (k, _) => k }
    for {
      costTree <- Planner.costTree[F](rootFields)
      plan = Planner.plan(costTree)
      executionDeps = Batching.plan[F](rootFields, plan)
      result <- interpret[F](rootSel, executionDeps, schemaState, signalAccum)
    } yield (result, executionDeps)
  }

  def run[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      plan: NonEmptyList[Planner.Node],
      schemaState: SchemaState[F]
  ): F[JsonObject] =
    interpret[F](rootSel.map((_, List(NodeValue.empty(rootInput, BigInt(0))))), Batching.plan[F](rootSel, plan), schemaState)
      .flatMap(reconstruct[F](rootSel, _))

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

  def runSync[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  ) = {
    for {
      costTree <- Planner.costTree[F](rootSel)
      plan = Planner.plan(costTree)
      out <- run[F](rootInput, rootSel, plan, schemaState)
    } yield out
  }

  def runStreamed[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, JsonObject] =
    SubscriptionSupervisor[F](schemaState).flatMap { implicit streamResouceAlg =>
      def runStreamIt(
          root: NonEmptyList[(PreparedField[F, Any], List[NodeValue])]
      ): F[(List[NodeValue], Batching[F], Map[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])])] =
        SignalMetadataAccumulator[F].flatMap { accumulator =>
          (planAndRun[F](root, schemaState, Some(accumulator)), accumulator.getState)
            .mapN { case ((res, deps), sigs) => (res, deps, sigs) }
        }

      // first iteration
      fs2.Stream
        .eval(runStreamIt(rootSel.map((_, List(NodeValue.empty(rootInput, BigInt(1)))))))
        .flatMap { case (initialNvs, deps, initialSignals) =>
          fs2.Stream.eval(reconstruct(rootSel, initialNvs)).flatMap { initialOutput =>
            fs2.Stream.emit(initialOutput) ++
              streamResouceAlg.changeLog
                .evalScan((initialOutput, initialSignals)) { case ((prevOutput, activeSigs), changes) =>
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
                          (field, cursor, List(NodeValue(NodeMeta.startAt(cursor, cursorGroup), input)), cursorGroup)
                        }

                      prepaedRoots.toNel
                        .traverse { newRootSel =>
                          runStreamIt(newRootSel.map { case (df, _, inputs, _) => (df, inputs) })
                            .flatMap { case (newNvs, _, newSigs) =>
                              val nameMap = deps.dataFieldMap.view.mapValues(_.name).toMap

                              val groupIdMapping =
                                newRootSel.toList.map { case (df, rootCursor, _, idx) => idx -> (rootCursor, df) }.toMap

                              val l: List[(BigInt, List[NodeValue])] = newNvs.groupBy(_.meta.cursorGroup).toList

                              // println("performing new stitch $$$$$$$$$$$$$$$$$$")
                              val recombinedF: F[JsonObject] =
                                l
                                  .parTraverse { case (group, results) =>
                                    val (rootCursor, df) = groupIdMapping(group.toInt)
                                    // TODO
                                    // reconstructField(df.selection, results)
                                    reconstruct[F](NonEmptyList.one(df), results).tupleRight(rootCursor.ided(df.id))
                                  }
                                  .map(_.foldLeft(prevOutput) { case (accum, (obj, pos)) =>
                                    // println("stitch iteration ###################")
                                    // println(accum)
                                    // this object has one entry, the resolved field itself
                                    val hack = obj.toMap.head._2
                                    // println(s"<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ${pos.path.map(_.asInstanceOf[Ided].id).map(nameMap.apply).mkString_("->")}")
                                    // println(hack)
                                    stitchInto(accum.asJson, hack, pos, nameMap).asObject.get
                                  })

                              recombinedF.flatMap { res =>
                                val withNew = newSigs ++ activeSigs
                                val garbageCollected = withNew -- meta.toRemove

                                meta.toRemove.toList
                                  .traverse(streamResouceAlg.remove)
                                  .as((res, garbageCollected))
                              }
                            }
                        }
                    }
                    .map(_.getOrElse((initialOutput, activeSigs)))
                }
                .map { case (j, _) => j }
          }
        }
    }

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
      rootSel: NonEmptyList[(PreparedField[F, Any], List[NodeValue])],
      executionDeps: Batching[F],
      schemaState: SchemaState[F],
      signalSubmission: Option[SignalMetadataAccumulator[F]] = None
  )(implicit F: Async[F], stats: Statistics[F]): F[List[NodeValue]] =
    Supervisor[F].use { sup =>
      executionDeps.batchExecutionState[F].flatMap { batchStateMap =>
        def submitAndMaybeStart(nodeId: Int, input: List[NodeValue]): F[StateSubmissionOutcome] =
          batchStateMap
            .get(nodeId)
            .traverse(_.modify { s =>
              val newSet = s.remainingInputs - nodeId
              val newMap = s.inputMap + (nodeId -> input)
              val newState = BatchExecutionState(newSet, newMap)
              (newState, if (newSet.isEmpty && s.remainingInputs.nonEmpty) FinalSubmission(newMap) else NotFinalSubmission)
            })
            .map(_.getOrElse(NoState))

        type BatchKey = Any

        final case class NodeBatch(
            inputs: List[(NodeMeta, Batch[F, BatchKey, Any, Any])],
            resolve: Set[BatchKey] => F[Map[BatchKey, Any]]
        )

        def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
          sup.supervise(stats.updateStats(name, duration, size)).void

        def collapseInputs(
            df: PreparedDataField[F, Any, Any],
            inputs: List[NodeValue]
        ): F[Either[List[NodeValue], NodeBatch]] = {
          val n = executionDeps.nodeMap(df.id)

          df.resolve match {
            case PureResolver(resolve) =>
              F.pure(Left(inputs.map(in => in.ided(df.id, resolve(in.value)))))
            case EffectResolver(resolve) =>
              inputs
                .parTraverse { in =>
                  resolve(in.value).timed
                    .flatMap { case (dur, value) =>
                      submit(n.name, dur, 1).as(in.ided(df.id, value))
                    }
                }
                .map(Left(_))
            case BatchResolver(batcher, partition) =>
              val impl = schemaState.batchers(batcher.id)

              inputs
                .parTraverse(in => partition(in.value).map(b => (in.meta.ided(df.id), b)))
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
            case SignalResolver(resolver, hd, tl) =>
              inputs
                .parTraverse { in =>
                  val subscribeF =
                    signalSubmission.traverse_ { alg =>
                      tl(in.value).flatMap { dst =>
                        // close in the initial value for tail Resorver[F, (I, T), A]
                        val df2 = df.copy(resolve = resolver.contramap[Any]((in.value, _)))
                        alg.add(in.meta.absolutePath, in.value, df2, dst.ref, dst.key)
                      }
                    }

                  subscribeF >>
                    // close in the initial value for head
                    hd(in.value).map(v => in.copy(value = (in.value, v)))
                }
                .flatMap { nvs =>
                  // we tupled the initial into nvs (head), cast since expects (Any, Any) but NodeValue contains Any
                  val df2 = df.copy(resolve = resolver.asInstanceOf[Resolver[F, Any, Any]])
                  collapseInputs(df2, nvs)
                }
          }
        }

        def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: List[NodeValue]): F[List[NodeValue]] =
          dfs.toList.parFlatTraverse {
            case PreparedFragField(id, specify, selection) =>
              runFields(selection.fields, in.flatMap(x => specify(x.value).toList.map(y => x.setValue(y))))
            case df @ PreparedDataField(id, name, _, _, _) => runDataField(df, in)
          }

        def startNext(df: PreparedDataField[F, Any, Any], outputs: List[NodeValue]): F[List[NodeValue]] = {
          def evalSel(s: Prepared[F, Any], in: List[NodeValue]): F[List[NodeValue]] = F.defer {
            s match {
              case PreparedLeaf(_, _) => F.pure(in)
              // F.fromEither(in.traverse(nv => enc(nv.value).leftMap(e => new Exception(e)).map(nv.setValue(_))))
              case Selection(fields) => runFields(fields, in)
              case PreparedList(of) =>
                val partitioned =
                  in.flatMap { nv =>
                    val inner = nv.value.asInstanceOf[Seq[Any]].toList

                    nv.index(inner)
                  }
                evalSel(of, partitioned)
            }
          }

          evalSel(df.selection, outputs)
        }

        def runDataField(df: PreparedDataField[F, Any, Any], input: List[NodeValue]): F[List[NodeValue]] = F.defer {
          // maybe join
          submitAndMaybeStart(df.id, input).flatMap {
            // No batching state, so just start
            case NoState =>
              val batchSize = input.size
              collapseInputs(df, input).flatMap {
                case Left(value) => startNext(df, value)
                case Right(value) =>
                  val keys = value.inputs

                  value
                    .resolve(keys.map { case (_, k) => k }.toSet)
                    .flatMap(outM => startNext(df, keys.map { case (c, k) => NodeValue(c, outM(k)) }))
              }
            // join
            // There is a batch state, but we didn't add the final input
            // Stop here, someone else will start the batch
            case NotFinalSubmission => F.pure(Nil)
            // join
            // We are the final submitter, start the computation
            case FinalSubmission(inputs) =>
              val inputLst = inputs.toList

              inputLst
                .map { case (k, v) => executionDeps.dataFieldMap(k) -> v }
                .parTraverse { case (df, v) => collapseInputs(df, v).map(df -> _) }
                .map(_.partitionEither { case (df, e) => e.bimap((df, _), (df, _)) })
                .flatMap { case (trivial, batched) =>
                  val trivialF = trivial.parFlatTraverse { case (df, fas) => startNext(df, fas) }

                  val batchedF =
                    batched.toNel
                      .traverse { nel =>
                        val (_, b) = nel.head

                        val keys = nel.toList.flatMap { case (_, bn) => bn.inputs.flatMap { case (_, b) => b.keys } }.toSet

                        b.resolve(keys).flatMap { resultLookup =>
                          nel.toList.parFlatTraverse { case (df, bn) =>
                            val mappedValuesF = bn.inputs.parTraverse { case (c, b) =>
                              b.post(b.keys.map(k => k -> resultLookup(k)))
                                .map(res => NodeValue(c, res))
                            }

                            mappedValuesF.flatMap(startNext(df, _))
                          }
                        }
                      }
                      .map(_.toList.flatten)

                  // fork each node's continuation
                  // in parallel, start every node's computation
                  (trivialF, batchedF).parMapN(_ ++ _)
                }
          }
        }

        rootSel.toList.parFlatTraverse { case (field, inputs) =>
          runFields(NonEmptyList.one(field), inputs)
        }
      }
    }

  def interpret2[F[_]](
      rootSel: NonEmptyList[(PreparedField[F, Any], Chain[EvalNode])],
      executionDeps: Batching[F],
      schemaState: SchemaState[F],
      signalSubmission: Option[SignalMetadataAccumulator[F]] = None
  )(implicit F: Async[F], stats: Statistics[F]): WriterT[F, Chain[EvalFailure], Chain[EvalNode]] = {
    type W[A] = WriterT[F, Chain[EvalFailure], A]
    val W = Async[W]
    val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]
    def fail[A](e: EvalFailure): W[Chain[A]] = WriterT.put(Chain.empty[A])(Chain.one(e))

    Supervisor[W].use { sup =>
      executionDeps.batchExecutionState[W].flatMap { batchStateMap =>
        def submitAndMaybeStart(nodeId: Int, input: Chain[EvalNode]): W[StateSubmissionOutcome] =
          batchStateMap
            .get(nodeId)
            .traverse(_.modify { s =>
              val newSet = s.remainingInputs - nodeId
              val newMap = s.inputMap + (nodeId -> null)
              val newState = BatchExecutionState(newSet, newMap)
              (newState, if (newSet.isEmpty && s.remainingInputs.nonEmpty) FinalSubmission(newMap) else NotFinalSubmission)
            })
            .map(_.getOrElse(NoState))

        type BatchKey = Any

        final case class NodeBatch(
            inputs: Chain[(NodeMeta, Batch[F, BatchKey, Any, Any])],
            resolve: Set[BatchKey] => W[Map[BatchKey, Any]]
        )

        def submit(name: String, duration: FiniteDuration, size: Int): W[Unit] =
          sup.supervise(WriterT.liftF(stats.updateStats(name, duration, size))).void

        def runInputs(
            df: PreparedDataField[F, Any, Any],
            inputs: Chain[EvalNode]
        ): W[Either[Chain[EvalNode], NodeBatch]] = {
          val n = executionDeps.nodeMap(df.id)

          df.resolve match {
            case EffectResolver2(resolve) =>
              inputs
                .parFlatTraverse { in =>
                  val next = in.meta.ided(df.id)

                  lift(resolve(in.value)).timed
                    .flatMap {
                      case (_, Left(e)) => fail[EvalNode](EvalFailure(Chain(next), Some(e), "during effect resolution", None))
                      case (dur, Right(x)) =>
                        val out = Chain(EvalNode(next, x))
                        submit(n.name, dur, 1).as(out)
                    }
                    .handleErrorWith(e => fail(EvalFailure(Chain(next), None, "during effect resolution", Some(e))))
                }
                .map(Left(_))
            case BatchResolver(batcher, partition) =>
              val impl = schemaState.batchers(batcher.id)

              val partitioned: W[Chain[(NodeMeta, Batch[F, Any, Any, Any])]] = inputs.parFlatTraverse { in =>
                val next = in.meta.ided(df.id)

                lift(partition(in.value))
                  .map(b => Chain((next, b)))
                  .handleErrorWith(e => fail(EvalFailure(Chain(next), None, "during partitioning", Some(e))))
              }

              partitioned
                .map { zs =>
                  Right(
                    NodeBatch(
                      zs,
                      xs =>
                        lift(impl(xs)).timed
                          .flatMap { case (dur, value) =>
                            submit(Planner.makeBatchName(batcher), dur, xs.size) >> submit(n.name, dur, xs.size).as(value)
                          }
                    )
                  )
                }
            case SignalResolver(resolver, hd, tl) =>
              inputs
                .parFlatTraverse { in =>
                  val hdF =
                    lift(hd(in.value)).map(v => Chain(in.copy(value = (in.value, v))))

                  def failHeadF(e: Throwable) =
                    fail[EvalNode](EvalFailure(Chain(in.meta), None, "during signal head resolution", Some(e)))

                  signalSubmission match {
                    case None => hdF.handleErrorWith(failHeadF)
                    case Some(ss) =>
                      lift(tl(in.value)).attempt
                        .flatMap {
                          case Left(e) =>
                            fail[EvalNode](EvalFailure(Chain(in.meta), None, "during signal tail resolution", Some(e)))
                          case Right(dst) =>
                            // close in the initial value for tail Resolver[F, (I, T), A]
                            val df2 = df.copy(resolve = resolver.contramap[Any]((in.value, _)))
                            lift(ss.add(in.meta.absolutePath, in.value, df2, dst.ref, dst.key)).flatMap { id =>
                              // if hd fails, then unsubscribe again
                              hdF.handleErrorWith(e => lift(ss.remove(id)) >> failHeadF(e))
                            }
                        }
                  }
                }
                .flatMap { nvs =>
                  // we tupled the initial into nvs (head), cast since expects (Any, Any) but EvalNode contains Any
                  val df2 = df.copy(resolve = resolver.asInstanceOf[Resolver[F, Any, Any]])
                  runInputs(df2, nvs)
                }
          }
        }

        def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode]): W[Chain[EvalNode]] =
          Chain.fromSeq(dfs.toList).parFlatTraverse {
            case PreparedFragField(id, specify, selection) =>
              runFields(selection.fields, in.flatMap(x => Chain.fromOption(specify(x.value)).map(y => x.setValue(y))))
            case df @ PreparedDataField(id, name, _, _, _) => runDataField(df, in)
          }

        def startNext(df: PreparedDataField[F, Any, Any], outputs: Chain[EvalNode]): W[Chain[EvalNode]] = {
          def evalSel(s: Prepared[F, Any], in: Chain[EvalNode]): W[Chain[EvalNode]] = W.defer {
            s match {
              case PreparedLeaf(_, _) => W.pure(in)
              // F.fromEither(in.traverse(nv => enc(nv.value).leftMap(e => new Exception(e)).map(nv.setValue(_))))
              case Selection(fields) => runFields(fields, in)
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

        def runDataField(df: PreparedDataField[F, Any, Any], input: Chain[EvalNode]): W[Chain[EvalNode]] = W.defer {
          // maybe join
          submitAndMaybeStart(df.id, input).flatMap {
            // No batching state, so just start
            case NoState =>
              runInputs(df, input)
                .flatMap[Chain[EvalNode]] {
                  case Left(xs) => W.pure(xs)
                  case Right(nb) =>
                    nb
                      .resolve(nb.inputs.map { case (_, k) => k }.toIterable.toSet)
                      .flatMap { outM =>
                        nb.inputs.flatTraverse { case (m, batch) =>
                          Chain.fromSeq(batch.keys).flatTraverse { k =>
                            outM.get(k) match {
                              case None    => fail(EvalFailure(Chain(m), None, "during batch resolution", None))
                              case Some(x) => W.pure(Chain(EvalNode(m, x)))
                            }
                          }
                        }
                      }
                }
                .flatMap(startNext(df, _))
            // join
            // There is a batch state, but we didn't add the final input
            // Stop here, someone else will start the batch
            case NotFinalSubmission => W.pure(Chain.empty)
            // join
            // We are the final submitter, start the computation
            case FinalSubmission(inputs) =>
              val in = inputs.toList.asInstanceOf[Chain[(Int, Chain[EvalNode])]]

              val inputLst = in

              inputLst
                .map { case (k, v) => executionDeps.dataFieldMap(k) -> v }
                .parTraverse { case (df, v) => runInputs(df, v).map(df -> _) }
                .map(_.partitionEither { case (df, e) => e.bimap((df, _), (df, _)) })
                .flatMap { case (trivial, batched) =>
                  val trivialF = trivial.parFlatTraverse { case (df, fas) => startNext(df, fas) }

                  val batchedF =
                    NonEmptyChain
                      .fromChain(batched)
                      .traverse { nec =>
                        val (_, b) = nec.head

                        val keys = nec.toChain
                          .flatMap { case (_, bn) => bn.inputs.flatMap { case (_, b) => Chain.fromSeq(b.keys) } }
                          .toIterable
                          .toSet

                        b
                          .resolve(keys)
                          .flatMap { resultLookup =>
                            nec.toChain.parFlatTraverse { case (df, bn) =>
                              val mappedValuesF = bn.inputs.parFlatTraverse { case (c, b) =>
                                val lookupsF = Chain.fromSeq(b.keys).flatTraverse { k =>
                                  resultLookup.get(k) match {
                                    case None    => fail[(Any, Any)](EvalFailure(Chain(c), None, "during batch resolution", None))
                                    case Some(x) => W.pure(Chain(k -> x))
                                  }
                                }

                                lookupsF.flatMap { keys =>
                                  lift(b.post(keys.toList).map(res => Chain(EvalNode(c, res))))
                                    .handleErrorWith(e =>
                                      fail[EvalNode](EvalFailure(Chain(c), None, "during batch post-resolution", Some(e)))
                                    )
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
        }

        Chain.fromSeq(rootSel.toList).parFlatTraverse { case (field, inputs) =>
          runFields(NonEmptyList.one(field), inputs)
        }
      }
    }
  }

  def reconstructField[F[_]](p: Prepared[F, Any], cursors: List[(Cursor, Any)]): ValidatedNec[String, Json] =
    p match {
      case PreparedLeaf(name, encode) =>
        cursors match {
          case (_, x) :: Nil => encode(x).toValidatedNec
          case _             => ???
        }
      case PreparedList(of) =>
        groupNodeValues(cursors).toList
          .traverse {
            case (Index(_), tl) => reconstructField[F](of, tl)
            case _              => ???
          }
          .map(Json.fromValues)
      case Selection(fields) => reconstructSelection(cursors, fields).map(_.asJson)
    }

  def reconstructSelection[F[_]](
      levelCursors: List[(Cursor, Any)],
      sel: NonEmptyList[PreparedField[F, Any]]
  ): ValidatedNec[String, JsonObject] = {
    val m = groupNodeValues(levelCursors)

    sel
      .traverse { pf =>
        pf match {
          case PreparedFragField(id, specify, selection) =>
            reconstructSelection(levelCursors, selection.fields)
          case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
            reconstructField(selection, m(Ided(id))).map(x => JsonObject(name -> x))
        }
      }
      .map(_.reduceLeft(_ deepMerge _))
  }

  def reconstruct[F[_]](rootSel: NonEmptyList[PreparedField[F, Any]], values: List[NodeValue])(implicit
      F: MonadThrow[F]
  ): F[JsonObject] = {
    def unpackPrep(df: PreparedDataField[F, Any, Any], cursors: List[(Cursor, Any)], p: Prepared[F, Any]): F[Json] =
      p match {
        case PreparedLeaf(name, encode) =>
          cursors match {
            case (_, x) :: Nil => F.fromEither(encode(x).leftMap(x => new Exception(x)))
            case _ => F.raiseError(new Exception(s"expected a single value at at ${df.name} (${df.id}), but got ${cursors.size}"))
          }
        case PreparedList(of) =>
          groupNodeValues(cursors).toList
            .traverse {
              case (Index(_), tl) => unpackPrep(df, tl, of)
              case (hd, _)        => F.raiseError[Json](new Exception(s"expected index at list in ${df.name} (${df.id}), but got $hd"))
            }
            .map(Json.fromValues)
        case Selection(fields) => unpackCursor(cursors, fields).map(_.asJson)
      }

    def unpackCursor(
        levelCursors: List[(Cursor, Any)],
        sel: NonEmptyList[PreparedField[F, Any]]
    ): F[JsonObject] = {
      val m = groupNodeValues(levelCursors)

      sel
        .traverse { pf =>
          pf match {
            case PreparedFragField(id, specify, selection) =>
              unpackCursor(levelCursors, selection.fields)
            case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
              unpackPrep(df, m(Ided(id)), selection).map(x => JsonObject(name -> x))
          }
        }
        .map(_.reduceLeft(_ deepMerge _))
    }

    unpackCursor(values.map(nv => nv.meta.relativePath -> nv.value), rootSel)
  }
}
