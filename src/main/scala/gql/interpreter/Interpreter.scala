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

  def stitchInto[F[_]](
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
            oldObj.add(name, stitchInto(oldObj(name).get, subTree, tl, nameMap)).asJson
          case Index(index) =>
            val oldArr = oldTree.asArray.get
            oldArr.updated(index, stitchInto(oldArr(index), subTree, tl, nameMap)).asJson
        }
    }

  def runStreamed[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, Json] =
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
            fs2.Stream.emit(initialOutput.asJson) ++
              streamResouceAlg.changeLog
                .evalScan((initialOutput.asJson, initialSignals)) { case ((prevOutput, activeSigs), changes) =>
                  // remove dead nodes (concurrent access can cause dead updates to linger)
                  changes
                    .filter { case (k, _) => activeSigs.contains(k) }
                    .toNel
                    .flatTraverse { activeChanges =>
                      val s = activeChanges.toList.map { case (k, _) => k }.toSet
                      val allSigNodes = activeSigs.toList.map { case (k, (cursor, _, _)) => (cursor, k) }
                      val meta = computeMetadata(allSigNodes, s)
                      val rootNodes: List[(BigInt, Any)] = activeChanges.filter { case (k, _) => meta.hcsa.contains(k) }
                      val prepaedRoots =
                        rootNodes.mapWithIndex { case ((id, input), idx) =>
                          val (cursor, _, field) = activeSigs(id)
                          // println(cursor)
                          (field, cursor, List(NodeValue(NodeMeta.startAt(cursor, BigInt(idx)), input)), idx)
                        }

                      prepaedRoots.toNel
                        .traverse { newRootSel =>
                          runStreamIt(newRootSel.map { case (df, _, inputs, _) => (df, inputs) }).flatMap { case (newNvs, _, newSigs) =>
                            val nameMap = deps.dataFieldMap.view.mapValues(_.name).toMap

                            val groupIdMapping =
                              newRootSel.toList.map { case (df, rootCursor, _, idx) => idx -> (rootCursor, df) }.toMap

                            val l: List[(BigInt, List[NodeValue])] = newNvs.groupBy(_.meta.stableId).toList

                            val recombinedF: F[Json] =
                              l
                                .parTraverse { case (group, results) =>
                                  val (rootCursor, df) = groupIdMapping(group.toInt)
                                  reconstruct[F](NonEmptyList.one(df), results).tupleRight(rootCursor)
                                }
                                .map(_.foldLeft(prevOutput) { case (accum, (obj, pos)) =>
                                  // println(s"stitchin into at pos $pos")
                                  // println(obj)
                                  // println(accum)
                                  stitchInto(accum, obj.asJson, pos, nameMap)
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
                    .map(_.getOrElse((initialOutput.asJson, activeSigs)))
                }
                .map { case (j, _) => j }
          }
        }
    }

  def computeToRemove(nodes: List[(Cursor, BigInt)], s: Set[BigInt]): Set[BigInt] = {
    import Chain._
    groupNodeValues(nodes).flatMap { case (k, tl) =>
      val (iterates, nodeHere) = tl
        .partitionEither {
          case (xs, y) if xs.path.isEmpty => Right(y)
          case (xs, y)                    => Left((xs, y))
        }

      lazy val msg =
        s"something went terribly wrong s=$s, k=$k, nodeHere:${nodeHere}\niterates:\n${iterates.mkString("\n")}\nall nodes:\n${nodes.mkString("\n")}"

      // println(msg)
      nodeHere match {
        case x :: Nil if s.contains(x) => iterates.map { case (_, v) => v }.toSet
        case x :: Nil                  => computeToRemove(iterates, s)
        case Nil                       => computeToRemove(iterates, s)
        case _                         => throw new Exception(msg)
      }
    }.toSet
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

  // Either there are multiple different paths or there is no more pathing
  def finalOrContinuation[A](nvs: List[(Cursor, A)]): Either[A, Map[GraphArc, List[(Cursor, A)]]] = {
    nvs match {
      case (c, v) :: Nil if c.path.isEmpty => Left(v)
      case xs                              => Right(groupNodeValues(xs))
    }
  }

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
              F.pure(Left(inputs.map(in => in.setValue(resolve(in.value)))))
            case EffectResolver(resolve) =>
              inputs
                .parTraverse { in =>
                  resolve(in.value).timed
                    .flatMap { case (dur, value) =>
                      submit(n.name, dur, 1).as(in.setValue(value))
                    }
                }
                .map(Left(_))
            case BatchResolver(batcher, partition) =>
              val impl = schemaState.batchers(batcher.id)

              inputs
                .parTraverse(in => partition(in.value).map(b => (in.meta, b)))
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
                        // println(s"adding sub for cursor ${in.meta.absolutePath}: $df2")
                        // TODO bug here maybe
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
            case df @ PreparedDataField(id, name, _, _, _) => 
              // println(s"adding $id by name $name to cursors:\n${in.map(_.meta).mkString("\n")}")
              runDataField(df, in.map(x => x.ided(id, x.value)))
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
            // m.get(Ided(id)) match {
            //   case None       => F.pure(JsonObject.empty)
            //   case Some(frag) => unpackCursor(frag, selection.fields)
            // }
            case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
              unpackPrep(df, m(Ided(id)), selection).map(x => JsonObject(name -> x))
          }
        }
        .map(_.reduceLeft(_ deepMerge _))
    }

    unpackCursor(values.map(nv => nv.meta.relativePath -> nv.value), rootSel)
  }
}
