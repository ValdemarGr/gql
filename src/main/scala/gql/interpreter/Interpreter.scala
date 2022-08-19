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
  ): F[List[NodeValue]] = {
    val rootFields = rootSel.map { case (k, _) => k }
    for {
      costTree <- Planner.costTree[F](rootFields)
      plan = Planner.plan(costTree)
      executionDeps = Batching.plan[F](rootFields, plan)
      result <- interpret[F](rootSel, executionDeps, schemaState, signalAccum)
    } yield result
  }

  def run[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      plan: NonEmptyList[Planner.Node],
      schemaState: SchemaState[F]
  ): F[NonEmptyList[JsonObject]] =
    interpret[F](rootSel.map((_, List(NodeValue.empty(rootInput)))), Batching.plan[F](rootSel, plan), schemaState)
      .flatMap(reconstruct[F](rootSel, _))

  def runStreamed[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, NonEmptyList[JsonObject]] =
    SubscriptionSupervisor[F](schemaState).flatMap { implicit streamResouceAlg =>
      def runStreamIt(
          root: NonEmptyList[(PreparedField[F, Any], List[NodeValue])]
      ): F[(List[NodeValue], Map[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])])] =
        SignalMetadataAccumulator[F].flatMap { accumulator =>
          (planAndRun[F](root, schemaState, Some(accumulator)), accumulator.getState).tupled
        }

      // first iteration
      fs2.Stream
        .eval(runStreamIt(rootSel.map((_, List(NodeValue.empty(rootInput))))))
        .flatMap { case (initialNvs, initialSignals) =>
          fs2.Stream.eval(reconstruct(rootSel, initialNvs)).flatMap { initialOutput =>
            streamResouceAlg.changeLog
              .evalMapAccumulate((initialOutput, initialSignals)) { case ((prevOutput, activeSigs), changes) =>
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
                      rootNodes.map { case (id, input) =>
                        val (cursor, _, field) = activeSigs(id)
                        (field, List(NodeValue(cursor, input)))
                      }

                    prepaedRoots.toNel
                      .traverse { newRootSel =>
                        runStreamIt(newRootSel).map { case (newNvs, newSigs) =>
                          def associateRoots(
                              xs: List[(Vector[GraphPath], (PreparedDataField[F, Any, Any], Cursor))],
                              soFar: List[(Vector[GraphPath], Any)]
                          ): List[(PreparedDataField[F, Any, Any], Cursor, List[(Vector[GraphPath], Any)])] = {
                            val (nodeHere, iterates) = xs.partitionEither {
                              case (xs, y) if xs.isEmpty => Left(y)
                              case (xs, y)               => Right((xs, y))
                            }

                            val heres: List[(PreparedDataField[F, Any, Any], Cursor, List[(Vector[GraphPath], Any)])] =
                              nodeHere.map { case (pf, c) => (pf, c, soFar) }

                            val its =
                              if (iterates.nonEmpty) {
                                val sf = groupNodeValues(soFar)
                                val its = groupNodeValues(iterates)

                                val m =
                                  sf.alignWith(its) {
                                    case Ior.Both(l, r) => associateRoots(r, l)
                                    case _              => ???
                                  }

                                m.values.toList.flatten
                              } else {
                                Nil
                              }

                            its ++ heres
                          }

                          // TODO do some removal of subscriptions and in active sigs
                          // TODO merge new nvs and old json constructively
                          // newRootSel's cursors are a strict suprset of nvs
                          // strategically we can use newRootSel + newNvs to figure out
                          // the replacement cursor and the replacement value.
                          // Said another way;
                          // initial:
                          // List[NodeValue]
                          //
                          // by traversing the initial field's cursor
                          // NonEmptyList[(Cursor, PreparedDataField[F, Any, Any], List[NodeValue])]
                          //
                          // by converting the remaining (PreparedDataField[F, Any, Any], List[NodeValue]) to json
                          // via usual reconstruction
                          // NonEmptyList[(Cursor, Json)]
                          //
                          // by stitching every (Json at position Cursor) into the previous object
                          // NonEmptyList[JsonObject]
                          val newOutput = (prevOutput, newRootSel, newNvs)
                          ((initialOutput, newSigs ++ activeSigs), None)
                        }
                      }
                  }
                  .map(_.getOrElse(((initialOutput, activeSigs), None)))
              }
          }
        }

      fs2.Stream.eval(SignalMetadataAccumulator[F]).flatMap { submissionAlg =>
        val outputF =
          for {
            costTree <- Planner.costTree[F](rootSel)
            plan = Planner.plan(costTree)
            executionDeps = Batching.plan[F](rootSel, plan)
            result <- interpret[F](rootSel.map((_, List(NodeValue.empty(rootInput)))), executionDeps, schemaState, Some(submissionAlg))
            output <- reconstruct[F](rootSel, result)
          } yield output

        fs2.Stream
          .eval(outputF)
          .flatMap { initialOutput =>
            fs2.Stream.emit(initialOutput) ++
              fs2.Stream.eval(submissionAlg.getState).flatMap { initState =>
                streamResouceAlg.changeLog
                  .evalMapAccumulate(initState) { case (accum, next) =>
                    next
                      .filter { case (k, _) => accum.contains(k) }
                      .toNel
                      .flatTraverse { nextNel =>
                        val s = nextNel.toList.map { case (k, _) => k }.toSet
                        val nodes = accum.toList.map { case (k, (cursor, _, _)) => (cursor, k) }
                        val meta = computeMetadata(nodes, s)
                        val rootNodes: List[(BigInt, Any)] = nextNel.filter { case (k, _) => meta.hcsa.contains(k) }
                        val prepaedRoots =
                          rootNodes.map { case (id, input) =>
                            val (cursor, _, field) = accum(id)
                            (field, List(NodeValue.empty(input)))
                          }
                        prepaedRoots.toNel
                          .traverse { newRootSel =>
                            SignalMetadataAccumulator[F].flatMap { submissionAlg =>
                              val outputF =
                                for {
                                  costTree <- Planner.costTree[F](rootSel)
                                  plan = Planner.plan(costTree)
                                  executionDeps = Batching.plan[F](rootSel, plan)
                                  result <- interpret[F](newRootSel, executionDeps, schemaState, Some(submissionAlg))
                                  output <- reconstruct[F](newRootSel.map { case (k, _) => k }, result)
                                } yield output

                              val ns = submissionAlg.getState.map(m => m ++ accum)

                              (ns, outputF.map(Some(_))).parTupled
                            }
                          }
                      }
                      .map(_.getOrElse((accum, None)))
                  }
                  .collect { case (_, Some(x)) => x }
              }
          }
      }
    }

  def computeToRemove(nodes: List[(Vector[GraphPath], BigInt)], s: Set[BigInt]): Set[BigInt] =
    groupNodeValues(nodes).flatMap { case (k, tl) =>
      val (nodeHere, iterates) = tl.partitionEither {
        case (xs, y) if xs.isEmpty => Left(y)
        case (xs, y)               => Right((xs, y))
      }

      nodeHere match {
        case x :: Nil if s.contains(x) => iterates.map { case (_, v) => v }.toSet
        case x :: Nil                  => computeToRemove(iterates, s)
        case Nil                       => computeToRemove(iterates, s)
        case _                         => throw new Exception(s"something went terribly wrong $nodes and $s")
      }
    }.toSet

  final case class SignalRecompute(
      toRemove: Set[BigInt],
      hcsa: Set[BigInt]
  )

  def computeMetadata(nodes: List[(Cursor, BigInt)], s: Set[BigInt]): SignalRecompute = {
    val tr = computeToRemove(nodes.map { case (k, v) => (k.path, v) }, s)
    val hcsa = s -- tr
    SignalRecompute(tr, hcsa)
  }

  def groupNodeValues[A](nvs: List[(Vector[GraphPath], A)]): Map[GraphPath, List[(Vector[GraphPath], A)]] =
    nvs.groupMap { case (c, _) => c.head } { case (c, v) => (c.tail, v) }

  // It is assumed that if any node occurs in the path to the new subtree
  // then it occurs as a node in the old tree
  def stichInto(
      olds: List[(Vector[GraphPath], Any)],
      newSubtrees: List[(Vector[GraphPath], List[(Vector[GraphPath], Any)])]
  ): List[(Vector[GraphPath], Any)] = {
    val (finals, iterates) = newSubtrees.partitionEither {
      case (x +: xs, ys) if xs.isEmpty => Left((x, ys))
      case (xs, ys)                    => Right((xs, ys))
    }

    val finalMap = finals.toMap

    val iteratesMap = groupNodeValues(iterates)

    groupNodeValues(olds).toList.flatMap { case (pathName, oldPathTail) =>
      // Either this path is a final path; we stich it into this path
      val newChildren =
        finalMap.get(pathName) match {
          case Some(newSubtree) => newSubtree
          case None             =>
            // Or there is something on this path that we need to merge
            iteratesMap.get(pathName) match {
              case Some(newPaths) => stichInto(oldPathTail, newPaths)
              // Or maybe this path has nothing new
              case None => oldPathTail
            }
        }
      newChildren.map { case (p, v) => (pathName +: p, v) }
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
            inputs: List[(Cursor, Batch[F, BatchKey, Any, Any])],
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
                .parTraverse(in => partition(in.value).map(b => (in.cursor.ided(df.id), b)))
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
                        alg.add(in.cursor.ided(df.id), in.value, df2, dst.ref, dst.key)
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

        def startNext(df: PreparedDataField[F, Any, Any], outputs: List[NodeValue]): F[List[NodeValue]] = {
          def evalSel(s: Prepared[F, Any], in: List[NodeValue]): F[List[NodeValue]] = F.defer {
            s match {
              case PreparedLeaf(_, _) => F.pure(in)
              case Selection(fields)  => go(fields, in)
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

        def go(sel: NonEmptyList[PreparedField[F, Any]], input: List[NodeValue]): F[List[NodeValue]] = F.defer {
          // fork each field
          sel.toList.parFlatTraverse {
            case PreparedFragField(id, specify, selection) =>
              go(selection.fields, input.flatMap(x => specify(x.value).toList.map(res => x.ided(id, res))))
            case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
              // maybe join
              submitAndMaybeStart(id, input).flatMap {
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
        }

        rootSel.toList.parFlatTraverse { case (field, inputs) =>
          go(NonEmptyList.of(field), inputs)
        }
      }
    }

  def reconstruct[F[_]](rootSel: NonEmptyList[PreparedField[F, Any]], values: List[NodeValue])(implicit
      F: MonadThrow[F]
  ): F[NonEmptyList[JsonObject]] = {
    def unpackPrep(df: PreparedDataField[F, Any, Any], cursors: List[(Vector[GraphPath], Any)], p: Prepared[F, Any]): F[Json] =
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
        case Selection(fields) => unpackCursor(cursors, fields).map(_.reduceLeft(_ deepMerge _).asJson)
      }

    def unpackCursor(
        levelCursors: List[(Vector[GraphPath], Any)],
        sel: NonEmptyList[PreparedField[F, Any]]
    ): F[NonEmptyList[JsonObject]] = {
      val m: Map[GraphPath, List[(Vector[GraphPath], Any)]] = groupNodeValues(levelCursors)

      sel.traverse { pf =>
        pf match {
          case PreparedFragField(id, specify, selection) =>
            m.get(Ided(id)) match {
              case None       => F.pure(JsonObject.empty)
              case Some(frag) => unpackCursor(frag, selection.fields).map(_.reduceLeft(_ deepMerge _))
            }
          case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
            unpackPrep(df, m(Ided(id)), selection).map(x => JsonObject(name -> x))
        }
      }
    }

    unpackCursor(values.map(nv => nv.cursor.path -> nv.value), rootSel)
  }
}
