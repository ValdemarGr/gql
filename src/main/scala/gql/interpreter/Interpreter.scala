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
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  ): F[NonEmptyList[JsonObject]] =
    for {
      costTree <- Optimizer.costTree[F](rootSel)
      plan = Optimizer.plan(costTree)
      executionDeps = planExecutionDeps[F](rootSel, plan)
      result <- interpret[F](rootSel.map((_, List(NodeValue.empty(rootInput)))), executionDeps, schemaState)
      output <- reconstruct[F](rootSel, result)
    } yield output

  def run[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      plan: NonEmptyList[Optimizer.Node],
      schemaState: SchemaState[F]
  ): F[NonEmptyList[JsonObject]] =
    interpret[F](rootSel.map((_, List(NodeValue.empty(rootInput)))), planExecutionDeps[F](rootSel, plan), schemaState)
      .flatMap(reconstruct[F](rootSel, _))

  def runStreamed[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, NonEmptyList[JsonObject]] =
    SubscriptionSupervisor[F](schemaState).flatMap { implicit streamResouceAlg =>
      // first iteration
      fs2.Stream.eval(SignalMetadataAccumulator[F]).flatMap { submissionAlg =>
        val outputF =
          for {
            costTree <- Optimizer.costTree[F](rootSel)
            plan = Optimizer.plan(costTree)
            executionDeps = planExecutionDeps[F](rootSel, plan)
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
                                  costTree <- Optimizer.costTree[F](rootSel)
                                  plan = Optimizer.plan(costTree)
                                  executionDeps = planExecutionDeps[F](rootSel, plan)
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

  final case class ExecutionDeps[F[_]](
      nodeMap: Map[Int, Optimizer.Node],
      dataFieldMap: Map[Int, PreparedDataField[F, Any, Any]],
      batches: List[NonEmptyList[Int]]
  ) {
    def batchExecutionState[F[_]](implicit F: Concurrent[F]): F[Map[Int, Ref[F, BatchExecutionState]]] =
      batches
        .flatTraverse { batch =>
          val l = batch.toList
          F.ref(BatchExecutionState(l.toSet, Map.empty[Int, List[NodeValue]])).map(s => l.map(_ -> s))
        }
        .map(_.toMap)
  }

  def planExecutionDeps[F[_]](rootSel: NonEmptyList[PreparedField[F, Any]], plan: NonEmptyList[Optimizer.Node]): ExecutionDeps[F] = {
    val flat = Optimizer.flattenNodeTree(plan)

    def unpackPrep(prep: Prepared[F, Any]): Eval[List[(Int, PreparedDataField[F, Any, Any])]] = Eval.defer {
      prep match {
        case PreparedLeaf(_, _) => Eval.now(Nil)
        case PreparedList(of)   => unpackPrep(of)
        case Selection(fields)  => flattenDataFieldMap(fields).map(_.toList)
      }
    }

    def flattenDataFieldMap(sel: NonEmptyList[PreparedField[F, Any]]): Eval[NonEmptyList[(Int, PreparedDataField[F, Any, Any])]] =
      Eval.defer {
        sel.flatTraverse { pf =>
          pf match {
            case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
              val hd = id -> df
              unpackPrep(selection).map(tl => NonEmptyList(hd, tl))
            case PreparedFragField(_, _, selection) => flattenDataFieldMap(selection.fields)
          }
        }
      }

    val nodeMap: Map[Int, Optimizer.Node] = flat.toList.map(x => x.id -> x).toMap

    val dataFieldMap: Map[Int, PreparedDataField[F, Any, Any]] = flattenDataFieldMap(rootSel).value.toList.toMap

    val batches: List[NonEmptyList[Int]] =
      flat
        .groupBy(_.end)
        .toList
        .zipWithIndex
        .flatMap { case ((_, group), idx) =>
          group
            .groupBy(_.batchName)
            .filter { case (o, nodes) => nodes.size > 1 && o.isDefined }
            .toList
            .map { case (nodeType, nodes) => nodes.map(_.id) }
        }

    ExecutionDeps(nodeMap, dataFieldMap, batches)
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
      executionDeps: ExecutionDeps[F],
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
                            submit(s"batch_${batcher.id}", dur, xs.size) >> submit(n.name, dur, xs.size).as(value)
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
                        alg
                          .add(in.cursor.ided(df.id), in.value, df.copy(resolve = resolver.contramap[Any]((in.value, _))), dst.ref, dst.key)
                      }
                    }

                  subscribeF >>
                    hd(in.value).map(v => in.copy(value = (in.value, v)))
                }
                .flatMap(nvs => collapseInputs(df.copy(resolve = resolver.asInstanceOf[Resolver[F, Any, Any]]), nvs))
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
