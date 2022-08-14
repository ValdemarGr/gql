package gql

import gql.resolver._
import cats.data._
import PreparedQuery._
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

object Interpreter {
  trait SignalSubscriptionAlg[F[_]] {
    def subscribe(ref: StreamReference[Any, Any], key: Any): F[BigInt]

    def remove(id: BigInt): F[Unit]

    def changeLog: fs2.Stream[F, NonEmptyList[(BigInt, Any)]]
  }

  object SignalSubscriptionAlg {
    def apply[F[_]](ss: SchemaState[F])(implicit F: Concurrent[F]): Resource[F, SignalSubscriptionAlg[F]] = {
      type ResourceGroupId = Int
      type ResourceKey = Any
      type Cleanup = F[Unit]
      type Listeners = Int

      final case class SubscriptionState(
          nextId: BigInt,
          subscriptions: Map[BigInt, (ResourceGroupId, ResourceKey)],
          openResources: Map[(ResourceGroupId, ResourceKey), (Listeners, F[Either[Throwable, Cleanup]])]
      )

      val refR =
        Resource.make(F.ref(SubscriptionState(BigInt(1), Map.empty, Map.empty)))(
          _.get.flatMap(_.openResources.toList.parTraverse { case (_, (_, e)) => e.flatMap(_.sequence_) }.void)
        )

      Resource
        .eval(Queue.bounded[F, Chunk[NonEmptyList[(BigInt, Any)]]](1024))
        .flatMap { q =>
          refR.map { state =>
            def broadcastChanges(stream: fs2.Stream[F, NonEmptyList[(BigInt, Any)]]) =
              stream
                .enqueueUnterminatedChunks(q)
                .compile
                .drain
                .start

            def addStreamData(rgi: ResourceGroupId, k: ResourceKey, stream: fs2.Stream[F, Any])
                : fs2.Stream[F, NonEmptyList[(BigInt, Any)]] =
              stream
                .evalMap { x =>
                  state.get
                    .map(_.subscriptions.toList.collect { case (bd, (rgi2, k2)) if rgi2 == rgi && k2 == k => bd }.toNel)
                    .map(_.map((x, _)))
                }
                .unNone
                .map { case (x, nel) => nel.map((_, x)) }

            def backgroundStreamBroadcast(rgi: ResourceGroupId, k: ResourceKey, stream: fs2.Stream[F, Any]) =
              broadcastChanges(addStreamData(rgi, k, stream))

            def openStream(rgi: ResourceGroupId, k: ResourceKey): F[Either[Throwable, Cleanup]] = {
              val res = ss.streams(rgi)(k)

              res.allocated
                .flatMap { case (stream, cleanup) =>
                  backgroundStreamBroadcast(rgi, k, stream)
                    .map(_.cancel >> cleanup)
                }
                .attempt
                .flatTap {
                  case Left(_) => state.update(s2 => s2.copy(openResources = s2.openResources - ((rgi, k))))
                  case _       => F.unit
                }
            }

            // Maybe there is already a resource open for this type of stream?
            // Also, this is pretty volatile and unsafe, so no cancellation here
            def reserveResourceF(rgi: ResourceGroupId, k: ResourceKey): F[BigInt] = F.uncancelable { _ =>
              for {
                d <- F.deferred[Either[Throwable, Cleanup]]
                tryAlloc <- state.modify { s =>
                  val compositeKey = (rgi, k)
                  val (newMap, openF) =
                    s.openResources.get(compositeKey) match {
                      case Some((listeners, fa)) =>
                        val newEntry = (listeners + 1, fa)
                        (s.openResources + (compositeKey -> newEntry), fa)
                      case None =>
                        val open: F[Either[Throwable, Cleanup]] = openStream(rgi, k).flatTap(d.complete)

                        val newEntry = (1, d.get)

                        (s.openResources + (compositeKey -> newEntry), open)
                    }

                  val nextId = s.nextId + 1

                  (s.copy(openResources = newMap, nextId = nextId + 1), openF.map(_.as(nextId)))
                }
                o <- tryAlloc.rethrow
              } yield o
            }

            def releaseSubscriptionF(id: BigInt): F[Unit] =
              state.modify { s =>
                s.subscriptions.get(id) match {
                  case None => (s, F.unit)
                  case Some(ck) =>
                    s.openResources.get(ck) match {
                      case None => (s, F.unit)
                      case Some((listeners, result)) =>
                        if (listeners > 1) {
                          val newEntry = (listeners - 1, result)
                          (
                            s.copy(
                              openResources = s.openResources + (ck -> newEntry),
                              subscriptions = s.subscriptions - id
                            ),
                            F.unit
                          )
                        } else {
                          (
                            s.copy(
                              openResources = s.openResources - ck,
                              subscriptions = s.subscriptions - id
                            ),
                            result.flatMap(_.sequence_).void
                          )
                        }
                    }
                }
              }.flatten

            new SignalSubscriptionAlg[F] {
              override def subscribe(ref: StreamReference[Any, Any], key: Any): F[BigInt] =
                reserveResourceF(ref.id, key)

              override def remove(id: BigInt): F[Unit] =
                releaseSubscriptionF(id)

              override def changeLog: fs2.Stream[F, NonEmptyList[(BigInt, Any)]] =
                fs2.Stream.fromQueueUnterminatedChunk(q)
            }
          }
        }
    }
  }

  final case class Converted(batchId: String, idsContained: Map[Int, List[Converted]])

  final case class BatchExecutionState(remainingInputs: Set[Int], inputMap: Map[Int, List[NodeValue]])

  sealed trait GraphPath
  final case class Ided(id: Int) extends GraphPath
  final case class Index(index: Int) extends GraphPath

  final case class Cursor(path: Vector[GraphPath]) {
    def add(next: GraphPath): Cursor = Cursor(path :+ next)
    def index(idx: Int) = add(Index(idx))
    def ided(id: Int) = add(Ided(id))
  }

  final case class NodeValue(cursor: Cursor, value: Any) {
    def index(xs: List[Any]): List[NodeValue] =
      xs.zipWithIndex.map { case (x, i) => NodeValue(cursor.index(i), x) }
    def ided(id: Int, value: Any): NodeValue =
      NodeValue(cursor.ided(id), value)
  }

  sealed trait StateSubmissionOutcome
  final case class FinalSubmission(accumulatedInputs: Map[Int, List[NodeValue]]) extends StateSubmissionOutcome
  case object NoState extends StateSubmissionOutcome
  case object NotFinalSubmission extends StateSubmissionOutcome

  def planAndRun[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  ): F[NonEmptyList[JsonObject]] =
    for {
      costTree <- Optimizer.costTree[F](rootSel)
      plan = Optimizer.plan(costTree)
      executionDeps = planExecutionDeps[F](rootSel, plan)
      result <- interpret[F](rootSel, rootSel, executionDeps, schemaState)
      output <- reconstruct[F](rootSel, result)
    } yield output

  def run[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      plan: NonEmptyList[Optimizer.Node],
      schemaState: SchemaState[F]
  ): F[NonEmptyList[JsonObject]] =
    interpret[F](rootInput, rootSel, planExecutionDeps[F](rootSel, plan), schemaState)
      .flatMap(reconstruct[F](rootSel, _))

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

  /*
   * Stream strategy:
   *
   * Emit the values for "head" as F[List[NodeValue]]
   *
   * Emit resource of node updates as Resource[F, Stream[F, Chunk[(StreamId, Cursor, NodeValue)]]]
   *
   * Return them as F[(F[List[NodeValue]], Resource[F, Stream[F, Chunk[(StreamId, Cursor, NodeValue)]]])]
   * Where the outer F allocates all evaluation dependencies, the inner F evaluates all head values and
   * the inner resource opens all streams.
   *
   * For queries and mutations the resource can stay closed.
   *
   * For subscriptions the resource should be opened and for every chunk
   * the smallest common signal node parent should be found, such that if two nodes N1 and N2 are updated
   * and cursor(N1) > cursor(N2) then N2 is the maximal parent of { N1, N2 }, since an update for N2
   * also causes N1 to re-evaluate regardless.
   *
   * Any updated nodes that do not occur in smallest common signal nodes must be released.
   * (This warrents an unique identifier for each node)
   * There exists a time window where a node is no longer relevant but is still emitting updates.
   * The stream of updates should be filtered by "active" nodes.
   *
   * Next, de-duplicate all smallest common signal nodes types by id.
   * Let this set of de-duplicated smallest common signal nodes be the new root nodes.
   * Now create an optimized plan and re-evaluate this set of root nodes like an ordinary query.
   *
   * This will provide new List[NodeValue] wich must be merged with the previously emitted List[NodeValue],
   * discarding all nodes that are no longer relevant by always picking the new List[NodeValue]'s result.
   *
   * The new set of signals contained in the right resource, must be merged with the previous set of signals.
   * (Maybe perform resource closing here?)
   *
   * The Resource type definiton serves as a simple abstraction, but is not sufficient since release should
   * occur on a per-signal basis.
   */

  /*
   * Strategy 2:
   * Subscribe to the changeLog in the signal alg.
   *
   * Evaluate the initial plan, which results in List[NodeValue]
   * Emit the initial plan's result.
   *
   * For every changeLog chunk:
   *   Find the highest common signal ancestor of all changed nodes by:
   *     Find all nodes parents, let this be P(id): Set[NodeId]:
   *       Start from the root with S = {}.
   *       If node is a signal node, add the node's id to S and save the mapping id -> S.
   *       Recurse into children with the parameter S.
   *     The highest common signal ancestor set of the changed nodes is
   *     HCSA = { x | x \in S \land (P(x) \cap S = \emptyset) }
   *
   *     Algo 2:
   *       For every changed node N, let this be IC: List[Set[NodeId]]:
   *         For every child, find the first node that occurs in the changed nodes.
   *       The changed nodes - IC are the highest common signal ancestor nodes,
   *       since every node that occurs as a child is found and eliminated.
   *
   *   Remove the subscription for all children of the highest common signal ancestor nodes.
   *   
   *   Now, let the highest common signal ancestors be the new root nodes.
   *   Plan and evaluate the new root nodes.
   *
   *   Merge the new List[NodeValue] with the old List[NodeValue] by always picking the newest result.
   *   Emit this new result and save it for next iteration.
   */

  def interpret[F[_]](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      executionDeps: ExecutionDeps[F],
      schemaState: SchemaState[F]
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
                            submit(s"batch_${batcher.id}", dur, value.size) >>
                              submit(n.name, dur, value.size).as(value)
                          }
                    )
                  )
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
                      (trivialF, batchedF).mapN(_ ++ _)
                    }
              }
          }
        }

        go(rootSel, List(NodeValue(Cursor(Vector.empty), rootInput)))
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
          cursors
            .groupMap { case (k, _) => k.head } { case (k, v) => k.tail -> v }
            .toList
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
      val m: Map[GraphPath, List[(Vector[GraphPath], Any)]] = levelCursors
        .groupMap { case (c, _) => c.head } { case (c, v) => (c.tail, v) }
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
