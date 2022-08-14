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

object Interpreter {
  sealed trait SignalSubscriptionAlg[F[_], A] {
    def add(initialValue: A, tail: fs2.Stream[F, A]): F[BigInt]

    def currentValue(id: BigInt): F[A]

    def signal(id: BigInt): F[Signal[F, A]]

    def remove(id: BigInt): F[Unit]
  }

  object SignalSubscriptionAlg {
    def apply[F[_], A](implicit F: Concurrent[F]) = {
      final case class SubscriptionState(
          nextId: BigInt,
          subscriptions: Map[BigInt, (Signal[F, A], Cleanup)]
      )

      type Cleanup = F[Unit]

      Resource.eval(F.ref(SubscriptionState(BigInt(1), Map.empty))).flatMap { state =>
        val cleanupR: Resource[F, Unit] =
          Resource.make(F.unit)(_ => state.get.flatMap(_.subscriptions.toList.traverse_ { case (_, (_, cleanup)) => cleanup }))

        new SignalSubscriptionAlg[F, A] {
          override def add(initialValue: A, tail: fs2.Stream[F, A]): F[BigInt] =
            tail
              .holdResource(initialValue)
              .allocated
              .flatMap { case (sig, release) =>
                state.modify { s =>
                  val nextId = s.nextId
                  (SubscriptionState(nextId + 1, s.subscriptions + (nextId -> (sig, release))), nextId)
                }
              }

          override def currentValue(id: BigInt): F[A] =
            signal(id).flatMap(_.get)

          override def signal(id: BigInt): F[Signal[F, A]] =
            state.get
              .map(_.subscriptions(id))
              .map { case (sig, _) => sig }

          override def remove(id: BigInt): F[Unit] =
            state.modify { s =>
              val (_, release) = s.subscriptions(id)
              s.copy(subscriptions = s.subscriptions - id) -> release
            }.flatten
        }
        Resource.unit[F]
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
