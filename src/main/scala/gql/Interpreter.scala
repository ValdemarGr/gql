package gql

import cats.data._
import PreparedQuery._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import io.circe._
import io.circe.syntax._
import cats.Eval
import cats.Monad
import cats.effect.std.Supervisor
import scala.collection.immutable.SortedMap
import gql.Output.Fields.DeferredResolution
import gql.Output.Fields.PureResolution
import cats.Apply
import cats.Applicative
import cats.SemigroupK
import cats.kernel.Semigroup
import gql.Output.Fields.BatchedResolution
import scala.concurrent.duration.FiniteDuration
import cats.MonadThrow

object Interpreter {
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

  def planAndRun[F[_]: Async: Statistics](rootInput: Any, rootSel: NonEmptyList[PreparedField[F, Any]]): F[NonEmptyList[JsonObject]] =
    for {
      costTree <- Optimizer.costTree[F](rootSel)
      plan = Optimizer.plan(costTree)
      executionDeps = planExecutionDeps[F](rootSel, plan)
      result <- interpret[F](rootSel, rootSel, executionDeps)
      output <- reconstruct[F](rootSel, result)
    } yield output

  def run[F[_]: Async: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      plan: NonEmptyList[Optimizer.Node]
  ): F[NonEmptyList[JsonObject]] =
    interpret[F](rootSel, rootSel, planExecutionDeps[F](rootSel, plan))
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

  def interpret[F[_]](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      executionDeps: ExecutionDeps[F]
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

        final case class BatchKey(k: Any)

        final case class Batch(
            inputs: List[(Cursor, BatchKey)],
            resolve: Set[BatchKey] => F[Map[BatchKey, Any]]
        )

        def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
          sup.supervise(stats.updateStats(name, duration, size)).void

        def collapseInputs(
            df: PreparedDataField[F, Any, Any],
            inputs: List[NodeValue]
        ): F[Either[List[NodeValue], Batch]] = {
          val n = executionDeps.nodeMap(df.id)

          df.resolve match {
            case PureResolution(resolve) =>
              F.pure(Left(inputs.map(in => in.ided(df.id, resolve(in.value)))))
            case DeferredResolution(resolve) =>
              inputs
                .parTraverse { in =>
                  resolve(in.value).timed
                    .flatMap { case (dur, value) =>
                      submit(n.name, dur, 1).as(in.ided(df.id, value))
                    }
                }
                .map(Left(_))
            case BatchedResolution(batchName, key, resolve) =>
              inputs
                .parTraverse(in => key(in.value).map(x => (in.cursor.ided(df.id), BatchKey(x))))
                .map { zs =>
                  Right(
                    Batch(
                      zs,
                      xs =>
                        resolve(xs.map(_.k)).timed
                          .flatMap { case (dur, value) =>
                            val ys = value.map { case (k, v) => BatchKey(k) -> v }
                            submit(batchName, dur, ys.size) >>
                              submit(n.name, dur, ys.size).as(ys)
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
                    val inner = nv.value.asInstanceOf[Vector[Any]].toList

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

                            val keys = nel.toList.flatMap { case (_, b) => b.inputs.map { case (_, k) => k } }.toSet

                            b.resolve(keys).flatMap { resultLookup =>
                              nel.toList.parFlatTraverse { case (df, b) =>
                                val mappedValues = b.inputs.map { case (c, k) => NodeValue(c, resultLookup(k)) }
                                startNext(df, mappedValues)
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
