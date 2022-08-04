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

object Interpreter {
  object Naive {
    def interpretPrep[F[_]](input: Any, prep: Prepared[F, Any])(implicit F: Async[F]): F[Json] = {
      prep match {
        case Selection(fields) => interpret[F](input, fields).map(_.reduceLeft(_ deepMerge _).asJson)
        case PreparedLeaf(_, encode) =>
          encode(input) match {
            case Left(value)  => F.raiseError(new Exception(value))
            case Right(value) => F.pure(value)
          }
        case PreparedList(of) =>
          val inputLst = input.asInstanceOf[Vector[Any]]
          inputLst.traverse(i => interpretPrep[F](i, of)).map(_.reduceLeft(_ deepMerge _).asJson)
      }
    }

    def interpretField[F[_]](input: Any, pf: PreparedField[F, Any])(implicit F: Async[F]): F[JsonObject] = {
      pf match {
        case PreparedDataField(_, name, resolve, selection, _) =>
          val fa: F[Any] = ??? /*resolve match {
            case Output.Fields.PureResolution(r)     => F.pure(r(input))
            case Output.Fields.DeferredResolution(r) => r(input)
          }*/

          fa
            .flatMap(i => interpretPrep[F](i, selection))
            .map(x => JsonObject(name -> x))
        case PreparedFragField(_, specify, Selection(fields)) =>
          specify(input) match {
            case None    => F.pure(JsonObject.empty)
            case Some(i) => interpret(i, fields).map(_.reduceLeft(_ deepMerge _))
          }
      }
    }

    def interpret[F[_]](input: Any, s: NonEmptyList[PreparedField[F, Any]])(implicit F: Async[F]) =
      s.traverse(pf => interpretField[F](input, pf))
  }

  object Planned {
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

    // sealed trait FetchM[F[_], A] {
    //   def run: F[A] = ???
    // }
    // final case class Batch[F[_], I, O](keys: Map[String, Set[I]], resolve: Set[I] => F[O]) extends FetchM[F, O]
    // object FetchM {
    //   def g[F[_]](implicit F: Applicative[F]) = {
    //     type H[A] = FetchM[F, A]
    //     new Applicative[FetchM[F, *]] {
    //       override def ap[A, B](ff: FetchM[F, A => B])(fa: FetchM[F, A]): FetchM[F, B] =
    //         (ff, fa) match {
    //           case (Batch(fkeys, fresolve), Batch(akeys, aresolve)) =>
    //             val comb = Semigroup[Map[String, Set[Any]]].combine(fkeys, akeys)

    //             comb
    //         }
    //       // Batch(ff.keys ++ fa.keys, )
    //       ???

    //       override def pure[A](x: A): FetchM[F, A] =
    //         Batch[F, Any, A](Set.empty, _ => F.pure(x))
    //     }
    //   }
    // }

    def run[F[_]](
        rootInput: Any,
        rootSel: NonEmptyList[PreparedField[F, Any]],
        plan: NonEmptyList[Optimizer.Node]
    )(implicit F: Async[F], stats: Statistics[F]) =
      Supervisor[F].use { sup =>
        val flat = Optimizer.flattenNodeTree(plan)

        def unpackPrep(prep: Prepared[F, Any]): List[(Int, PreparedDataField[F, Any, Any])] =
          prep match {
            case PreparedLeaf(_, _) => Nil
            case PreparedList(of)   => unpackPrep(of)
            case Selection(fields)  => flattenDataFieldMap(fields).toList
          }

        def flattenDataFieldMap(sel: NonEmptyList[PreparedField[F, Any]]): NonEmptyList[(Int, PreparedDataField[F, Any, Any])] =
          sel.flatMap { pf =>
            pf match {
              case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
                val hd = id -> df
                val tl = unpackPrep(selection)
                NonEmptyList(hd, tl)
              case PreparedFragField(_, _, selection) => flattenDataFieldMap(selection.fields)
            }
          }

        val dataFieldMap = flattenDataFieldMap(rootSel).toList.toMap

        val batches: Map[Int, (String, NonEmptyList[Optimizer.Node])] =
          flat
            .groupBy(_.end)
            .toList
            .zipWithIndex
            .flatMap { case ((_, group), idx) =>
              group
                .groupBy(_.name)
                .filter { case (_, nodes) => nodes.size > 1 }
                .toList
                .flatMap { case (nodeType, nodes) =>
                  val thisBatch = (s"$nodeType-$idx", nodes)
                  nodes.map(n => n.id -> thisBatch).toList
                }
            }
            .toMap

        val batchStateMapF: F[Map[String, Ref[F, BatchExecutionState]]] =
          batches.values.toList
            .distinctBy { case (k, _) => k }
            .traverse { case (k, nodes) =>
              F.ref(BatchExecutionState(nodes.map(_.id).toList.toSet, Map.empty[Int, List[NodeValue]])).map(k -> _)
            }
            .map(_.toMap)

        val forkJoinResultF: F[List[NodeValue]] =
          batchStateMapF.flatMap { batchStateMap =>
            def getBatchState(nodeId: Int): Option[Ref[F, BatchExecutionState]] =
              batches.get(nodeId).flatMap { case (k, _) => batchStateMap.get(k) }

            def submitAndMaybeStart(nodeId: Int, input: List[NodeValue]): F[StateSubmissionOutcome] =
              getBatchState(nodeId)
                .traverse(_.modify { s =>
                  val newSet = s.remainingInputs - nodeId
                  val newMap = s.inputMap + (nodeId -> input)
                  val newState = BatchExecutionState(newSet, newMap)
                  (newState, if (newSet.isEmpty && s.remainingInputs.nonEmpty) FinalSubmission(newMap) else NotFinalSubmission)
                })
                .map(_.getOrElse(NoState))

            // def collapseInputs2(df: PreparedDataField[F, Any, Any], inputs: List[NodeValue]) = {
            //   val (batches, individuals) = inputs
            //     .map(nv => (nv.cursor.ided(df.id), df.resolve(nv.value)))
            //     .partitionEither {
            //       case (c, PureResolution(value))           => Left(F.pure(NodeValue(c, value)))
            //       case (c, DeferredResolution(fa))          => Left(fa.map(a => NodeValue(c, a)))
            //       case (c, BatchedResolution(key, resolve)) => Right((key, resolve))
            //     }
            // }

            def collapseInputs(df: PreparedDataField[F, Any, Any], inputs: List[NodeValue]): F[List[NodeValue]] =
              inputs
                .traverse { nv =>
                  val fb: F[Any] =
                    df.resolve match {
                      case DeferredResolution(r) => r(nv.value)
                      case PureResolution(r)     => F.pure(r(nv.value))
                      // case BatchedResolution(key, resolve) => resolve(Set(key)).map(_.values.head)
                    }

                  fb.map(b => nv.ided(df.id, b))
                }

            def startNext(df: PreparedDataField[F, Any, Any], outputs: List[NodeValue]): F[List[NodeValue]] = {
              def evalSel(s: Prepared[F, Any], in: List[NodeValue]): F[List[NodeValue]] =
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

              evalSel(df.selection, outputs)
            }

            def go(sel: NonEmptyList[PreparedField[F, Any]], input: List[NodeValue]): F[List[NodeValue]] =
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
                      collapseInputs(df, input).timed
                        .flatMap { case (dur, out) =>
                          sup.supervise(stats.updateStats(batchName, dur, batchSize)) >>
                            startNext(df, out)
                        }
                    // join
                    // There is a batch state, but we didn't add the final input
                    // Stop here, someone else will start the batch
                    case NotFinalSubmission => F.pure(Nil)
                    // join
                    // We are the final submitter, start the computation
                    case FinalSubmission(inputs) =>
                      val inputLst = inputs.toList
                      // outer list are seperate node ids, inner is is the list of results for that node
                      val outputsF: F[List[(PreparedDataField[F, Any, Any], List[NodeValue])]] =
                        inputLst
                          .map { case (k, v) => dataFieldMap(k) -> v }
                          .traverse { case (df, v) => collapseInputs(df, v).map(df -> _) }

                      val batchSize = inputLst.map { case (_, xs) => xs.size }.sum

                      // fork each node's continuation
                      // in parallel, start every node's computation
                      outputsF.timed
                        .flatMap { case (dur, res) =>
                          sup.supervise(stats.updateStats(batchName, dur, batchSize)) >>
                            res.parFlatTraverse { case (df, outputs) => startNext(df, outputs) }
                        }
                  }
              }

            go(rootSel, List(NodeValue(Cursor(Vector.empty), rootInput)))
          }

        forkJoinResultF.flatMap { res =>
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
                    case (hd, _) => F.raiseError[Json](new Exception(s"expected index at list in ${df.name} (${df.id}), but got $hd"))
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

          unpackCursor(res.map(nv => nv.cursor.path -> nv.value), rootSel)
        }
      }
  }
}
