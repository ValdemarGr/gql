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

        val nodeMap = flat.toList.map(x => x.id -> x).toMap

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

        val batchStateMapF: F[Map[Int, Ref[F, BatchExecutionState]]] =
          flat
            .groupBy(_.end)
            .toList
            .zipWithIndex
            .flatTraverse { case ((_, group), idx) =>
              group
                .groupBy(_.batchName)
                .filter { case (o, nodes) => nodes.size > 1 && o.isDefined }
                .toList
                .flatTraverse { case (nodeType, nodes) =>
                  F.ref(BatchExecutionState(nodes.map(_.id).toList.toSet, Map.empty[Int, List[NodeValue]])).map { s =>
                    nodes.map(_.id).toList.map(_ -> s)
                  }
                }
            }
            .map(_.toMap)

        val forkJoinResultF: F[List[NodeValue]] =
          batchStateMapF.flatMap { batchStateMap =>
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
              val n = nodeMap(df.id)

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
                        .map { case (k, v) => dataFieldMap(k) -> v }
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
