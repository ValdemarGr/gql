/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql.server.interpreter

import cats._
import io.circe.syntax._
import cats.effect._
import cats.implicits._
import cats.data._
import io.circe._
import gql.server.planner._
import gql._
import gql.preparation._
import fs2.Stream
import scala.concurrent.duration.FiniteDuration
import cats.arrow.FunctionK
import cats.effect.std.Supervisor
import cats.effect.std.Queue
import fs2.concurrent.SignallingRef

/** The [[StreamInterpreter]] is resposible for:
  *   - Wireing together results for a query.
  *   - Handling incoming asynchronous events.
  *   - Handling resource lifetimes.
  *
  * For actual query excution, take a look at [[QueryInterpreter]].
  */

trait StreamInterpreter[F[_]] {
  import StreamInterpreter._

  def interpretStream0[A](
      root: A,
      selection: Selection[F, A],
      takeOne: Boolean = false,
      throttle: F ~> F = FunctionK.id[F]
  ): Resource[F, ResultStream[F]]

  def interpretStream[A](
      root: A,
      selection: Selection[F, A],
      takeOne: Boolean = false,
      throttle: F ~> F = FunctionK.id[F]
  ): Stream[F, Result]

  def interpretSync[A](
      root: A,
      selection: Selection[F, A],
      throttle: F ~> F = FunctionK.id[F]
  ): F[Result]
}

object StreamInterpreter {
  final case class AwaitEvents[F[_]](
      await: F[Option[ResultStream[F]]]
  )

  final case class ResultStream[F[_]](
      evalNext: F[(Result, AwaitEvents[F])]
  )

  final case class Result(
      errors: Chain[EvalFailure],
      data: JsonObject
  ) {
    val asQueryResult = QueryResult(data, errors.flatMap(_.asResult))
  }

  def apply[F[_]: Statistics: Planner](
      schemaState: SchemaState[F],
      accumulate: Option[FiniteDuration]
  )(implicit F: Async[F]): StreamInterpreter[F] = new StreamInterpreter[F] {
    override def interpretSync[A](root: A, selection: Selection[F, A], throttle: F ~> F): F[Result] =
      interpretStream(root, selection, takeOne = true, throttle).take(1).compile.lastOrError

    override def interpretStream0[A](
        root: A,
        selection: Selection[F, A],
        takeOne: Boolean = false,
        throttle: F ~> F = FunctionK.id[F]
    ): Resource[F, ResultStream[F]] = {
      case class StreamingApiState(
          awaitExecution: Deferred[F, Unit],
          entries: List[EvalState.Entry[F, ?]]
      )

      for {
        state <- Resource.eval {
          Deferred[F, Unit].flatMap(dx0 => Ref[F].of(StreamingApiState(dx0, Nil)))
        }
        newEntries <- Resource.eval(Queue.bounded[F, Unit](1))
        api = new StreamingApi[F] {
          def submit[B](cont: Continuation[F, B], node: EvalNode[F, B]): F[Unit] =
            state.modify { s =>
              (
                s.copy(entries = EvalState.Entry[F, B](cont, node) :: s.entries),
                newEntries.offer(()) *> s.awaitExecution.get
              )
            }.flatten
        }
        rootScope <- Res.make[F]
        sup <- Supervisor[F]
        counter <- Resource.eval(SignallingRef[F].of(0))
      } yield {
        val interpreter = QueryInterpreter[F](schemaState, throttle, sup, api, counter)

        def next(prev: JsonObject): F[Option[ResultStream[F]]] = {
          val exec = for {
            _ <- accumulate.traverse_(d => Temporal[F].sleep(d))
            doneExecNext <- Deferred[F, Unit]
            state <- state.getAndSet(StreamingApiState(doneExecNext, Nil))
            entries = state.entries
            (res: Option[ResultStream[F]]) <- rootScope
              .lease {
                entries.traverse { e =>
                  e.a.active.keepAlive.map {
                    case false => None
                    case true  => Some(e)
                  }
                }
              }
              .use {
                case None => F.canceled.as(Option.empty[ResultStream[F]]) // root dead, stop
                case Some((openedLease, opened)) =>
                  val opt = opened.collect { case Some(x) => x }.toNel.map { relevant =>
                    val inputs = relevant.map { case (entr: EvalState.Entry[F, a]) =>
                      QueryInterpreter.Input[F, a](entr.cont, entr.a)
                    }

                    ResultStream[F] {
                      interpreter.interpretAll(inputs).flatMap { results =>
                        val patched = results.data.toList.foldLeftM(prev) { case (accum, (pos, patch)) =>
                          F.fromEither {
                            stitchInto(accum.asJson, patch, pos, Cursor.empty).value.value
                          }.map(_.asObject.get)
                        }

                        rootScope.release(openedLease) *>
                          state.awaitExecution.complete(()) *>
                          patched.map { jo =>
                            (
                              Result(results.errors, jo),
                              AwaitEvents(next(jo))
                            )
                          }
                      }
                    }
                  }

                  F.pure(opt)
              }
          } yield res

          val r = F.race(
            counter.discrete.find(_ === 0).compile.drain,
            newEntries.take
          )

          r.flatMap {
            case Left(_) => F.pure(None)
            case Right(_) =>
              exec.flatMap {
                case None    => next(prev)
                case Some(r) => F.pure(Some(r))
              }
          }
        }

        ResultStream[F] {
          val initial = QueryInterpreter.Input.root(root, selection, rootScope)
          interpreter.interpretAll(NonEmptyList.one(initial)).map { initRes =>
            val jo: JsonObject = initRes.data.map { case (_, j) => j }.reduceLeft(_ deepMerge _).asObject.get
            val result0 = Result(initRes.errors, jo)

            (result0, AwaitEvents(next(jo)))
          }
        }
      }
    }

    override def interpretStream[A](
        root: A,
        selection: Selection[F, A],
        takeOne: Boolean = false,
        throttle: F ~> F = FunctionK.id[F]
    ): Stream[F, Result] =
      Stream
        .resource(interpretStream0(root, selection, takeOne, throttle))
        .flatMap { rec =>
          def unpack(rs: ResultStream[F]): Stream[F, Result] =
            Stream.eval(rs.evalNext).flatMap { case (result, goNext) =>
              Stream(result) ++
                Stream
                  .eval(goNext.await)
                  .flatMap(Stream.fromOption(_))
                  .flatMap(unpack)
            }
          unpack(rec)
        }
  }

  final case class StitchFailure(
      currentPath: Cursor,
      remainingPath: Cursor,
      msg: String
  ) extends Exception(msg) {
    override def toString = s"StitchFailure($currentPath, $remainingPath, $msg)"

    override def getMessage(): String = toString
  }
  def stitchInto(oldTree: Json, subTree: Json, remainingPath: Cursor, currentPath: Cursor): EitherT[Eval, StitchFailure, Json] = {
    remainingPath.uncons match {
      case None => EitherT.pure(subTree)
      case Some((p, tl)) =>
        val newCurrent = currentPath.add(p)
        def err(msg: String) = StitchFailure(newCurrent, tl, msg)
        p match {
          case GraphArc.Field(name) =>
            for {
              oldObj <-
                EitherT.fromOption[Eval](oldTree.asObject, err(s"Expected object at $name, but got ${oldTree.name}"))
              oldValue <-
                EitherT.fromOption[Eval](oldObj(name), err(s"Expected field $name in object, but found nothing"))
              newSubTree <- stitchInto(oldValue, subTree, tl, newCurrent)
            } yield oldObj.add(name, newSubTree).asJson
          case GraphArc.Index(index) =>
            for {
              oldArr <-
                EitherT.fromOption[Eval](oldTree.asArray, err(s"expected array at, but found ${oldTree.name}"))
              atIndex <-
                EitherT.fromOption[Eval](oldArr.get(index.toLong), err(s"expected array at $index, but found nothing"))
              newSubTree <- stitchInto(atIndex, subTree, tl, newCurrent)
            } yield oldArr.updated(index, newSubTree).asJson
        }
    }
  }
}
