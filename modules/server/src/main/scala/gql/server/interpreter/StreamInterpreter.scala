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
          entries: List[EvalState.Entry[F]]
      )

      for {
        state <- Resource.eval {
          Deferred[F, Unit].flatMap { d =>
            Ref[F].of(StreamingApiState(d, Nil))
          }
        }
        newEntries <- Resource.eval(Queue.bounded[F, Unit](1))
        api = new StreamingApi[F] {
          def submitAndAwaitExecution(
              ident: NodeId,
              sen: StepEvalNode[F, Either[Throwable, ?], ?]
          ): F[Unit] =
            state.modify { s =>
              (
                s.copy(entries = EvalState.Entry[F](ident, sen) :: s.entries),
                newEntries.tryOffer(()).void >> s.awaitExecution.get
              )
            }.flatten

          def currentExecution: F[F[Unit]] = state.get.map(_.awaitExecution.get)
        }
        rootScope <- Res.make[F]
        sup <- Supervisor[F]
        counter <- Resource.eval(SignallingRef[F].of(0))
        interpreter <- Resource.eval {
          QueryInterpreter[F, A](selection, schemaState, throttle, sup, api, counter, rootScope)
        }
      } yield {
        def patch(res: QueryInterpreter.Results, prev: JsonObject): JsonObject = {
          val errPatches = res.errors.flatMap(_.paths).map(p => (p, Json.Null))
          (errPatches ++ Chain.fromSeq(res.data)).toList.foldLeft(prev) { case (accum, (pos, patch)) =>
            applyPatch(accum, pos, patch)
          }
        }

        def next(prev: JsonObject): F[Option[ResultStream[F]]] = {
          val exec = for {
            _ <- accumulate.traverse_(d => Temporal[F].sleep(d))
            doneExecNext <- Deferred[F, Unit]
            state <- state.getAndSet(StreamingApiState(doneExecNext, Nil))
            entries = state.entries
            (res: Option[ResultStream[F]]) <- rootScope
              .lease {
                entries.traverse { e =>
                  e.node.en.active.keepAlive.map {
                    case false => None
                    case true  => Some(e)
                  }
                }
              }
              .use {
                case None => F.canceled.as(Option.empty[ResultStream[F]]) // root dead, stop
                case Some((openedLease, opened)) =>
                  val opt = opened.collect { case Some(x) => x }.toNel.map { relevant =>
                    val lookup = relevant.toList.groupMap(_.nodeId)(_.node)
                    // val inputs = relevant.map { case (entr: EvalState.Entry[F, a]) =>
                    //   QueryInterpreter.Input[F, a](entr.cont, entr.a)
                    // }

                    ResultStream[F] {
                      interpreter
                        .interpret(
                          Nil,
                          StreamingAdditions(lookup)
                        )
                        .flatMap { results =>
                          val jo = patch(results, prev)

                          rootScope.release(openedLease).as {
                            (
                              Result(results.errors, jo),
                              AwaitEvents(state.awaitExecution.complete(()) *> next(jo))
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
          interpreter
            .interpret(
              List(root),
              StreamingAdditions(Map.empty)
            )
            .map { initRes =>
              val jo = patch(initRes, JsonObject.empty)
              // initRes.data.map { case (_, j) => j }.reduceLeft(_ deepMerge _).asObject.get
              val result0 = Result(initRes.errors, jo)

              val reset0 = Deferred[F, Unit].flatMap { d =>
                state.modify { s =>
                  assert(
                    s.entries.isEmpty,
                    "Initial execution should not have any pending entries, everything should be blocked on awaitExecution."
                  )
                  (s.copy(awaitExecution = d), s.awaitExecution.complete(()))
                }.flatten
              }
              (result0, AwaitEvents(reset0 >> next(jo)))
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

  def applyPatch(data: JsonObject, path: Cursor, value: Json): JsonObject = {
    def go(
        current: Option[Json],
        remainingPath: Cursor
    ): Eval[Json] = Eval.defer {
      remainingPath.uncons match {
        case None => Eval.now(value)
        case Some((p, tl)) =>
          p match {
            case GraphArc.Field(name) =>
              val jo = current match {
                case Some(j) =>
                  j.asObject.getOrElse {
                    assert(j.isNull)
                    JsonObject.empty
                  }
                case None => JsonObject.empty
              }
              go(jo(name), tl).map { newValue =>
                jo.add(name, newValue).asJson
              }
            case GraphArc.Index(index) =>
              val ja = current match {
                case Some(j) =>
                  j.asArray.getOrElse {
                    assert(j.isNull)
                    Vector.empty
                  }
                case None => Vector.empty
              }
              go(ja.get(index.toLong), tl).map { newValue =>
                val padded = ja.padTo(index + 1, Json.Null).updated(index, newValue)
                padded.asJson
              }
          }
      }
    }
    go(Some(data.asJson), path).value.asObject.get
  }
}
