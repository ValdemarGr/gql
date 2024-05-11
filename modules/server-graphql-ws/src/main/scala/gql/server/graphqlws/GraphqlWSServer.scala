/*
 * Copyright 2024 Valdemar Grange
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
package gql.graphqlws

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import gql._
import cats.data._
import cats.effect.std._
import gql.graphqlws.GraphqlWS._

object GraphqlWSServer {
  trait Subscribe[F[_]] {
    def subscribe(id: String, payload: QueryParameters): Resource[F, Compiler.Outcome[F]]
  }

  trait ConnectionInit[F[_]] {
    def init(payload: Map[String, Json]): F[Either[String, Subscribe[F]]]
  }

  final case class SubscriptionState[F[_]](close: F[Unit])

  sealed trait State[F[_]]
  object State {
    final case class Connecting[F[_]]() extends State[F]
    final case class Connected[F[_]](
        initPayload: Map[String, Json],
        compiler: Subscribe[F],
        subscriptions: Map[String, F[Unit]]
    ) extends State[F]
    final case class Terminating[F[_]](
        subscriptions: Map[String, F[Unit]]
    ) extends State[F]
  }

  final case class TechnicalError(code: Code, message: String)

  type Message = Either[TechnicalError, FromServer]

  def apply[F[_]](init: ConnectionInit[F])(implicit
      F: Async[F]
  ): Resource[F, (fs2.Stream[F, Either[TechnicalError, FromServer]], fs2.Stream[F, FromClient] => fs2.Stream[F, Unit])] = {
    Supervisor[F].evalMap { sup =>
      Queue.bounded[F, Message](1024).flatMap { toClient =>
        F.ref[State[F]](State.Connecting()).flatMap { state =>
          import scala.concurrent.duration._
          val timeoutF =
            F.sleep(30.seconds) >> toClient.offer(Left(TechnicalError(Code.ConnectionInitTimedOut, "Connection initialisation timeout")))

          sup.supervise(timeoutF).map { timeoutFiber =>
            def send(fc: FromServer): F[Unit] = toClient.offer(fc.asRight)

            def err(m: Map[String, F[Unit]], code: Code, message: String): (State.Terminating[F], F[Unit]) =
              (State.Terminating(m), toClient.offer(TechnicalError(code, message).asLeft))

            def handleMessage(fc: FromClient): F[Unit] =
              fc match {
                case Bidirectional.Ping(payload) => send(Bidirectional.Pong(payload))
                case Bidirectional.Pong(_)       => F.unit
                case Bidirectional.Complete(id)  =>
                  // If we remove the subscription, we need to close it
                  // Cancellation can occur between map removal and flattening
                  state
                    .modify[F[Unit]] {
                      case State.Connecting()   => err(Map.empty, Code.Unauthorized, "Unauthorized")
                      case State.Terminating(m) => (State.Terminating(m), F.unit)
                      case c @ State.Connected(ip, compiler, m) =>
                        m.get(id) match {
                          case None        => (c, F.unit)
                          case Some(close) => (State.Connected(ip, compiler, m - id), close)
                        }
                    }
                    .flatten
                case FromClient.Subscribe(id, payload) =>
                  state
                    .modify[F[Unit]] {
                      case State.Connecting()   => err(Map.empty, Code.Unauthorized, "Unauthorized")
                      case State.Terminating(m) => (State.Terminating(m), F.unit)
                      case State.Connected(ip, compiler, m) =>
                        m.get(id) match {
                          case Some(_) => err(m, Code.SubscriptionForIdAlreadyExists, s"Subscriber for $id already exists")
                          case None    =>
                            // Start out with F.unit
                            val newState = State.Connected(ip, compiler, m + (id -> F.unit))

                            val compiled = compiler.subscribe(id, payload)

                            val subscribeF: F[Unit] =
                              F.uncancelable { _ =>
                                compiled.allocated.flatMap { case (outcome, release) =>
                                  outcome match {
                                    case Left(err) =>
                                      val j = err match {
                                        case CompilationError.Parse(p)       => Chain(p.asJsonObject)
                                        case CompilationError.Preparation(p) => p.toChain.map(_.asJsonObject)
                                      }

                                      val cleanupF = state.update {
                                        case State.Connected(_, _, m2) => State.Connected(ip, compiler, m2 - id)
                                        case x                         => x
                                      }

                                      release >> cleanupF >> send(FromServer.Error(id, j))
                                    case Right(app) =>
                                      val s = app match {
                                        case Application.Query(run)        => fs2.Stream.eval(run)
                                        case Application.Mutation(run)     => fs2.Stream.eval(run)
                                        case Application.Subscription(run) => run
                                      }

                                      val cleanupF = state.update {
                                        case State.Connected(_, _, m2) => State.Connected(ip, compiler, m2 - id)
                                        case x                         => x
                                      }

                                      val bgFiber =
                                        sup.supervise {
                                          (
                                            s.map(x => Right(FromServer.Next(id, x))) ++
                                              fs2.Stream(Right(Bidirectional.Complete(id))) ++
                                              fs2.Stream.exec(cleanupF)
                                          )
                                            .enqueueUnterminated(toClient)
                                            .compile
                                            .drain
                                            .guarantee(release)
                                        }

                                      bgFiber.flatMap { fib =>
                                        state.modify {
                                          case State.Connected(ip, compiler, m) =>
                                            (State.Connected(ip, compiler, m + (id -> fib.cancel)), F.unit)
                                          case x => (x, fib.cancel)
                                        }.flatten
                                      }
                                  }
                                }
                              }

                            (newState, subscribeF)
                        }
                    }
                    .flatten
                case FromClient.ConnectionInit(payload) =>
                  timeoutFiber.cancel >>
                    init.init(payload).flatMap { ce =>
                      state
                        .modify[F[Unit]] {
                          case State.Terminating(m)     => (State.Terminating(m), F.unit)
                          case State.Connected(_, _, m) => err(m, Code.TooManyInitRequests, "Too many initialization requests")
                          case State.Connecting() =>
                            ce match {
                              case Left(x) => err(Map.empty, Code.StillConnecting, x)
                              case Right(compiler) =>
                                (State.Connected(payload, compiler, Map.empty), send(FromServer.ConnectionAck(Map.empty)))
                            }
                        }
                        .flatten
                    }
              }

            val dequeue =
              fs2.Stream
                .fromQueueUnterminated(toClient)
                .takeWhile(_.isRight, takeFailure = true)

            val handle: fs2.Pipe[F, FromClient, Unit] = _.evalMap(handleMessage)

            (dequeue, handle)
          }
        }
      }
    }
  }
}
