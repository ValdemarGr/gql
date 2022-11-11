/*
 * Copyright 2022 Valdemar Grange
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

object GraphqlWS {
  type Compiler[F[_]] = CompilerParameters => Resource[F, Compiler.Outcome[F]]

  type GetCompiler[F[_]] = Map[String, Json] => F[Either[String, Compiler[F]]]

  final case class SubscriptionState[F[_]](
      close: F[Unit]
  )

  sealed trait State[F[_]]
  object State {
    final case class Connecting[F[_]]() extends State[F]
    final case class Connected[F[_]](
        initPayload: Map[String, Json],
        compiler: Compiler[F],
        subscriptions: Map[String, F[Unit]]
    ) extends State[F]
    final case class Terminating[F[_]](
        subscriptions: Map[String, F[Unit]]
    ) extends State[F]
  }

  final case class TechnicalError(
      code: Int,
      message: String
  )

  type Message = Either[TechnicalError, FromServer]

  def apply[F[_]](getCompiler: GetCompiler[F])(implicit
      F: Async[F]
  ): Resource[F, (fs2.Stream[F, Either[TechnicalError, FromServer]], fs2.Stream[F, FromClient] => fs2.Stream[F, Unit])] = {
    Supervisor[F].evalMap { sup =>
      Queue.bounded[F, Message](1024).flatMap { toClient =>
        F.ref[State[F]](State.Connecting()).flatMap { state =>
          import scala.concurrent.duration._
          val timeoutF =
            F.sleep(30.seconds) >> toClient.offer(Left(TechnicalError(4408, "Connection initialisation timeout")))

          sup.supervise(timeoutF).map { timeoutFiber =>
            def send(fc: FromServer): F[Unit] = toClient.offer(fc.asRight)

            def err(m: Map[String, F[Unit]], code: Int, message: String): (State.Terminating[F], F[Unit]) =
              (State.Terminating(m), toClient.offer(TechnicalError(code, message).asLeft))

            def handleMessage(fc: FromClient): F[Unit] =
              fc match {
                case Bidirectional.Ping(payload) => send(Bidirectional.Pong(payload))
                case Bidirectional.Pong(_)       => F.unit
                case Bidirectional.Complete(id)  =>
                  // If we remove the subscription, we need to close it
                  // Cancellation can occur between map removal and falttening
                  state
                    .modify[F[Unit]] {
                      case State.Connecting()   => err(Map.empty, 4401, "Unauthorized")
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
                      case State.Connecting()   => err(Map.empty, 4401, "Unauthorized")
                      case State.Terminating(m) => (State.Terminating(m), F.unit)
                      case State.Connected(ip, compiler, m) =>
                        m.get(id) match {
                          case Some(_) => err(m, 4409, s"Subscriber for $id already exists")
                          case None    =>
                            // Start out with F.unit
                            val newState = State.Connected(ip, compiler, m + (id -> F.unit))

                            val compiled = compiler(payload)

                            val subscribeF: F[Unit] =
                              F.uncancelable { _ =>
                                compiled.allocated.flatMap { case (outcome, release) =>
                                  outcome match {
                                    case Left(err) =>
                                      val j = err match {
                                        case CompilationError.Parse(p)       => p.asGraphQL
                                        case CompilationError.Preparation(p) => p.asGraphQL
                                      }

                                      val cleanupF = state.update {
                                        case State.Connected(_, _, m2) => State.Connected(ip, compiler, m2 - id)
                                        case x                         => x
                                      }

                                      release >> cleanupF >> send(FromServer.Error(id, Chain(j)))
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
                    getCompiler(payload).flatMap { ce =>
                      state
                        .modify[F[Unit]] {
                          case State.Terminating(m)     => (State.Terminating(m), F.unit)
                          case State.Connected(_, _, m) => err(m, 4429, "Too many initialization requests")
                          case State.Connecting() =>
                            ce match {
                              case Left(x) => err(Map.empty, 4441, x)
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

  sealed trait Bidirectional extends FromClient with FromServer
  object Bidirectional {
    final case class Ping(payload: Map[String, Json]) extends Bidirectional
    final case class Pong(payload: Map[String, Json]) extends Bidirectional
    final case class Complete(id: String) extends Bidirectional

    implicit lazy val enc: Encoder[Bidirectional] = {
      case Ping(payload) => Json.obj("type" -> "ping".asJson, "payload" -> payload.asJson)
      case Pong(payload) => Json.obj("type" -> "pong".asJson, "payload" -> payload.asJson)
      case Complete(id)  => Json.obj("type" -> "complete".asJson, "id" -> id.asJson)
    }
    implicit lazy val dec: Decoder[Bidirectional] = Decoder.instance[Bidirectional] { c =>
      lazy val payload = c.downField("payload").as[Option[Map[String, Json]]].map(_.getOrElse(Map.empty))
      c.downField("type").as[String].flatMap {
        case "ping"     => payload.map(Ping.apply)
        case "pong"     => payload.map(Pong.apply)
        case "complete" => c.downField("id").as[String].map(Complete.apply)
        case other      => Left(DecodingFailure(s"unknown type $other", c.history))
      }
    }
  }

  sealed trait FromClient
  object FromClient {
    final case class ConnectionInit(payload: Map[String, Json]) extends FromClient
    final case class Subscribe(id: String, payload: CompilerParameters) extends FromClient

    implicit lazy val compilerParamsDec: Decoder[CompilerParameters] = Decoder.instance[CompilerParameters] { c =>
      (
        c.downField("query").as[String],
        c.downField("variables").as[Option[Map[String, Json]]],
        c.downField("operationName").as[Option[String]]
      ).mapN(CompilerParameters.apply)
    }

    implicit lazy val dec: Decoder[FromClient] = Decoder.instance[FromClient] { c =>
      c.downField("type").as[String].flatMap {
        case "connection_init" => c.downField("payload").as[Option[Map[String, Json]]].map(m => ConnectionInit(m.getOrElse(Map.empty)))
        case "subscribe" =>
          (
            c.downField("id").as[String],
            c.downField("payload").as[CompilerParameters]
          ).mapN(Subscribe(_, _))
        case _ => c.as[Bidirectional]
      }
    }
  }

  sealed trait FromServer
  object FromServer {
    final case class ConnectionAck(payload: Map[String, Json]) extends FromServer
    final case class Next(id: String, payload: QueryResult) extends FromServer
    final case class Error(id: String, payload: Chain[JsonObject]) extends FromServer

    implicit lazy val enc: Encoder[FromServer] = {
      case ConnectionAck(payload) => Json.obj("type" -> "connection_ack".asJson, "payload" -> payload.asJson)
      case Next(id, payload)      => Json.obj("type" -> "next".asJson, "id" -> id.asJson, "payload" -> payload.asGraphQL.asJson)
      case Error(id, payload) =>
        Json.obj(
          "type" -> "error".asJson,
          "id" -> id.asJson,
          "payload" -> payload.asJson
        )
      case b: Bidirectional => b.asJson
    }
  }
}
