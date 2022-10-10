package gql.graphqlws

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.syntax._
import gql._
import cats.data._
import gql.interpreter.EvalFailure
import cats.effect.std._
import fs2.Pull

object GraphqlWS {
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

  def apply[F[_]](
      getCompiler: Map[String, Json] => F[Either[String, Compiler[F]]]
  )(implicit
      F: Async[F]
  ): Resource[F, (fs2.Stream[F, Either[TechnicalError, FromServer]], fs2.Stream[F, FromClient] => fs2.Stream[F, Unit])] = {
    val stateR =
      Resource.make(F.ref[State[F]](State.Connecting()))(
        _.get
          .map {
            case State.Connecting()       => Map.empty
            case State.Connected(_, _, m) => m
            case State.Terminating(m)     => m
          }
          .flatMap(_.values.toList.parSequence_)
      )

    stateR.flatMap { state =>
      Resource.eval(Queue.bounded[F, Message](1024)).flatMap { toClient =>
        Supervisor[F].evalMap { sup =>
          import scala.concurrent.duration._
          val timeoutF =
            F.sleep(30.seconds) >> toClient.offer(Left(TechnicalError(4408, "Connection initialisation timeout")))

          sup.supervise(timeoutF).map { timeoutFiber =>
            def send(fc: FromServer): F[Unit] = toClient.offer(fc.asRight)

            def err(m: Map[String, F[Unit]], code: Int, message: String): (State.Terminating[F], F[Unit]) =
              (State.Terminating(m), toClient.offer(TechnicalError(code, message).asLeft))

            def handleMessage(fc: FromClient): F[Unit] =
              F.uncancelable { _ =>
                fc match {
                  case Bidirectional.Ping(payload) => F.pure(Some(Right(Bidirectional.Pong(payload))))
                  case Bidirectional.Pong(_)       => F.pure(None)
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
                        case c @ State.Connected(ip, compiler, m) =>
                          m.get(id) match {
                            case Some(sub) => err(m, 4409, s"Subscriber for $id already exists")
                            case None      =>
                              // Start out with F.unit
                              val newState = State.Connected(ip, compiler, m + (id -> F.unit))

                              val subscribeF: F[Unit] =
                                compiler
                                  .compile(payload)
                                  .flatMap {
                                    // Failed during parsing or preparation
                                    // Emit error and remove ourselves from the map
                                    case Left(err) =>
                                      val j = err match {
                                        case CompilationError.Parse(p)       => p.asGraphQL
                                        case CompilationError.Preparation(p) => p.asGraphQL
                                      }

                                      val cleanupF = state.modify {
                                        case State.Connected(_, _, m2) =>
                                          val releaseF = m2.get(id).sequence_
                                          (State.Connected(ip, compiler, m2 - id), releaseF)
                                        case x => (x, F.unit)
                                      }.flatten

                                      cleanupF >> send(FromServer.Error(id, Chain(j)))
                                    case Right(app) =>
                                      val s = app match {
                                        case Application.Query(run)        => fs2.Stream.eval(run)
                                        case Application.Mutation(run)     => fs2.Stream.eval(run)
                                        case Application.Subscription(run) => run
                                      }

                                      val bgFiber =
                                        sup.supervise {
                                          (s.map(x => Right(FromServer.Next(id, x))) ++ fs2.Stream(Right(Bidirectional.Complete(id))))
                                            .enqueueUnterminated(toClient)
                                            .compile
                                            .drain
                                        }

                                      bgFiber.flatMap { fib =>
                                        state.modify {
                                          case State.Connected(ip, compiler, m) =>
                                            (State.Connected(ip, compiler, m + (id -> fib.cancel)), F.unit)
                                          case x => (x, fib.cancel)
                                        }.flatten
                                      } as None
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
                            case State.Terminating(m)            => (State.Terminating(m), F.unit)
                            case State.Connected(_, compiler, m) => err(m, 4429, "Too many initialization requests")
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
      lazy val payload = c.downField("payload").as[Map[String, Json]]
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

    implicit lazy val compilerParamsDec = io.circe.generic.semiauto.deriveDecoder[CompilerParameters]

    implicit lazy val dec: Decoder[FromClient] = Decoder.instance[FromClient] { c =>
      c.downField("type").as[String].flatMap {
        case "connection_init" => c.downField("payload").as[Map[String, Json]].map(ConnectionInit.apply)
        case "subscribe" =>
          (
            c.downField("id").as[String],
            c.downField("payload").as[CompilerParameters]
          ).mapN(Subscribe(_, _))
        case other => c.as[Bidirectional]
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
