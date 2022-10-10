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

sealed trait GraphqlWS[F[_]] {}

object GraphqlWS {
  final case class SubscriptionState[F[_]](
      close: F[Unit]
  )

  sealed trait State[F[_]]
  object State {
    final case class Connecting[F[_]]() extends State[F]
    final case class Connected[F[_]](
        initPayload: Map[String, Json],
        subscriptions: Map[Int, SubscriptionState[F]]
    ) extends State[F]
    final case class Terminating[F[_]](
        subscriptions: Map[Int, SubscriptionState[F]]
    ) extends State[F]
  }

  final case class TechnicalError(
      code: Int,
      message: String
  )

  type Message = Either[TechnicalError, FromServer]

  def apply[F[_]](implicit
      F: Async[F]
  ): Resource[F, (fs2.Stream[F, Either[TechnicalError, FromServer]], fs2.Stream[F, FromClient] => fs2.Stream[F, Unit])] = {
    val stateR =
      Resource.make(F.ref[State[F]](State.Connecting()))(
        _.get
          .map {
            case State.Connecting()    => Map.empty
            case State.Connected(_, m) => m
            case State.Terminating(m)  => m
          }
          .flatMap(_.values.toList.parTraverse_(_.close))
      )

    stateR.flatMap { state =>
      Resource.eval(Queue.bounded[F, Message](1024)).flatMap { toClient =>
        Supervisor[F].evalMap { sup =>
          import scala.concurrent.duration._
          val timeoutF =
            F.sleep(30.seconds) >> toClient.offer(Left(TechnicalError(4408, "Connection initialisation timeout")))

          sup.supervise(timeoutF).map { timeoutFiber =>
            def handleMessage(fc: FromClient): F[Option[Message]] =
              F.uncancelable { _ =>
                fc match {
                  case Bidirectional.Ping(payload) => F.pure(Some(Right(Bidirectional.Pong(payload))))
                  case Bidirectional.Pong(_)       => F.pure(None)
                  case Bidirectional.Complete(id)  =>
                    // If we remove the subscription, we need to close it
                    // Cancellation can occur between map removal and falttening
                    state
                      .modify[F[Option[TechnicalError]]] {
                        case State.Connecting() =>
                          (State.Terminating(Map.empty), F.pure(Some(TechnicalError(4401, "Unauthorized"))))
                        case State.Terminating(m) =>
                          (State.Terminating(m), F.pure(None))
                        case c @ State.Connected(ip, m) =>
                          scala.util.Try(id.toInt).toOption match {
                            case None => (State.Terminating(m), F.pure(Some(TechnicalError(42, "provided id is illegal"))))
                            case Some(id) =>
                              m.get(id) match {
                                case None      => (c, F.pure(None))
                                case Some(sub) => (State.Connected(ip, m - id), sub.close.as(None))
                              }
                          }
                      }
                      .flatten
                      .map(_.map(Left(_)))
                  case FromClient.Subscribe(id, payload) =>
                    state
                      .modify[F[Option[TechnicalError]]] {
                        case State.Connecting() =>
                          (State.Terminating(Map.empty), F.pure(Some(TechnicalError(4401, "Unauthorized"))))
                        case State.Terminating(m) =>
                          (State.Terminating(m), F.pure(None))
                        case c @ State.Connected(ip, m) =>
                          scala.util.Try(id.toInt).toOption match {
                            case None => (State.Terminating(m), F.pure(Some(TechnicalError(42, "provided id is illegal"))))
                            case Some(id) =>
                              m.get(id) match {
                                case Some(sub) =>
                                  (State.Terminating(m), F.pure(Some(TechnicalError(4409, s"Subscriber for $id already exists"))))
                                // TODO
                                case None => ???
                              }
                          }
                      }
                      .flatten
                      .map(_.map(Left(_)))
                  case FromClient.ConnectionInit(payload) =>
                    timeoutFiber.cancel >>
                      state
                        .modify[Option[Message]] {
                          case State.Terminating(m) =>
                            (State.Terminating(m), None)
                          case State.Connected(_, m) =>
                            (State.Terminating(m), Some(Left(TechnicalError(4429, "Too many initialization requests"))))
                          case State.Connecting() =>
                            (State.Connected(payload, Map.empty), Some(Right(FromServer.ConnectionAck(Map.empty))))
                        }
                }
              }

            def handleRaise(fc: FromClient): F[Unit] =
              handleMessage(fc).flatMap(_.traverse_(toClient.offer))

            val dequeue =
              fs2.Stream
                .fromQueueUnterminated(toClient)
                .takeWhile(_.isRight, takeFailure = true)

            val handle: fs2.Pipe[F, FromClient, Unit] = _.evalMap(handleRaise)

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
    final case class Error(id: String, payload: Chain[EvalFailure]) extends FromServer

    implicit lazy val enc: Encoder[FromServer] = {
      case ConnectionAck(payload) => Json.obj("type" -> "connection_ack".asJson, "payload" -> payload.asJson)
      case Next(id, payload)      => Json.obj("type" -> "next".asJson, "id" -> id.asJson, "payload" -> payload.asGraphQL.asJson)
      case Error(id, payload) =>
        Json.obj(
          "type" -> "error".asJson,
          "id" -> id.asJson,
          "payload" -> payload.flatMap(_.asGraphQL).asJson
        )
      case b: Bidirectional => b.asJson
    }
  }
}
