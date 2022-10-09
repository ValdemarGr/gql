package gql.graphqlws

import cats.implicits._
import io.circe._
import io.circe.syntax._
import gql._
import cats.data._
import gql.interpreter.EvalFailure

sealed trait GraphqlWS[F[_]] {}

object GraphqlWS {
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
