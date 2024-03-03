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

import cats.implicits._
import io.circe._
import io.circe.syntax._
import gql._
import cats.data._

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
    final case class Subscribe(id: String, payload: QueryParameters) extends FromClient

    implicit lazy val queryParamsDec: Decoder[QueryParameters] = Decoder.instance[QueryParameters] { c =>
      (
        c.downField("query").as[String],
        c.downField("variables").as[Option[Map[String, Json]]],
        c.downField("operationName").as[Option[String]]
      ).mapN(QueryParameters.apply)
    }

    implicit lazy val dec: Decoder[FromClient] = Decoder.instance[FromClient] { c =>
      c.downField("type").as[String].flatMap {
        case "connection_init" => c.downField("payload").as[Option[Map[String, Json]]].map(m => ConnectionInit(m.getOrElse(Map.empty)))
        case "subscribe" =>
          (
            c.downField("id").as[String],
            c.downField("payload").as[QueryParameters]
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
      case Next(id, payload)      => Json.obj("type" -> "next".asJson, "id" -> id.asJson, "payload" -> payload.asJson)
      case Error(id, payload) =>
        Json.obj(
          "type" -> "error".asJson,
          "id" -> id.asJson,
          "payload" -> payload.asJson
        )
      case b: Bidirectional => b.asJson
    }
  }

  sealed trait Code {
    def code: Int
  }
  object Code {
    case object ConnectionInitTimedOut extends Code {
      def code = 4408
    }

    case object Unauthorized extends Code {
      def code = 4401
    }

    case object SubscriptionForIdAlreadyExists extends Code {
      def code = 4409
    }

    case object TooManyInitRequests extends Code {
      def code = 4429
    }

    case object StillConnecting extends Code {
      def code = 4441
    }
  }
}
