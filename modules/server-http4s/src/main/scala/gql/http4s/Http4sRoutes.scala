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
package gql.http4s

import cats.effect._
import cats.implicits._
import io.circe._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import gql._
import org.http4s.server.websocket.WebSocketBuilder
import gql.graphqlws.GraphqlWS
import org.http4s.websocket.WebSocketFrame
import org.typelevel.ci._
import gql.graphqlws.GraphqlWSServer
import scala.concurrent.duration._
import fs2.concurrent.SignallingRef

object implicits {
  implicit lazy val cd: Decoder[QueryParameters] = Decoder.instance[QueryParameters] { c =>
    for {
      query <- c.downField("query").as[String]
      variables <- c.downField("variables").as[Option[Map[String, Json]]]
      operationName <- c.downField("operationName").as[Option[String]]
    } yield QueryParameters(query, variables, operationName)
  }

  implicit def ed[F[_]: Concurrent]: EntityDecoder[F, QueryParameters] =
    org.http4s.circe.jsonOf[F, QueryParameters]
}

object Http4sRoutes {
  import implicits._
  import org.http4s.dsl.{impl => I}

  object QueryParam extends I.QueryParamDecoderMatcher[String]("query")
  object OperationName extends I.OptionalQueryParamDecoderMatcher[String]("operationName")
  object Variables extends I.OptionalQueryParamDecoderMatcher[String]("variables")

  final case class Params(
      query: String,
      variables: Option[String],
      operationName: Option[String]
  )

  object opName {
    def unapply(s: Option[String]): Option[Option[String]] = Some(if (s.forall(_.isEmpty)) None else s)
  }

  object parms {
    def unapply[F[_]](req: Request[F]): Option[(Request[F], Params)] = {
      val d = new Http4sDsl[F] {}
      import d._
      req match {
        case r :? QueryParam(query) +& OperationName(opName(operationName)) +& Variables(variables) =>
          Some(r -> Params(query, variables, operationName))
        case _ => None
      }
    }
  }

  final case class Content[F[_]](
      request: Request[F],
      parse: F[QueryParameters]
  )

  def sync[F[_]](path: String = "graphql")(f: Content[F] => F[Response[F]])(implicit F: Concurrent[F]) = {
    val d = new Http4sDsl[F] {}
    import d._
    HttpRoutes.of[F] {
      // https://graphql.github.io/graphql-over-http/draft/#sec-GET
      case r @ GET -> Root / `path` parms p =>
        val x = F.unit >> F.fromEither {
          p.variables
            .traverse(io.circe.parser.decode[JsonObject](_).map(_.toMap))
            .map(variables => QueryParameters(p.query, variables, p.operationName))
        }
        f(Content(r, x))
      // https://graphql.github.io/graphql-over-http/draft/#sec-Request
      // all methods are allowed
      case r @ _ -> Root / `path` => f(Content(r, r.as[QueryParameters]))
    }
  }

  def toResponse[F[_]: Concurrent](compiled: Compiler.Outcome[F]): F[Response[F]] = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._
    import org.http4s.circe._

    compiled match {
      case Left(pe: CompilationError.Parse)       => Ok(pe.asJson)
      case Left(pe: CompilationError.Preparation) => Ok(pe.asJson)
      case Right(app) =>
        val fa = app match {
          case Application.Mutation(run)     => run
          case Application.Query(run)        => run
          case Application.Subscription(run) => run.take(1).compile.lastOrError
        }

        fa.flatMap(qr => Ok(qr.asJson))
    }
  }

  def ws[F[_]](
      connectionInit: GraphqlWSServer.ConnectionInit[F],
      wsb: WebSocketBuilder[F],
      path: String = "ws",
      pingInterval: FiniteDuration = 30.seconds
  )(implicit F: Async[F]): HttpRoutes[F] = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._

    HttpRoutes.of[F] { case GET -> Root / `path` =>
      for {
        state <- SignallingRef[F].of(Option.empty[fs2.Pipe[F, GraphqlWS.FromClient, Unit]])
        response <- wsb
          .withFilterPingPongs(true)
          .withHeaders(Headers(Header.Raw(ci"Sec-WebSocket-Protocol", "graphql-transport-ws")))
          .build(
            fs2.Stream.resource(GraphqlWSServer[F](connectionInit)).flatMap { case (toClient, fromClient) =>
              val toSend = toClient.evalMap[F, WebSocketFrame] {
                case Left(te) => F.fromEither(WebSocketFrame.Close(te.code.code, te.message))
                case Right(x) => F.pure(WebSocketFrame.Text(x.asJson.noSpaces))
              }
              fs2.Stream.eval(state.set(Some(fromClient))) >>
                toSend.mergeHaltL(fs2.Stream.awakeEvery[F](pingInterval).as(WebSocketFrame.Ping()))
            },
            fromClient =>
              state.discrete
                .collectFirst { case Some(pipe) => pipe }
                .flatMap { pipe =>
                  val stream = fromClient
                    .evalMap[F, Option[String]] {
                      case WebSocketFrame.Text(x, true) => F.pure(Some(x))
                      case _: WebSocketFrame.Close      => F.pure(None)
                      // case c: WebSocketFrame.Close if c.closeCode == 1000 => F.pure(None)
                      case other => F.raiseError(new Exception(s"Unexpected frame: $other"))
                    }
                    .unNone
                    .evalMap(x => F.fromEither(io.circe.parser.decode[GraphqlWS.FromClient](x)))

                  stream.through(pipe)
                }
          )
      } yield response
    }
  }
}
