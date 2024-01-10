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

trait RequestHandler[F[_]] {
  type A

  def preParsing(headers: Headers): F[Either[Response[F], A]]

  def compile(params: QueryParameters, value: A): F[Either[Response[F], Compiler.Outcome[F]]]
}

object RequestHandler {
  type Aux[F[_], A0] = RequestHandler[F] { type A = A0 }

  def apply[F[_], A](
      preParsing: Headers => F[Either[Response[F], A]],
      compile: (QueryParameters, A) => F[Either[Response[F], Compiler.Outcome[F]]]
  ): RequestHandler[F] = {
    val preParsing0 = preParsing
    val compile0 = compile
    type A0 = A
    new RequestHandler[F] {
      type A = A0
      def preParsing(headers: Headers): F[Either[Response[F], A]] = preParsing0(headers)
      def compile(params: QueryParameters, value: A): F[Either[Response[F], Compiler.Outcome[F]]] = compile0(params, value)
    }
  }
}

trait WSHandler[F[_]] {
  type A

  def preParsing(headers: Map[String, Json]): F[Either[String, A]]

  def compile(params: QueryParameters, value: A): Resource[F, Compiler.Outcome[F]]
}

object WSHandler {
  type Aux[F[_], A0] = WSHandler[F] { type A = A0 }

  def apply[F[_], A](
      preParsing: Map[String, Json] => F[Either[String, A]],
      compile: (QueryParameters, A) => Resource[F, Compiler.Outcome[F]]
  ): WSHandler[F] = {
    val preParsing0 = preParsing
    val compile0 = compile
    type A0 = A
    new WSHandler[F] {
      type A = A0
      def preParsing(headers: Map[String, Json]): F[Either[String, A]] = preParsing0(headers)
      def compile(params: QueryParameters, value: A): Resource[F, Compiler.Outcome[F]] = compile0(params, value)
    }
  }
}

object Http4sRoutes {
  implicit protected lazy val cd: Decoder[QueryParameters] = Decoder.instance[QueryParameters] { c =>
    for {
      query <- c.downField("query").as[String]
      variables <- c.downField("variables").as[Option[Map[String, Json]]]
      operationName <- c.downField("operationName").as[Option[String]]
    } yield QueryParameters(query, variables, operationName)
  }

  implicit protected def ed[F[_]: Concurrent]: EntityDecoder[F, QueryParameters] =
    org.http4s.circe.jsonOf[F, QueryParameters]

  def runCompiledSync[F[_]: Concurrent](compiled: Compiler.Outcome[F]) = {
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

  type SC[F[_], A] = F[Either[Response[F], A]]
  def syncFull[F[_]](full: Headers => SC[F, QueryParameters => SC[F, Compiler.Outcome[F]]], path: String = "graphql")(implicit
      F: Concurrent[F]
  ): HttpRoutes[F] = {
    val d = new Http4sDsl[F] {}
    import d._

    object QueryParam extends QueryParamDecoderMatcher[String]("query")
    object OperationName extends OptionalQueryParamDecoderMatcher[String]("operationName")
    object Variables extends OptionalQueryParamDecoderMatcher[String]("variables")

    def runWith(req: Request[F], parse: F[QueryParameters]) =
      F.flatMap(full(req.headers)) {
        case Left(resp) => F.pure(resp)
        case Right(f) =>
          parse.flatMap { params =>
            F.flatMap(f(params)) {
              case Left(resp) => F.pure(resp)
              case Right(c)   => runCompiledSync[F](c)
            }
          }
      }

    HttpRoutes.of[F] {
      // https://graphql.github.io/graphql-over-http/draft/#sec-GET
      case r @ GET -> Root / `path` :? QueryParam(query) :? OperationName(operationName) :? Variables(variables) =>
        val opName = if (operationName.forall(_.isEmpty)) None else operationName
        variables.traverse(io.circe.parser.decode[JsonObject](_)) match {
          case Left(err) => BadRequest(err.getMessage)
          case Right(variables) =>
            runWith(r, F.pure(QueryParameters(query, variables.map(_.toMap), opName)))
        }
      // https://graphql.github.io/graphql-over-http/draft/#sec-Request
      // all methods are allowed
      case r @ (_: Method) -> Root / `path` => runWith(r, r.as[QueryParameters])
    }
  }

  def syncHandler[F[_]](
      handler: RequestHandler[F],
      path: String = "graphql"
  )(implicit F: Concurrent[F]): HttpRoutes[F] = syncFull[F](
    headers => handler.preParsing(headers).map(_.map(a => (qp: QueryParameters) => handler.compile(qp, a))),
    path
  )

  def syncSimple[F[_]](
      compile: QueryParameters => F[Either[Response[F], Compiler.Outcome[F]]],
      path: String = "graphql"
  )(implicit F: Concurrent[F]): HttpRoutes[F] = syncFull[F]({ _ => F.pure(Right(compile)) }, path)

  def wsHandler[F[_]](
      handler: WSHandler[F],
      wsb: WebSocketBuilder[F],
      path: String = "ws"
  )(implicit F: Async[F]): HttpRoutes[F] =
    ws[F](
      m => handler.preParsing(m).map(_.map(a => (qp: QueryParameters) => handler.compile(qp, a))),
      wsb,
      path
    )

  def ws[F[_]](
      getCompiler: GraphqlWSServer.GetCompiler[F],
      wsb: WebSocketBuilder[F],
      path: String = "ws",
      pingInterval: FiniteDuration = 30.seconds
  )(implicit F: Async[F]): HttpRoutes[F] = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._

    HttpRoutes.of[F] { case GET -> Root / `path` =>
      F.uncancelable { _ =>
        GraphqlWSServer[F](getCompiler).allocated.flatMap { case ((toClient, fromClient), close) =>
          val toSend = toClient.evalMap[F, WebSocketFrame] {
            case Left(te) => F.fromEither(WebSocketFrame.Close(te.code.code, te.message))
            case Right(x) => F.pure(WebSocketFrame.Text(x.asJson.noSpaces))
          }
          wsb
            .withOnClose(close)
            .withFilterPingPongs(true)
            .withHeaders(Headers(Header.Raw(ci"Sec-WebSocket-Protocol", "graphql-transport-ws")))
            .build(
              toSend.mergeHaltL(fs2.Stream.awakeEvery[F](pingInterval).as(WebSocketFrame.Ping())),
              _.evalMap[F, Option[String]] {
                case WebSocketFrame.Text(x, true) => F.pure(Some(x))
                case _: WebSocketFrame.Close      => F.pure(None)
                // case c: WebSocketFrame.Close if c.closeCode == 1000 => F.pure(None)
                case other => F.raiseError(new Exception(s"Unexpected frame: $other"))
              }.unNone
                .evalMap(x => F.fromEither(io.circe.parser.decode[GraphqlWS.FromClient](x)))
                .through(fromClient)
            )
        }
      }
    }
  }
}
