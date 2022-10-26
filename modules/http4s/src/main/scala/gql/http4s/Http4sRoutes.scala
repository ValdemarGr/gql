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

object Http4sRoutes {
  protected implicit lazy val cd: Decoder[CompilerParameters] =
    io.circe.generic.semiauto.deriveDecoder[CompilerParameters]
  protected implicit def ed[F[_]: Concurrent]: EntityDecoder[F, CompilerParameters] =
    org.http4s.circe.jsonOf[F, CompilerParameters]

  def sync[F[_]](
      compiler: Http4sCompiler[F],
      path: String = "graphql"
  )(implicit F: Concurrent[F]): HttpRoutes[F] = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._
    import org.http4s.circe._

    HttpRoutes.of[F] { case r @ POST -> Root / `path` =>
      r.as[CompilerParameters].flatMap { params =>
        compiler.compile(Http4sCompilerParametes(params, r.headers)).flatMap {
          case Left(res) => F.pure(res)
          case Right(app) =>
            val fa = app match {
              case Application.Mutation(run)     => run
              case Application.Query(run)        => run
              case Application.Subscription(run) => run.take(1).compile.lastOrError
            }

            fa.flatMap(qr => Ok(qr.asGraphQL.asJson))
        }
      }
    }
  }

  def ws[F[_]](
      getCompiler: Map[String, Json] => F[Either[String, Compiler[F]]],
      wsb: WebSocketBuilder[F],
      path: String = "ws"
  )(implicit F: Async[F]): HttpRoutes[F] = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._

    HttpRoutes.of[F] { case GET -> Root / `path` =>
      GraphqlWS[F](getCompiler).allocated.flatMap { case ((toClient, fromClient), close) =>
        wsb
          .withOnClose(close)
          .withFilterPingPongs(true)
          .build(
            toClient.evalMap[F, WebSocketFrame] {
              case Left(te) => F.fromEither(WebSocketFrame.Close(te.code, te.message))
              case Right(x) => F.pure(WebSocketFrame.Text(x.asJson.noSpaces))
            },
            _.evalMap[F, Option[String]] {
              case WebSocketFrame.Text(x, true)                   => F.pure(Some(x))
              case c: WebSocketFrame.Close if c.closeCode == 1000 => F.pure(None)
              case other                                          => F.raiseError(new Exception(s"Unexpected frame: $other"))
            }.unNone
              .evalMap { x => F.fromEither(io.circe.parser.decode[GraphqlWS.FromClient](x)) }
              .through(fromClient)
          )
      }
    }
  }
}
