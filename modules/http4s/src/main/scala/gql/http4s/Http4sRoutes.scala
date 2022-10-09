package gql.http4s

import cats.effect._
import gql.parser.{QueryParser => P}
import cats.data._
import cats.implicits._
import cats._
import io.circe._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.Http4sDsl
import fs2.{Stream, Pure}
import org.http4s.headers.Authorization
import gql.interpreter.Interpreter
import gql._
import org.http4s.server.websocket.WebSocketBuilder

object Http4sRoutes {
  implicit lazy val cd = io.circe.generic.semiauto.deriveDecoder[CompilerParameters]
  implicit def ed[F[_]: Concurrent] = org.http4s.circe.jsonOf[F, CompilerParameters]

  def sync[F[_]](
      compiler: Http4sCompiler[F],
      path: String = "graphql"
  )(implicit F: Concurrent[F]) = {
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
      compiler: Http4sCompiler[F],
      path: String = "ws",
      wsb: WebSocketBuilder[F]
  )(implicit F: Concurrent[F]) = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._
    import org.http4s.circe._

    HttpRoutes.of[F] {
      case r @ GET -> Root / `path` => ???
    }
  }
}
