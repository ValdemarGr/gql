package gql.http4s

import cats.effect._
import gql.parser.{QueryParser => P}
import cats.data._
import cats.implicits._
import cats._
import io.circe._
import org.http4s._
import org.http4s.implicits._
import gql.Schema
import org.http4s.dsl.Http4sDsl
import fs2.{Stream, Pure}
import gql.Executable
import org.http4s.headers.Authorization

object Http4sRoutes {
  sealed trait HandlerOutcome

  trait Handler[F[_]] { self =>
    def run(
        req: Request[Pure],
        query: NonEmptyList[P.ExecutableDefinition],
        variables: Map[String, Json]
    ): F[Either[Response[F], Executable[F, Unit, Unit, Unit]]]

    def mapK[G[_]: Monad](fk: F ~> G): Handler[G] =
      new Handler[G] {
        def run(
            req: Request[Pure],
            query: NonEmptyList[P.ExecutableDefinition],
            variables: Map[String, Json]
        ): G[Either[Response[G], Executable[G, Unit, Unit, Unit]]] =
          fk(self.run(req, query, variables)).map(_.leftMap(_.mapK(fk)).map(_.mapK(fk)))
      }
  }

  object Handler {
    def full[F[_]: Monad](
        f: (
            Request[Pure],
            NonEmptyList[P.ExecutableDefinition],
            Map[String, Json]
        ) => F[Either[Response[F], Executable[F, Unit, Unit, Unit]]]
    ): Handler[F] = {
      new Handler[F] {
        def run(
            req: Request[Pure],
            query: NonEmptyList[P.ExecutableDefinition],
            variables: Map[String, Json]
        ): F[Either[Response[F], Executable[F, Unit, Unit, Unit]]] = f(req, query, variables)
      }
    }

    def fullSchema[F[_]: Async](
        f: Request[Pure] => F[Either[Response[F], Schema[F, Unit, Unit, Unit]]]
    ): Handler[F] = {
      new Handler[F] {
        def run(
            req: Request[Pure],
            query: NonEmptyList[P.ExecutableDefinition],
            variables: Map[String, Json]
        ): F[Either[Response[F], Executable[F, Unit, Unit, Unit]]] = f(req).map(_.map(_.assemble(query, variables)))
      }
    }

    def simple[F[_]: Monad](
        f: Request[Pure] => F[Either[Response[F], Executable[F, Unit, Unit, Unit]]]
    ) = full[F]((req, _, _) => f(req))

    def simpleSchema[F[_]: Async](
        f: Request[Pure] => F[Either[Response[F], Schema[F, Unit, Unit, Unit]]]
    ) = fullSchema[F](f)

    def authorized[F[_], A](
        authorize: Authorization => F[Option[A]],
        f: (Request[Pure], A) => F[Either[Response[F], Executable[F, Unit, Unit, Unit]]]
    )(implicit F: Monad[F]) = simple[F] { req =>
      req.headers.get[Authorization] match {
        case None => F.pure(Left(Response[F](Status.Forbidden)))
        case Some(auth) =>
          authorize(auth).flatMap {
            case None    => F.pure(Left(Response[F](Status.Unauthorized)))
            case Some(a) => f(req, a)
          }
      }
    }

    def authorizedSchema[F[_], A](
        authorize: Authorization => F[Option[A]],
        f: (Request[Pure], A) => F[Either[Response[F], Schema[F, Unit, Unit, Unit]]]
    )(implicit F: Async[F]) =
      fullSchema[F] { req =>
        req.headers.get[Authorization] match {
          case None => F.pure(Left(Response[F](Status.Forbidden)))
          case Some(auth) =>
            authorize(auth).flatMap {
              case None    => F.pure(Left(Response[F](Status.Unauthorized)))
              case Some(a) => f(req, a)
            }
        }
      }
  }

  def runWithHandler[F[_]](
    handler: Handler[F],
    req: Request[Pure],
    query: String,
    operationName: Option[String],
    variables: Option[Map[String, Json]],
  ) = {
    // gql.parser.parse()
  }

  def simple[F[_]: Monad, Q, M, S](
      schema: Schema[F, Q, M, S],
      handler: Handler[F],
      path: String = "graphql"
  ) = {
    val d = new Http4sDsl[F] {}
    import d._

    HttpRoutes.of[F] { case r @ POST -> Root / `path` =>
      ???
    }
  }
}
