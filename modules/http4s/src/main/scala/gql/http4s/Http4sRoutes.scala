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

object Http4sRoutes {
  final case class Http4sCompilerParametes(
      compilerParameters: CompilerParameters,
      headers: Headers
  )

  trait Http4sCompiler[F[_]] {
    def compile(params: Http4sCompilerParametes): F[Either[Response[F], Application[F]]]
  }

  object Http4sCompiler {
    def apply[F[_]](compiler: Http4sCompilerParametes => F[Either[Response[F], Compiler.Outcome[F]]])(implicit
        F: Async[F]
    ): Http4sCompiler[F] =
      new Http4sCompiler[F] {
        def compile(params: Http4sCompilerParametes): F[Either[Response[F], Application[F]]] = {
          val dsl = new org.http4s.dsl.Http4sDsl[F] {}
          import dsl._
          import org.http4s.circe._
          import io.circe.syntax._

          compiler(params).flatMap(_.flatTraverse {
            case Left(compErr) =>
              compErr match {
                case CompilationError.Parse(pe)       => BadRequest(pe.asGraphQL.asJson).map(_.asLeft)
                case CompilationError.Preparation(pe) => BadRequest(pe.asGraphQL.asJson).map(_.asLeft)
              }
            case Right(application) => F.pure(Right(application))
          })
        }
      }

    def fromCompiler[F[_]](compiler: Http4sCompilerParametes => F[Either[Response[F], Compiler[F]]])(implicit
        F: Async[F]
    ): Http4sCompiler[F] =
      apply[F](params => compiler(params).flatMap(_.traverse(_.compile(params.compilerParameters))))
  }

  def simple[F[_], Q, M, S](
      schema: Schema[F, Q, M, S],
      compiler: Http4sCompiler[F],
      path: String = "graphql"
  )(implicit F: Concurrent[F]) = {
    val d = new Http4sDsl[F] {}
    import d._
    import io.circe.syntax._
    import org.http4s.circe._
    implicit lazy val cd = io.circe.generic.semiauto.deriveDecoder[CompilerParameters]
    implicit lazy val ed = org.http4s.circe.jsonOf[F, CompilerParameters]

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
}
