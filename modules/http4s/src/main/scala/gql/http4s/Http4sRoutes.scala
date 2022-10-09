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

  def runWithHandler[F[_]](
      handler: Http4sCompiler[F],
      req: Request[Pure],
      query: String,
      operationName: Option[String],
      variables: Option[Map[String, Json]]
  ) = {
    // gql.parser.parse()
  }

  def simple[F[_]: Monad, Q, M, S](
      schema: Schema[F, Q, M, S],
      handler: Http4sCompiler[F],
      path: String = "graphql"
  ) = {
    val d = new Http4sDsl[F] {}
    import d._

    HttpRoutes.of[F] { case r @ POST -> Root / `path` =>
      ???
    }
  }
}
