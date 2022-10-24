package gql.http4s

import cats.effect._
import cats._
import org.http4s._
import gql._
import cats.implicits._

final case class Http4sCompilerParametes(
    compilerParameters: CompilerParameters,
    headers: Headers
)

trait Http4sCompiler[F[_]] {
  def compile(params: Http4sCompilerParametes): F[Either[Response[F], Application[F]]]
}

object Http4sCompiler {
  def apply[F[_]](compiler: Http4sCompilerParametes => F[Either[Response[F], Compiler.Outcome[F]]])(implicit
      F: Monad[F]
  ): Http4sCompiler[F] = compiler(_).flatMap(_.flatTraverse(serializeOutcomeErrors[F]))

  def serializeOutcomeErrors[F[_]](outcome: Compiler.Outcome[F])(implicit F: Applicative[F]): F[Either[Response[F], Application[F]]] = {
    val dsl = new org.http4s.dsl.Http4sDsl[F] {}
    import dsl._
    import org.http4s.circe._
    import io.circe.syntax._

    outcome match {
      case Left(compErr) =>
        Ok {
          compErr match {
            case CompilationError.Parse(pe)       => pe.asGraphQL.asJson
            case CompilationError.Preparation(pe) => pe.asGraphQL.asJson
          }
        }.map(_.asLeft)
      case Right(application) => F.pure(Right(application))
    }
  }

  def makeFromCompiler[F[_]](compiler: Http4sCompilerParametes => F[Either[Response[F], Compiler[F]]])(implicit
      F: Async[F]
  ): Http4sCompiler[F] =
    apply[F](params => compiler(params).flatMap(_.traverse(_.compile(params.compilerParameters))))

  def fromCompiler[F[_]](compiler: Compiler[F])(implicit
      F: Async[F]
  ): Http4sCompiler[F] =
    makeFromCompiler[F]((_: Http4sCompilerParametes) => F.pure(compiler.asRight))
}
