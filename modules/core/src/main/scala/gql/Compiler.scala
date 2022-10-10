package gql

import cats.implicits._
import io.circe._
import cats.effect._
import gql.parser.{QueryParser => P}
import gql.interpreter.Interpreter

sealed trait CompilationError
object CompilationError {
  final case class Parse(error: gql.parser.ParseError) extends CompilationError
  final case class Preparation(error: gql.PreparedQuery.PositionalError) extends CompilationError
}

sealed trait Application[F[_]]
object Application {
  final case class Query[F[_]](run: F[QueryResult]) extends Application[F]
  final case class Mutation[F[_]](run: F[QueryResult]) extends Application[F]
  final case class Subscription[F[_]](run: fs2.Stream[F, QueryResult]) extends Application[F]
}

final case class CompilerParameters(
    query: String,
    variables: Option[Map[String, Json]],
    operationName: Option[String]
)

trait Compiler[F[_]] {
  def compile(params: CompilerParameters): F[Either[CompilationError, Application[F]]]
}

object Compiler { outer =>
  type Outcome[F[_]] = Either[CompilationError, Application[F]]

  final class PartiallyAppliedCompiler[F[_]](val F: Async[F]) extends AnyVal {
    def discard[A]: A => F[Unit] = _ => F.unit

    implicit def F0 = F

    def make[Q, M, S](
        schema: Schema[F, Q, M, S],
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit
    ): Compiler[F] =
      cp => F.pure(compileWith(schema, cp, queryInput, mutationInput, subscriptionInput))

    def compile[Q, M, S](
        schema: Schema[F, Q, M, S],
        query: String,
        variables: Map[String, Json] = Map.empty,
        operationName: Option[String] = None,
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit
    ) = compileWith(schema, CompilerParameters(query, Some(variables), operationName), queryInput, mutationInput, subscriptionInput)

    def compileWith[Q, M, S](
        schema: Schema[F, Q, M, S],
        cp: CompilerParameters,
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit
    ): Outcome[F] =
      gql.parser.parse(cp.query) match {
        case Left(pe) => Left(CompilationError.Parse(pe))
        case Right(q) =>
          PreparedQuery.prepare2(q, schema, cp.variables.getOrElse(Map.empty)) match {
            case Left(pe) => Left(CompilationError.Preparation(pe))
            case Right((ot, exec)) =>
              implicit val s = schema.statistics
              implicit val p = schema.planner

              ot match {
                case P.OperationType.Query =>
                  Right(Application.Query {
                    queryInput.flatMap { qi =>
                      Interpreter.runSync(qi, exec, schema.state).map { case (e, d) => QueryResult(e, d) }
                    }
                  })
                case P.OperationType.Mutation =>
                  Right(Application.Mutation {
                    mutationInput.flatMap { mi =>
                      Interpreter.runSync(mi, exec, schema.state).map { case (e, d) => QueryResult(e, d) }
                    }
                  })
                case P.OperationType.Subscription =>
                  Right(Application.Subscription {
                    fs2.Stream.eval(subscriptionInput).flatMap { si =>
                      Interpreter.runStreamed((), exec, schema.state).map { case (e, d) => QueryResult(e, d) }
                    }
                  })
              }
          }
      }

  }

  def apply[F[_]](implicit F: Async[F]): PartiallyAppliedCompiler[F] = new PartiallyAppliedCompiler[F](F)
}
