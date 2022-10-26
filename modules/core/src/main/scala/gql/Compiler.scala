package gql

import cats._
import cats.implicits._
import io.circe._
import cats.effect._
import gql.parser.{QueryParser => P}
import gql.interpreter.Interpreter
import cats.data.NonEmptyList
import gql.parser.QueryParser

sealed trait CompilationError {
  def asGraphQL: JsonObject
}
object CompilationError {
  final case class Parse(error: gql.parser.ParseError) extends CompilationError {
    lazy val asGraphQL: JsonObject = JsonObject("errors" -> Json.arr(Json.fromJsonObject(error.asGraphQL)))
  }
  final case class Preparation(error: gql.PreparedQuery.PositionalError) extends CompilationError {
    lazy val asGraphQL: JsonObject = JsonObject("errors" -> Json.arr(Json.fromJsonObject(error.asGraphQL)))
  }
}

sealed trait Application[F[_]] {
  def mapK[G[_]](f: F ~> G): Application[G]
}
object Application {
  final case class Query[F[_]](run: F[QueryResult]) extends Application[F] {
    override def mapK[G[_]](f: F ~> G): Query[G] = Query(f(run))
  }
  final case class Mutation[F[_]](run: F[QueryResult]) extends Application[F] {
    override def mapK[G[_]](f: F ~> G): Mutation[G] = Mutation(f(run))
  }
  final case class Subscription[F[_]](run: fs2.Stream[F, QueryResult]) extends Application[F] {
    override def mapK[G[_]](f: F ~> G): Subscription[G] = Subscription(run.translate(f))
  }
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
    implicit def F0: Async[F] = F

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
      parsePrep(schema, cp)
        .map { case (ot, pfs) => compilePrepared(schema, ot, pfs, queryInput, mutationInput, subscriptionInput) }

    def parsePrep(
        schema: Schema[F, ?, ?, ?],
        cp: CompilerParameters
    ): Either[CompilationError, (QueryParser.OperationType, NonEmptyList[PreparedQuery.PreparedField[F, Any]])] =
      gql.parser.parse(cp.query) match {
        case Left(pe) => Left(CompilationError.Parse(pe))
        case Right(q) =>
          PreparedQuery.prepare(q, schema, cp.variables.getOrElse(Map.empty), cp.operationName) match {
            case Left(pe) => Left(CompilationError.Preparation(pe))
            case Right(x) => Right(x)
          }
      }

    def compilePrepared[Q, M, S](
        schema: Schema[F, Q, M, S],
        operationType: P.OperationType,
        ps: NonEmptyList[PreparedQuery.PreparedField[F, Any]],
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit
    ) = {
      implicit val s = schema.statistics
      implicit val p = schema.planner

      operationType match {
        case P.OperationType.Query =>
          Application.Query {
            queryInput.flatMap { qi =>
              Interpreter.runSync(qi, ps, schema.state).map { case (e, d) => QueryResult(e, d) }
            }
          }
        case P.OperationType.Mutation =>
          Application.Mutation {
            mutationInput.flatMap { mi =>
              Interpreter.runSync(mi, ps, schema.state).map { case (e, d) => QueryResult(e, d) }
            }
          }
        case P.OperationType.Subscription =>
          Application.Subscription {
            fs2.Stream.eval(subscriptionInput).flatMap { si =>
              Interpreter.runStreamed(si, ps, schema.state).map { case (e, d) => QueryResult(e, d) }
            }
          }
      }
    }
  }

  def apply[F[_]](implicit F: Async[F]): PartiallyAppliedCompiler[F] = new PartiallyAppliedCompiler[F](F)
}
