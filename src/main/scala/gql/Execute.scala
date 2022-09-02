package gql

import fs2.Stream
import cats.implicits._
import cats.effect._
import io.circe._
import cats.data._
import gql.interpreter.Interpreter

object Execute {
  sealed trait ExecutionOutcome[F[_]]
  object ExecutionOutcome {
    final case class ValidationError[F[_]](message: String) extends ExecutionOutcome[F]
    final case class StaticOutcome[F[_]](out: JsonObject) extends ExecutionOutcome[F]
    final case class StreamOutcome[F[_]](out: Stream[F, JsonObject]) extends ExecutionOutcome[F]
  }

  // todo handle all elements in executable definition together
  def execute[F[_], Q, M, S](
      query: NonEmptyList[GQLParser.ExecutableDefinition],
      schema: Schema[F, Q],
      variables: Map[String, Json],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]) = {
    PreparedQuery.prepare2(query, schema, variables) match {
      case Left(err) => F.pure(Left(err))
      case Right(x) =>
        x match {
          case PreparedQuery.StaticOrStream.Static(rootFields) =>
            Interpreter
          case PreparedQuery.StaticOrStream.Stream(stream, root) =>
        }
      // Interpreter.run
    }
  }
}
