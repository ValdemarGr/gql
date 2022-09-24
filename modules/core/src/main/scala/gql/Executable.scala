package gql

import gql.parser._
import gql.parser.{QueryParser => P}
import fs2.Stream
import cats.implicits._
import cats.effect._
import io.circe._
import io.circe.syntax._
import cats.data._
import gql.interpreter._
import gql.parser.ParserUtil

final case class Executable[F[_]: Async, Q, M, S](
    schema: Schema[F, Q],
    statistics: Statistics[F]
) {
  def execute[O](query: NonEmptyList[P.ExecutableDefinition], variables: Map[String, Json]): ExecutableQuery[F, Q, M, S] = {
    implicit lazy val s = statistics
    ExecutableQuery.assemble[F, Q, M, S](query, schema, variables)
  }

  def execute[O](query: String, variables: Map[String, Json]): Either[ParseError, ExecutableQuery[F, Q, M, S]] =
    parse(query).map(execute(_, variables))
}

object Executable {
  def apply[F[_]: Async, Q, M, S](schema: Schema[F, Q]): F[Executable[F, Q, M, S]] =
    Statistics[F].map(Executable(schema, _))
}

sealed trait ExecutableQuery[F[_], Q, M, S]
object ExecutableQuery {
  case class Result(
      errors: Chain[EvalFailure],
      data: JsonObject
  ) {
    lazy val asGraphQL =
      JsonObject(
        "errors" -> errors.map(_.asGraphQL).asJson,
        "data" -> data.asJson
      )
  }

  final case class ValidationError[F[_], Q, M, S](msg: PreparedQuery.PositionalError) extends ExecutableQuery[F, Q, M, S]
  final case class Query[F[_], Q, M, S](run: Q => F[Result]) extends ExecutableQuery[F, Q, M, S]
  final case class Mutation[F[_], Q, M, S](run: M => F[Result]) extends ExecutableQuery[F, Q, M, S]
  final case class Subscription[F[_], Q, M, S](run: S => fs2.Stream[F, Result]) extends ExecutableQuery[F, Q, M, S]

  def assemble[F[_]: Statistics, Q, M, S](
      query: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, Q],
      variables: Map[String, Json]
  )(implicit F: Async[F]): ExecutableQuery[F, Q, M, S] = {
    PreparedQuery.prepare2(query, schema, variables) match {
      case Left(err) => ExecutableQuery.ValidationError(err)
      case Right(x) =>
        x match {
          case (P.OperationType.Query, rootFields) =>
            ExecutableQuery.Query[F, Q, M, S](
              Interpreter.runSync(_, rootFields, schema.state).map { case (e, d) => Result(e, d) }
            )
          case (P.OperationType.Mutation, rootFields) =>
            ExecutableQuery.Mutation[F, Q, M, S](
              Interpreter.runSync(_, rootFields, schema.state).map { case (e, d) => Result(e, d) }
            )
          case (P.OperationType.Subscription, rootFields) =>
            ExecutableQuery.Subscription[F, Q, M, S](
              Interpreter.runStreamed(_, rootFields, schema.state).map { case (e, d) => Result(e, d) }
            )
        }
    }
  }
}
