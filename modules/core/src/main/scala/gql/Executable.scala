package gql

import cats._
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

sealed trait Executable[F[_], Q, M, S] {
  def mapK[G[_]](fk: F ~> G): Executable[G, Q, M, S] =
    this match {
      case Executable.ValidationError(msg) => Executable.ValidationError(msg)
      case Executable.Query(run)           => Executable.Query(run.andThen(fk.apply))
      case Executable.Mutation(run)        => Executable.Mutation(run.andThen(fk.apply))
      case Executable.Subscription(run)    => Executable.Subscription(run.andThen(_.translate(fk)))
    }

  def contramap[Q2, M2, S2](
      onQuery: Q2 => F[Q],
      onMutation: M2 => F[M],
      onSubscription: S2 => F[S]
  )(implicit F: Monad[F]): Executable[F, Q2, M2, S2] = this match {
    case Executable.ValidationError(msg) => Executable.ValidationError(msg)
    case Executable.Query(run)           => Executable.Query(q => onQuery(q) >>= run)
    case Executable.Mutation(run)        => Executable.Mutation(m => onMutation(m) >>= run)
    case Executable.Subscription(run)    => Executable.Subscription(s => fs2.Stream.eval(onSubscription(s)) >>= run)
  }

  def applyQuery(onQuery: F[Q])(implicit F: Monad[F]): Executable[F, Unit, M, S] =
    contramap[Unit, M, S](_ => onQuery, F.pure, F.pure)

  def applyMutation(onMutation: F[M])(implicit F: Monad[F]): Executable[F, Q, Unit, S] =
    contramap[Q, Unit, S](F.pure, _ => onMutation, F.pure)

  def applySubscription(onSubscription: F[S])(implicit F: Monad[F]): Executable[F, Q, M, Unit] =
    contramap[Q, M, Unit](F.pure, F.pure, _ => onSubscription)

  def applyCase(
      onQuery: F[Q],
      onMutation: F[M],
      onSubscription: F[S]
  )(implicit F: Monad[F]): Executable[F, Unit, Unit, Unit] =
    contramap[Unit, Unit, Unit](_ => onQuery, _ => onMutation, _ => onSubscription)
}

object Executable {
  final case class ValidationError[F[_], Q, M, S](msg: PreparedQuery.PositionalError) extends Executable[F, Q, M, S]
  final case class Query[F[_], Q, M, S](run: Q => F[QueryResult]) extends Executable[F, Q, M, S]
  final case class Mutation[F[_], Q, M, S](run: M => F[QueryResult]) extends Executable[F, Q, M, S]
  final case class Subscription[F[_], Q, M, S](run: S => fs2.Stream[F, QueryResult]) extends Executable[F, Q, M, S]

  def assemble[F[_]: Statistics, Q, M, S](
      query: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, Q, M, S],
      variables: Map[String, Json]
  )(implicit F: Async[F]): Executable[F, Q, M, S] = {
    PreparedQuery.prepare2(query, schema, variables) match {
      case Left(err) => Executable.ValidationError(err)
      case Right(x) =>
        x match {
          case (P.OperationType.Query, rootFields) =>
            Executable.Query[F, Q, M, S](
              Interpreter.runSync(_, rootFields, schema.state).map { case (e, d) => QueryResult(e, d) }
            )
          case (P.OperationType.Mutation, rootFields) =>
            Executable.Mutation[F, Q, M, S](
              Interpreter.runSync(_, rootFields, schema.state).map { case (e, d) => QueryResult(e, d) }
            )
          case (P.OperationType.Subscription, rootFields) =>
            Executable.Subscription[F, Q, M, S](
              Interpreter.runStreamed(_, rootFields, schema.state).map { case (e, d) => QueryResult(e, d) }
            )
        }
    }
  }
}
