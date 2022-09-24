package gql

import gql.execution._
import gql.parser.{QueryParser => P, ParseError, parse}
import io.circe._
import cats.effect._
import cats.implicits._
import cats.data._
import alleycats.Empty
import cats.mtl._
import cats._
import gql.ast._

final case class Schema[F[_], Q, M, S](
    shape: SchemaShape[F, Q, M, S],
    state: SchemaState[F],
    statistics: Statistics[F]
) {
  protected implicit lazy val s = statistics

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G)(implicit F: Functor[F]): Schema[G, Q, M, S] =
    Schema(shape.mapK(fk), state.mapK(fk), statistics.mapK(fk))

  def assemble(query: NonEmptyList[P.ExecutableDefinition], variables: Map[String, Json])(implicit
      F: Async[F]
  ): Executable[F, Q, M, S] =
    Executable.assemble[F, Q, M, S](query, this, variables)

  def assemble(query: String, variables: Map[String, Json])(implicit F: Async[F]): Either[ParseError, Executable[F, Q, M, S]] =
    parse(query).map(assemble(_, variables))

  def assembleMonoid(query: NonEmptyList[P.ExecutableDefinition], variables: Map[String, Json])(implicit
      F: Async[F],
      Q: Monoid[Q],
      M: Monoid[M],
      S: Monoid[S]
  ): AppliedExecutable[F] = assemble(query, variables) match {
    case Executable.Mutation(run)        => AppliedExecutable.Mutation(run(M.empty))
    case Executable.Query(run)           => AppliedExecutable.Query(run(Q.empty))
    case Executable.Subscription(run)    => AppliedExecutable.Subscription(run(S.empty))
    case Executable.ValidationError(msg) => AppliedExecutable.ValidationError(msg)
  }

  def assembleMonoid(query: String, variables: Map[String, Json])(implicit
      F: Async[F],
      Q: Monoid[Q],
      M: Monoid[M],
      S: Monoid[S]
  ): Either[ParseError, AppliedExecutable[F]] =
    parse(query).map(assembleMonoid(_, variables))
}

object Schema {
  def stateful[F[_], Q, M, S](statistics: Statistics[F])(fa: State[SchemaState[F], SchemaShape[F, Q, M, S]]): Schema[F, Q, M, S] = {
    val (state, shape) = fa.run(SchemaState(0, Map.empty)).value
    Schema(shape, state, statistics)
  }

  def stateful[F[_]: Async, Q, M, S](fa: State[SchemaState[F], SchemaShape[F, Q, M, S]]): F[Schema[F, Q, M, S]] =
    Statistics[F].map(stateful(_)(fa))

  def query[F[_], Q](statistics: Statistics[F])(query: Type[F, Q]): Schema[F, Q, Unit, Unit] =
    stateful(statistics)(State.pure(SchemaShape(query, None, None)))

  def query[F[_]: Async, Q](query: Type[F, Q]): F[Schema[F, Q, Unit, Unit]] =
    stateful(State.pure(SchemaShape(query, None, None)))

  def simple[F[_]: Async, Q, M, S](
      query: Type[F, Q],
      mutation: Option[Type[F, M]],
      subscription: Option[Type[F, S]]
  ): F[Schema[F, Q, M, S]] =
    Statistics[F].map { stats =>
      Schema(SchemaShape[F, Q, M, S](query, mutation, subscription), Empty[SchemaState[F]].empty, stats)
    }

  def simple[F[_], Q, M, S](statistics: Statistics[F])(
      query: Type[F, Q],
      mutation: Option[Type[F, M]],
      subscription: Option[Type[F, S]]
  ): Schema[F, Q, M, S] =
    Schema(SchemaShape[F, Q, M, S](query, mutation, subscription), Empty[SchemaState[F]].empty, statistics)
}
