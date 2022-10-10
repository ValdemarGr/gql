package gql

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
    statistics: Statistics[F],
    planner: Planner[F]
) {
  protected implicit lazy val s = statistics

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G)(implicit F: Functor[F]): Schema[G, Q, M, S] =
    Schema(shape.mapK(fk), state.mapK(fk), statistics.mapK(fk), planner.mapK(fk))

  lazy val validate: Chain[SchemaShape.Problem] = shape.validate

  lazy val render: String = shape.render
}

object Schema {
  def stateful[F[_]: Applicative, Q, M, S](
      statistics: Statistics[F]
  )(fa: State[SchemaState[F], SchemaShape[F, Q, M, S]]): Schema[F, Q, M, S] = {
    val (state, shape) = fa.run(SchemaState(0, Map.empty)).value
    Schema(shape, state, statistics, Planner[F])
  }

  def stateful[F[_]: Async, Q, M, S](fa: State[SchemaState[F], SchemaShape[F, Q, M, S]]): F[Schema[F, Q, M, S]] =
    Statistics[F].map(stateful(_)(fa))

  def query[F[_]: Applicative, Q](statistics: Statistics[F])(query: Type[F, Q]): Schema[F, Q, Unit, Unit] =
    stateful(statistics)(State.pure(SchemaShape(Some(query), None, None)))

  def query[F[_]: Async, Q](query: Type[F, Q]): F[Schema[F, Q, Unit, Unit]] =
    stateful(State.pure(SchemaShape(Some(query), None, None)))

  def simple[F[_]: Async, Q, M, S](shape: SchemaShape[F, Q, M, S]): F[Schema[F, Q, M, S]] =
    Statistics[F].map(Schema(shape, Empty[SchemaState[F]].empty, _, Planner[F]))

  def simple[F[_]: Applicative, Q, M, S](statistics: Statistics[F])(shape: SchemaShape[F, Q, M, S]): Schema[F, Q, M, S] =
    Schema(shape, Empty[SchemaState[F]].empty, statistics, Planner[F])
}
