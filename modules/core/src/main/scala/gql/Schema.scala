package gql

import cats.implicits._
import cats.data._
import alleycats.Empty
import cats.mtl._
import cats._
import gql.ast._

final case class Schema[F[_], Q](
    shape: SchemaShape[F, Q],
    state: SchemaState[F]
)

object Schema {
  def simple[F[_], Q](query: Type[F, Q]) =
    Schema(SchemaShape(query), Empty[SchemaState[F]].empty)

  def stateful[F[_], Q](fa: State[SchemaState[F], SchemaShape[F, Q]]) = {
    val (state, shape) = fa.run(SchemaState(0, Map.empty)).value
    Schema(shape, state)
  }
}
