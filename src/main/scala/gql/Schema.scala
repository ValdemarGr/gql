package gql

import cats.data._
import alleycats.Empty

final case class SchemaShape[F[_], Q](
    query: Output.Obj[F, Q]
)

final case class Schema[F[_], Q](
    shape: SchemaShape[F, Q],
    state: SchemaState[F]
)

object Schema {
  def simple[F[_], Q](query: Output.Obj[F, Q]) =
    Schema(SchemaShape(query), Empty[SchemaState[F]].empty)

  def stateful[F[_], Q](fa: State[SchemaState[F], SchemaShape[F, Q]]) = {
    val (state, shape) = fa.run(SchemaState(0, Map.empty, Map.empty)).value
    Schema(shape, state)
  }
}
