package gql

import cats.data._
import alleycats.Empty
import gql.out._

final case class SchemaShape[F[_], Q](
    query: Obj[F, Q]
    // mutation: Output.Obj[F, M],
    // subscription: Output.Obj[F, M],
)

final case class Schema[F[_], Q](
    shape: SchemaShape[F, Q],
    state: SchemaState[F]
)

object Schema {
  def simple[F[_], Q](query: Obj[F, Q]) =
    Schema(SchemaShape(query), Empty[SchemaState[F]].empty)

  def stateful[F[_], Q](fa: State[SchemaState[F], SchemaShape[F, Q]]) = {
    val (state, shape) = fa.run(SchemaState(0, Map.empty, Map.empty)).value
    Schema(shape, state)
  }
}
