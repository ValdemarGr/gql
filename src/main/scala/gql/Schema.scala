package gql

import cats.data._
import alleycats.Empty

final case class Subscription[F[_], I, M](
  name: String,
  fields: NonEmptyList[(String, I => fs2.Stream[F, M], Output.Field[F, M, _, _])]
)

final case class SchemaShape[F[_], Q](
    query: Output.Obj[F, Q],
    // mutation: Output.Obj[F, M],
    // subscription: Output.Obj[F, M],
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
