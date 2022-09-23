package gql

import cats.effect._
import alleycats.Empty

final case class SchemaState[F[_]](
    nextId: Int,
    batchers: Map[Int, Set[Any] => F[Map[Any, Any]]]
)

object SchemaState {
  implicit def emptyInstance[F[_]] = Empty[SchemaState[F]](
    SchemaState[F](nextId = 0, batchers = Map.empty)
  )
}
