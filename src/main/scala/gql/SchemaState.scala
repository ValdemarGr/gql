package gql

import cats.effect._
import alleycats.Empty

final case class SchemaState[F[_]](
    nextId: Int,
    streams: Map[Int, Any => Resource[F, fs2.Stream[F, Any]]],
    batchers: Map[Int, Set[Any] => F[Map[Any, Any]]]
)

object SchemaState {
  implicit def emptyInstance[F[_]] = Empty[SchemaState[F]](
    SchemaState[F](
      nextId = 0,
      streams = Map.empty,
      batchers = Map.empty
    )
  )
}
