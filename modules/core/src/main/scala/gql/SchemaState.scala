package gql

import cats._
import alleycats.Empty
import gql.resolver.BatchResolver

final case class SchemaState[F[_]](
    nextId: Int,
    batchers: Map[BatchResolver.ResolverKey, Set[Any] => F[Map[Any, Any]]]
) {
  def mapK[G[_]](fk: F ~> G): SchemaState[G] =
    SchemaState(nextId, batchers.map { case (k, v) => k -> (v andThen fk.apply) })
}

object SchemaState {
  implicit def emptyInstance[F[_]] = Empty[SchemaState[F]](
    SchemaState[F](nextId = 0, batchers = Map.empty)
  )
}
