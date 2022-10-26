package gql

import cats._
import gql.resolver.BatchResolver

final case class SchemaState[F[_]](
    nextId: Int,
    batchers: Map[BatchResolver.ResolverKey, Set[Any] => F[Map[Any, Any]]]
) {
  def mapK[G[_]](fk: F ~> G): SchemaState[G] =
    SchemaState(nextId, batchers.map { case (k, v) => k -> (v andThen fk.apply) })
}

object SchemaState {
  def empty[F[_]] = SchemaState[F](nextId = 0, batchers = Map.empty)
}
