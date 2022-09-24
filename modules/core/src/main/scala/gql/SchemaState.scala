package gql

import cats._
import cats.effect._
import alleycats.Empty

final case class SchemaState[F[_]](
    nextId: Int,
    batchers: Map[Int, Set[Any] => F[Map[Any, Any]]]
) {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SchemaState[G] =
    SchemaState(nextId, batchers.map { case (k, v) => k -> (v andThen fk.apply) })
}

object SchemaState {
  implicit def emptyInstance[F[_]] = Empty[SchemaState[F]](
    SchemaState[F](nextId = 0, batchers = Map.empty)
  )
}
