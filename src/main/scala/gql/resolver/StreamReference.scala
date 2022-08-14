package gql.resolver

import cats.data._
import cats._
import cats.effect._
import gql.SchemaState

final case class StreamReference[K, T](id: Int)

object StreamReference {
  def apply[F[_], K, T](
      pickSubscription: K => Resource[F, fs2.Stream[F, T]]
  ): State[SchemaState[F], StreamReference[K, T]] =
    State { s =>
      val id = s.nextId
      val entry = pickSubscription.asInstanceOf[Any => Resource[F, fs2.Stream[F, Any]]]
      (s.copy(nextId = id + 1, streams = s.streams + (id -> entry)), StreamReference(id))
    }
}
