package gql.resolver

import cats.data._
import cats._
import cats.effect._
import gql.SchemaState

final case class StreamReference[I, A](id: Int)

object StreamReference {
  def apply[F[_], I, A, B](
      subscription: Resource[F, (I, A, B) => fs2.Stream[F, A]]
  ): State[SchemaState[F], StreamReference[I, A]] =
    State { s =>
      val id = s.nextId
      val entry = subscription.asInstanceOf[Resource[F, (Any, Any) => fs2.Stream[F, Any]]]
      (s.copy(nextId = id + 1, streams = s.streams + (id -> entry)), StreamReference(id))
    }
}
