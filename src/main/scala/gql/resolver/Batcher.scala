package gql.resolver

import cats._
import cats.data._

final case class Batcher[K, T](id: Int)

object Batcher {
  def apply[F[_], K, T](f: Set[K] => F[Map[K, T]]): State[BatchingState[F], Batcher[K, T]] =
    State[BatchingState[F], Batcher[K, T]] { s =>
      val id = s.nextId
      val entry: Set[Any] => F[Map[Any, Any]] = f.asInstanceOf[Set[Any] => F[Map[Any, Any]]]
      (s.copy(nextId = s.nextId + 1, batchers = s.batchers + (id -> entry)), Batcher[K, T](id))
    }
}
