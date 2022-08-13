package gql.resolver

import cats._
import cats.data._
import gql.SchemaState

final case class BatcherReference[K, T](id: Int)

object BatcherReference {
  def apply[F[_], K, T](f: Set[K] => F[Map[K, T]]): State[SchemaState[F], BatcherReference[K, T]] =
    State { s =>
      val id = s.nextId
      val entry = f.asInstanceOf[Set[Any] => F[Map[Any, Any]]]
      (s.copy(nextId = id + 1, batchers = s.batchers + (id -> entry)), BatcherReference[K, T](id))
    }
}
