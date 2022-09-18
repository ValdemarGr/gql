package gql.resolver

import cats.implicits._
import cats._
import cats.data._
import gql._

final case class BatcherReference[K, T](id: Int) {
  def full[F[_], I, A](keys: I => IorT[F, String, Batch[F, K, A, T]])(implicit F: Applicative[F]) =
    BatchResolver[F, I, K, A, T](this, keys)

  def traverse[F[_], G[_]: Traverse, I](keys: I => IorT[F, String, G[K]])(implicit F: Applicative[F]) =
    full[F, I, G[T]] { i =>
      keys(i).map { gk =>
        Batch(
          gk.toList,
          { xs =>
            val arr = xs.toVector
            IorT.pure {
              gk.mapWithIndex { case (_, i) =>
                val (_, v) = xs(i)
                v
              }
            }
          }
        )
      }
    }

  def simple[F[_], I](key: I => IorT[F, String, K])(implicit F: Applicative[F]) =
    traverse[F, Id, I](key)
}

object BatcherReference {
  def apply[F[_], K, T](f: Set[K] => F[Map[K, T]]): State[SchemaState[F], BatcherReference[K, T]] =
    State { s =>
      val id = s.nextId
      val entry = f.asInstanceOf[Set[Any] => F[Map[Any, Any]]]
      (s.copy(nextId = id + 1, batchers = s.batchers + (id -> entry)), BatcherReference[K, T](id))
    }
}
