package gql.resolver

import cats._
import cats.implicits._

final case class Batch[F[_], K, A, T](
    keys: List[K],
    post: List[(K, T)] => F[A]
) {
  def flatMapF[B](f: A => F[B])(implicit F: FlatMap[F]) =
    Batch(keys, post.andThen(_.flatMap(f)))
}

object BatchPartition {
  implicit def applicativeForBatchPartition[F[_]: Applicative, K, T]: Applicative[Batch[F, K, *, T]] = {
    type G[A] = Batch[F, K, A, T]
    new Applicative[G] {
      override def pure[A](x: A): G[A] = Batch(List.empty, _ => x.pure[F])

      override def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] =
        Batch(
          ff.keys ++ fa.keys,
          { m =>
            val f = ff.post(m.take(ff.keys.size))
            val a = fa.post(m.drop(ff.keys.size))
            f.ap(a)
          }
        )
    }
  }
}
