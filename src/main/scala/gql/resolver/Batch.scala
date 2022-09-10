package gql.resolver

import cats._
import cats.implicits._
import cats.data._

final case class Batch[F[_], K, A, T](
    keys: List[K],
    post: List[(K, T)] => EitherT[F, String, A]
) {
  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]) =
    Batch(keys, post.andThen(_.semiflatMap(f)))
}

object Batch {
  implicit def applicativeForBatchPartition[F[_]: Monad, K, T]: Applicative[Batch[F, K, *, T]] = {
    type G[A] = Batch[F, K, A, T]
    new Applicative[G] {
      override def pure[A](x: A): G[A] = Batch(List.empty, _ => EitherT.pure(x))

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
