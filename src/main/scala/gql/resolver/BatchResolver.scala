package gql.resolver

import cats.implicits._
import cats._
import cats.effect._

final case class BatchResolver[F[_], I, K, A, T](
    // Pick batch implementation based on input
    batcher: I => BatcherReference[K, T],
    partition: I => F[Batch[F, K, A, T]]
) extends LeafResolver[F, I, A] {
  def flatMapF[B](f: A => F[B])(implicit F: FlatMap[F]) =
    BatchResolver(batcher, partition.andThen(_.map(_.flatMapF(f))))

  def contraMap[B](g: B => I): BatchResolver[F, B, K, A, T] =
    BatchResolver(i => batcher(g(i)), g.andThen(partition))

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): BatchResolver[G, I, K, A, T] =
    BatchResolver(batcher, partition.andThen(fa => fk(fa).map(bp => bp.copy(post = bp.post.andThen(fk.apply)))))
}
