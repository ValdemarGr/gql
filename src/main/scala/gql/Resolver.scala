package gql

import cats._
import cats.implicits._

sealed trait Resolver[F[_], I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]
}

object Resolver {
  final case class Pure[F[_], I, A](resolve: I => A) extends Resolver[F, I, A] {
    override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A] =
      Pure(resolve)

    override def contramap[B](g: B => I): Resolver[F, B, A] =
      Pure(g andThen resolve)
  }

  final case class Effect[F[_], I, A](resolve: I => F[A]) extends Resolver[F, I, A] {
    override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A] =
      Effect(resolve.andThen(fk.apply))

    override def contramap[B](g: B => I): Resolver[F, B, A] =
      Effect(g andThen resolve)
  }

  final case class Batcher[F[_], K, T](
      batchName: String,
      resolver: Set[K] => F[Map[K, T]]
  ) {
    def mapK[G[_]: Functor](fk: F ~> G): Batcher[G, K, T] =
      Batcher(batchName, resolver.andThen(fk.apply))
  }

  final case class Batch[F[_], K, A, T](
      keys: List[K],
      post: List[(K, T)] => F[A]
  ) {
    def mapK[G[_]](fk: F ~> G): Batch[G, K, A, T] =
      Batch(keys, post.andThen(fk.apply))
  }

  object Batch {
    implicit def applyForBatch[F[_]: Applicative, K, T] = {
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

  final case class Batched[F[_], I, K, A, T](
      batch: I => F[Batch[F, K, A, T]],
      batcher: Batcher[F, K, T]
  ) extends Resolver[F, I, A] {
    override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A] =
      Batched(batch.andThen(fa => fk(fa).map(_.mapK(fk))), batcher.mapK(fk))

    override def contramap[B](g: B => I): Resolver[F, B, A] =
      Batched(g.andThen(batch), batcher)
  }
}
