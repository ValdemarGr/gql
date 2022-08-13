package gql

import cats._
import cats.implicits._
import cats.effect._
import cats.data.State

sealed trait Resolver[F[_], I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]
}

object Resolver {
  sealed trait LeafResolver[F[_], I, A] extends Resolver[F, I, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): LeafResolver[G, I, A]

    override def contramap[B](g: B => I): LeafResolver[F, B, A]
  }

  /*
   * Signal
   * Simplest type would be input => stream of output value
   *
   * But what if we'd like to include some kind of batching? what does that even mean?
   *
   * Naively every output in the stream triggers a *unique* re-evaluation of the subtree
   * Consequently this is not batchable, even though there may be multiple nodes that react to the same data.
   *
   * Some choices can be made:
   *
   * Do we require the user to "push" the signal up to the common ancestor?
   *
   * Maybe we can generate a key from the input that tells the interpreter what streams are equivalent,
   * which in turn can be used to group updates?
   * This seems like it would be a lot of trouble to both implement and reason with.
   *
   * How do we track these updates? Maybe we have a map Map[K, List[StreamData]]
   * where StreamData is the field information and such.
   * Then we must await the change in all List[StreamData].size elements.
   * There is a lot of concurrency problems involved.
   * For instance, what happens if two nodes get the same element concurrently?
   * Maybe we have a map Map[K, Int] of generations of K? such that we can zipWithIndex and only cause updates to generations
   * older than our index?
   *
   * Maybe we assume that the streams are the same, thus they share the subscription?
   * Using this model, we must decompose the (potential) effect performed by the stream such that we can avoid
   * the N+1 problem (every instance of the same stream performing the same effect for every element).
   *
   * This might also be solvable with just a cache?
   * One might use some data generation technique and query the cache with type Map[K, F[A]] where F[A]
   * is some uncompleted promise.
   * I think this caching solution is the most feasible.
   *
   * Batching different keys (say, within a time period or as a debounce) can be implemented by the end-user.
   * For instance, the end-user can provide some state shared by the whole subscription query that
   * uses some structure to block streams until the timeout has elapsed.
   *
   * After more thought:
   * Deduplication can be solved by a cache.
   *
   * Current problem is:
   * How to batch updates.
   * Clearly we need some sort of strategy, which could be something like fs2.Pipe[F, A, A]
   *
   */

  /*
   * Okay so there are two cases:
   *   Same subscription:
   *     Two nodes that over an infinite time period will contain the exact same sequence of data.
   *     The problem here is identifying two "same" nodes, which only the user can do.
   *
   *     Solution 1:
   *       Have the user provide an id for the subscription, that can be used to identify same subscriptions,
   *       such that the interpreter can de-duplicate the streams of data.
   *       If the subtree is pure, that is, there are no derived effects from this node's output, this optimization
   *       seems quite complex compared to just using a cache.
   *       However, let E be the set of side effects (the subtree of fields that require evaluation)
   *       caused by new data arriving in the stream. Now let there be n instances of the same stream.
   *       Either the end-user has to provide O(|E|) cache implementations to de-duplicate the updates,
   *       or suffer the N+1 problem since if |E| = n then O(n * |E|) = O(n^2).
   *
   */
  final case class Signal[F[_]: MonadCancelThrow, I, A](
      head: LeafResolver[F, I, A],
      tail: Resource[F, (A, I) => fs2.Stream[F, A]]
  )
  // ) extends Resolver[F, I, A] {
  //   override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Resolver[G, I, A] =
  //     Signal(head.mapK(fk), tail.mapK(fk).map(f => (a, i) => f(a, i).translate(fk)))

  //   override def contramap[B](g: B => I): Resolver[F, B, A] =
  //     Signal(head.contramap(g), tail.map(f => (a, i) => f(a, g(i))))
  // }

  final case class Pure[F[_], I, A](resolve: I => A) extends LeafResolver[F, I, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Pure[G, I, A] =
      Pure(resolve)

    override def contramap[B](g: B => I): Pure[F, B, A] =
      Pure(g andThen resolve)
  }

  final case class Effect[F[_], I, A](resolve: I => F[A]) extends LeafResolver[F, I, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Effect[G, I, A] =
      Effect(resolve.andThen(fk.apply))

    override def contramap[B](g: B => I): Effect[F, B, A] =
      Effect(g andThen resolve)
  }

  final case class Batcher[F[_], K, T](
      batchName: String,
      resolver: Set[K] => F[Map[K, T]]
  ) {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Batcher[G, K, T] =
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
  ) extends LeafResolver[F, I, A] {
    override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Batched[G, I, K, A, T] =
      Batched(batch.andThen(fa => fk(fa).map(_.mapK(fk))), batcher.mapK(fk))

    override def contramap[B](g: B => I): Batched[F, B, K, A, T] =
      Batched(g.andThen(batch), batcher)
  }

  object Signal2 {
    final case class DataStream[F[_], I, A](
        id: Int,
        tail: Resource[F, (I, A) => fs2.Stream[F, A]]
    )

    object DataStream {
      val S = State.get[Int]

      def apply[F[_], I, A](f: Resource[F, (I, A) => fs2.Stream[F, A]]): State[Int, DataStream[F, I, A]] =
        S.inspect(DataStream(_, f)) <* S.modify(_ + 1)
    }

    final case class Stuff[F[_], I, A, B](
        head: LeafResolver[F, I, A],
        tail: DataStream[F, I, A],
        post: (I, A) => F[B]
    ) {
      def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): Stuff[F, I, A, C] =
        Stuff(head, tail, (i, a) => post(i, a).flatMap(f))
    }
  }

  object Batch2 {
    final case class Batcher[F[_], K, T](
        id: Int,
        resolver: Set[K] => F[Map[K, T]]
    )

    object Batcher {
      val S = State.get[Int]

      def apply[F[_], K, T](f: Set[K] => F[Map[K, T]]): State[Int, Batcher[F, K, T]] =
        S.inspect(Batcher(_, f)) <* S.modify(_ + 1)
    }

    final case class BatchResult[F[_], K, A, T](
        keys: List[K],
        post: List[(K, T)] => F[A]
    ) {
      def flatMapF[B](f: A => F[B])(implicit F: FlatMap[F]) =
        BatchResult(keys, post.andThen(_.flatMap(f)))
    }

    object BatchResult {
      implicit def applicativeForBatchResult[F[_]: Applicative, K, T]: Applicative[BatchResult[F, K, *, T]] = {
        type G[A] = BatchResult[F, K, A, T]
        new Applicative[G] {
          override def pure[A](x: A): G[A] = BatchResult(List.empty, _ => x.pure[F])

          override def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] =
            BatchResult(
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

    final case class Batch[F[_], I, K, A, T](
        batcher: Batcher[F, K, T],
        run: I => F[BatchResult[F, K, A, T]]
    ) {
      def flatMapF[B](f: A => F[B])(implicit F: FlatMap[F]) =
        Batch(batcher, run.andThen(_.map(_.flatMapF(f))))

      def contraMap[B](g: B => I): Batch[F, B, K, A, T] =
        Batch(batcher, g.andThen(run))
    }
  }
}
