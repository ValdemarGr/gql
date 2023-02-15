package gql.interpreter

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}

// Offers a flat representation of a scope tree of streams
trait StreamScopes[F[_], A] {
  // Awaits the first element of the stream in the parent scope
  // and returns that element and the scope it was reserved in
  def acquireAwait(stream: Stream[F, A], scope: Scope[F]): F[(Scope[F], A)]

  // Gets changes in the entire scope tree
  def changes: Stream[F, Chunk[(Scope[F], A)]]
}

object StreamScopes {
  def apply[F[_], A](takeOne: Boolean)(implicit F: Async[F]) = {
    val qF = Queue.bounded[F, Chunk[(Scope[F], A)]](if (takeOne) 1 else 1024)

    qF.map { q =>
      def acquireAwait0(stream: Stream[F, A], scope: Scope[F]): F[(Scope[F], A)] = {
        F.deferred[(Scope[F], A)].flatMap { head =>
          def publish(idx: Long, a: A, scope: Scope[F]): F[Unit] =
            if (idx === 0L) head.complete((scope, a)).void
            else q.offer(Chunk.singleton((scope, a)))

          val stream0 = if (takeOne) stream.take(1) else stream

          scope.openChild { parentScope =>
            stream0.zipWithIndex
              .evalMap { case (a, i) =>
                F.deferred[Unit].flatMap { d =>
                  parentScope
                    .openChild { _ =>
                      Resource.onFinalize(d.complete(()).void)
                    }
                    .flatMap {
                      case None                  => F.pure(fs2.Stream[F, Unit]())
                      case Some((childScope, _)) => publish(i, a, childScope).as(fs2.Stream.eval(d.get))
                    }
                }
              }
              .parJoinUnbounded
              .compile
              .drain
              .background
          } >> head.get
        }
      }

      new StreamScopes[F, A] {
        def acquireAwait(stream: Stream[F, A], scope: Scope[F]): F[(Scope[F], A)] =
          acquireAwait0(stream, scope)

        def changes: Stream[F, Chunk[(Scope[F], A)]] = Stream.fromQueueUnterminated(q)
      }
    }
  }
}
