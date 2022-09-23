package gql.interpreter

import cats.implicits._
import cats.effect.implicits._
import gql.resolver._
import cats.data._
import gql._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream, Pull}

trait StreamSupervisor[F[_], A] {
  def acquireAwait(stream: Stream[F, A]): F[(Unique.Token, Either[Throwable, A])]

  def release(token: Unique.Token): F[Unit]

  def changes: Stream[F, NonEmptyList[(Unique.Token, Either[Throwable, A])]]
}

object StreamSupervisor {
  def apply[F[_], A](openTail: Boolean)(implicit F: Concurrent[F]): Stream[F, StreamSupervisor[F, A]] = {
    fs2.Stream
      .bracket(F.ref(Map.empty[Unique.Token, F[Unit]]))(_.get.flatMap(_.values.toList.sequence_))
      .flatMap { state =>
        fs2.Stream.eval(Queue.bounded[F, Chunk[(Unique.Token, Either[Throwable, A])]](1024)).map { q =>
          new StreamSupervisor[F, A] {
            override def acquireAwait(stream: Stream[F, A]): F[(Unique.Token, Either[Throwable, A])] =
              for {
                token <- F.unique
                head <- F.deferred[Either[Throwable, A]]
                close <-
                  // Some danger here
                  // We must NOT start the background stream outside of the main stream since that would escape the scope (as in fs2 `Scope`) the main stream
                  // Instead we use the resource compiler to do so
                  stream.attempt.pull.uncons1
                    .flatMap {
                      case None => ???
                      case Some((hd, tl)) =>
                        val back =
                          if (openTail)
                            tl.map(e => (token, e)).enqueueUnterminatedChunks(q).pull.echo
                          else Pull.done
                        Pull.eval(head.complete(hd)) >> back
                    }
                    .stream
                    .compile
                    .drain
                    .start
                    .map(_.cancel)
                head <- head.get
                _ <- state.update(_ + (token -> close))
              } yield (token, head)

            override def release(token: Unique.Token): F[Unit] =
              state.modify(m => (m - token, m.get(token))).flatMap(_.sequence_)

            override def changes: Stream[F, NonEmptyList[(Unique.Token, Either[Throwable, A])]] =
              Stream
                .fromQueueUnterminatedChunk(q)
                .chunks
                .map(_.toNel)
                .unNone
          }
        }
      }
  }
}
