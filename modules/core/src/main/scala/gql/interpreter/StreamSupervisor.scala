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
                // head <- F.deferred[Either[Throwable, A]]
                (head, close) <-
                  // stream
                  //   .attempt
                  //   .pull
                  //   .uncons1
                  //   .flatMap{
                  //     case None => ???
                  //     case Some((hd, tl)) =>
                  //       ???
                  //       // Pull.output1(hd).flatMap{ hd =>
                  //       // }>> tl.pull.echo
                  //   }
                  //   .head
                  //   .compile
                  //   .resource
                  //   .lastOrError
                  //   .allocated
                  fs2.Stream
                    .eval(F.deferred[Either[Throwable, A]])
                    .flatMap{ d =>
                      fs2.Stream.eval(d.get).concurrently {
                        stream.attempt
                          .evalTap(d.complete)
                          .tail
                          .through(stream =>
                            if (openTail) stream.map((token, _)).enqueueUnterminatedChunks(q) else fs2.Stream.never[F] ++ stream
                          )
                      }
                    }
                    .compile
                    .resource
                    .lastOrError
                    .allocated
                // .attempt.pull.uncons1
                // .flatMap {
                //   case None => ???
                //   case Some((hd, tl)) =>
                //     val back =
                //       if (openTail)
                //         tl.map(e => (token, e)).enqueueUnterminatedChunks(q).pull.echo
                //       else Pull.done
                //     Pull.eval(head.complete(hd)) >> back
                // }
                // .stream
                // .compile
                // .resource
                // .drain
                // .allocated
                // head <- head.get
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
