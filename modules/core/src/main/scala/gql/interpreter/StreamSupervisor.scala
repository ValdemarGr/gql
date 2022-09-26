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
  def acquireAwait(stream: Stream[F, A]): F[(StreamToken, Either[Throwable, A])]

  def release(token: Set[StreamToken]): F[Unit]

  def freeUnused(token: StreamToken, resource: ResourceToken): F[Unit]

  def changes: Stream[F, NonEmptyList[(StreamToken, ResourceToken, Either[Throwable, A])]]
}

object StreamSupervisor {
  final case class State[F[_]](
      unsubscribe: F[Unit],
      allocatedResources: Vector[(ResourceToken, F[Unit])]
  )

  def apply[F[_], A](openTail: Boolean)(implicit F: Async[F]): Stream[F, StreamSupervisor[F, A]] = {
    def removeStates(xs: List[State[F]]) = {
      xs.traverse_(_.unsubscribe) >>
        xs.traverse_(_.allocatedResources.toList.traverse_ { case (_, fa) => fa })
    }

    type StateMap = Map[StreamToken, State[F]]
    fs2.Stream
      .bracket(F.ref(Map.empty[StreamToken, State[F]]))(_.get.flatMap(xs => removeStates(xs.values.toList)))
      .flatMap { (state: Ref[F, StateMap]) =>
        fs2.Stream.eval(Queue.bounded[F, Chunk[(StreamToken, ResourceToken, Either[Throwable, A])]](1024)).map { q =>
          new StreamSupervisor[F, A] {
            override def acquireAwait(stream: Stream[F, A]): F[(Unique.Token, Either[Throwable, A])] =
              for {
                token <- F.unique

                head <- F.deferred[Either[Throwable, A]]

                // TODO find a more elegant way of holding fs2 Leases
                start <- F.deferred[Unit]

                // Hold fs2 leases by:
                // 1. allocate a token for the resource and a killSignal / releaseSignal
                // 2. do data publishing
                // 3. await one killSignal for every stream element/resource scope
                close <- {
                  // def goRec(stream: Stream[F, Either[Throwable, A]], idx: Long): Pull[F, Nothing, Unit] =
                  //   stream.pull.uncons1
                  //     .flatMap {
                  //       case None => Pull.done
                  //       case Some((x, xs)) =>
                  //         Pull.eval(F.deferred[Unit]).flatMap { ks =>
                  //           Pull.eval(F.unique).flatMap { resourceToken =>
                  //             val release = ks.complete(()).void
                  //             val await = ks.get
                  //             val lease =
                  //             Pull
                  //               .extendScopeTo(fs2.Stream.eval(await))
                  //               .evalMap(_.compile.drain.start)

                  //             val updateState =
                  //               Pull.eval {
                  //                 state.modify { m =>
                  //                   m.get(token) match {
                  //                     // We are closed for business
                  //                     case None => (m, release)
                  //                     case Some(state) =>
                  //                       val resourceEntry = (resourceToken, release)
                  //                       val newEntry = state.copy(
                  //                         allocatedResources = state.allocatedResources :+ resourceEntry
                  //                       )
                  //                       val emit =
                  //                         if (idx == 0) head.complete(x).void
                  //                         else if (openTail) q.offer(Chunk((token, resourceToken, x)))
                  //                         else F.unit

                  //                       (m + (token -> newEntry), emit)
                  //                   }
                  //                 }.flatten
                  //               }

                  //             lease >> updateState >> goRec(xs, idx + 1)
                  //           }
                  //         }
                  //     }

                  fs2.Stream.eval(start.get) >> {
                    stream
                      .attempt
                      // .pull
                      // .uncons1
                      // .flatMap {
                      //   case None => ???
                      //   case Some((hd, tl)) =>
                      //     Pull.eval(head.complete(hd)) >> tl.map((token, token, _)).enqueueUnterminatedChunks(q).pull.echo
                      // }
                      // .stream
                      // .pull
                      // .uncons1
                      // .flatMap {
                      //   case None           => ???
                      //   case Some((hd, tl)) => goRec(tl.cons1(hd), 0)
                      // }
                      // .stream
                      .zipWithIndex
                      .evalMap { case (a, i) =>
                        F.deferred[Unit].flatMap { killSignal =>
                          F.unique.flatMap { resourceToken =>
                            state.update { m =>
                              m.get(token) match {
                                // We are closed for business
                                case None => m
                                case Some(state) =>
                                  val resourceEntry = (resourceToken, killSignal.complete(()).void)
                                  val newEntry = state.copy(
                                    allocatedResources = state.allocatedResources :+ resourceEntry
                                  )
                                  (m + (token -> newEntry))
                              }
                            } >> {
                              if (i == 0) head.complete(a)
                              else if (openTail) q.offer(Chunk((token, resourceToken, a)))
                              else F.unit
                            }.as(killSignal.get)
                          }
                        }
                      }
                      .map(fs2.Stream.eval(_))
                      .parJoinUnbounded
                  }
                }.compile.drain.start
                  .map(_.cancel)

                _ <- state.update(_ + (token -> State(close, Vector.empty)))
                _ <- start.complete(())
                hd <- head.get
              } yield (token, hd)

            override def release(tokens: Set[StreamToken]): F[Unit] =
              state
                .modify(m => (m -- tokens, tokens.toList.flatMap(m.get(_).toList)))
                .flatMap(removeStates)

            override def freeUnused(token: StreamToken, resource: ResourceToken): F[Unit] =
              state.modify { s =>
                s.get(token) match {
                  case None => (s, F.unit)
                  case Some(state) =>
                    val (release, keep) = state.allocatedResources.span { case (k, _) => k != resource }
                    val newState = state.copy(allocatedResources = keep)
                    (s + (token -> newState), release.traverse_ { case (_, fa) => fa })
                }
              }.flatten

            override def changes: Stream[F, NonEmptyList[(StreamToken, ResourceToken, Either[Throwable, A])]] =
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
