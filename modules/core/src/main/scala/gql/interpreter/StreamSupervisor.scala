/*
 * Copyright 2022 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql.interpreter

import cats.data._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}

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
    fs2.Stream.resource(Supervisor[F]).flatMap { sup =>
      fs2.Stream
        .bracket(F.ref(Map.empty[StreamToken, State[F]]))(_.get.flatMap(xs => removeStates(xs.values.toList)))
        .flatMap { (state: Ref[F, StateMap]) =>
          fs2.Stream.eval(Queue.bounded[F, Chunk[(StreamToken, ResourceToken, Either[Throwable, A])]](1024)).map { q =>
            new StreamSupervisor[F, A] {
              override def acquireAwait(stream: Stream[F, A]): F[(Unique.Token, Either[Throwable, A])] =
                F.uncancelable { _ =>
                  for {
                    token <- F.unique

                    head <- F.deferred[Either[Throwable, A]]

                    start <- F.deferred[Unit]

                    // close <- {
                    //   def go(stream: fs2.Stream[F, (Either[Throwable, A], Long)]): fs2.Pull[F, Nothing, Unit] =
                    //     stream.pull.uncons1
                    //       .flatMap {
                    //         case None => fs2.Pull.done
                    //         case Some(((hd, i), tl)) =>
                    //           fs2.Pull.eval(F.deferred[Either[Throwable, Unit]]).flatMap { die =>
                    //             fs2.Pull
                    //               .extendScopeTo(fs2.Stream.never[F].interruptWhen(die))
                    //               .evalMap(back => sup.supervise(back.compile.drain))
                    //               .flatMap { resourceFiber =>
                    //                 fs2.Pull.eval(F.unique).flatMap { resourceToken =>
                    //                   val updateStateF = state.modify { m =>
                    //                     m.get(token) match {
                    //                       // We are closed for business
                    //                       case None => (m, resourceFiber.cancel)
                    //                       case Some(state) =>
                    //                         val resourceEntry = (resourceToken, resourceFiber.cancel)
                    //                         val newEntry = state.copy(
                    //                           allocatedResources = state.allocatedResources :+ resourceEntry
                    //                         )
                    //                         val publish =
                    //                           if (i == 0) head.complete(hd).void
                    //                           else if (openTail) q.offer(Chunk((token, resourceToken, hd)))
                    //                           else F.unit
                    //                         (m + (token -> newEntry), publish)
                    //                     }
                    //                   }.flatten

                    //                   fs2.Pull.eval(updateStateF) >> go(tl)
                    //                 }
                    //               }
                    //           }
                    //       }

                    //   sup.supervise {
                    //     start.get >> go(stream.attempt.zipWithIndex).stream.as(()).compile.drain
                    //   }
                    // }.map(_.cancel)

                    // TODO find a more elegant way of holding fs2 Leases
                    // start <- F.deferred[Unit]

                    // Hold fs2 leases by:
                    // 1. allocate a token for the resource and a killSignal / releaseSignal
                    // 2. do data publishing
                    // 3. await one killSignal for every stream element/resource scope
                    close <- sup
                      .supervise {
                        {
                          fs2.Stream.eval(start.get) >> {
                            stream.attempt.zipWithIndex
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
                                      if (i == 0) head.complete(a).void
                                      else if (openTail) q.offer(Chunk((token, resourceToken, a)))
                                      else F.unit
                                    } as killSignal.get
                                  }
                                }
                              }
                              .map(fs2.Stream.eval(_))
                              .parJoinUnbounded
                          }
                        }.compile.drain
                      }
                      .map(_.cancel)

                    _ <- state.update(_ + (token -> State(close, Vector.empty)))
                    _ <- start.complete(())
                  } yield head.get tupleLeft token
                }.flatten

              override def release(tokens: Set[StreamToken]): F[Unit] =
                F.uncancelable { _ =>
                  state
                    .modify(m => (m -- tokens, tokens.toList.flatMap(m.get(_).toList)))
                    .flatMap(removeStates)
                }

              override def freeUnused(token: StreamToken, resource: ResourceToken): F[Unit] =
                F.uncancelable { _ =>
                  state.modify { s =>
                    s.get(token) match {
                      case None => (s, F.unit)
                      case Some(state) =>
                        val (release, keep) = state.allocatedResources.span { case (k, _) => k != resource }
                        val newState = state.copy(allocatedResources = keep)
                        (s + (token -> newState), release.traverse_ { case (_, fa) => fa })
                    }
                  }.flatten
                }

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
}
