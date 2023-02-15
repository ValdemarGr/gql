/*
 * Copyright 2023 Valdemar Grange
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

import cats.effect.implicits._
import cats.data._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}

final case class StreamToken(value: BigInt) extends AnyVal
final case class ResourceToken(value: BigInt) extends AnyVal

trait StreamSupervisor[F[_]] {
  def acquireAwait[A](stream: Stream[F, A]): F[(StreamToken, Either[Throwable, A])]

  def release(token: Set[StreamToken]): F[Unit]

  def freeUnused(token: StreamToken, resource: ResourceToken): F[Unit]

  def changes: Stream[F, NonEmptyList[(StreamToken, ResourceToken, Either[Throwable, ?])]]
}

object StreamSupervisor {
  final case class State[F[_]](
      unsubscribe: F[Unit],
      allocatedResources: Vector[(ResourceToken, F[Unit])]
  )

  final case class SupervisorState[F[_]](
      states: Map[StreamToken, State[F]],
      nextResourceOrder: BigInt
  )

  def apply[F[_]](openTail: Boolean)(implicit F: Async[F]): Stream[F, StreamSupervisor[F]] = {
    def f[A] = {
      Queue.bounded[F, Chunk[(Scope[F], A)]](1024).map { q =>
        def acquireAwait(stream: Stream[F, A], scope: Scope[F]): F[(Scope[F], A)] = {
          F.deferred[(Scope[F], A)].flatMap { head =>
            def publish(idx: Long, a: A, scope: Scope[F]): F[Unit] =
              if (idx === 0L) head.complete((scope, a)).void
              else if (openTail) q.offer(Chunk.singleton((scope, a)))
              else F.unit

            scope.openChild { parentScope =>
              stream.zipWithIndex
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
      }
    }

    def removeStates(xs: List[State[F]]) = {
      xs.traverse_(_.unsubscribe) >>
        xs.traverse_(_.allocatedResources.toList.traverse_ { case (_, fa) => fa })
    }

    val emptyState = SupervisorState(Map.empty[StreamToken, State[F]], 1)

    def stateF =
      Resource.make(F.ref(emptyState))(_.get.flatMap { (s: SupervisorState[F]) =>
        val ided = s.states.toList.flatMap { case (st, s) =>
          (st.value, s.unsubscribe) :: s.allocatedResources.toList.map { case (tok, cleanup) => (tok.value, cleanup) }
        }

        ided.sortBy { case (k, _) => k }(Ordering[BigInt].reverse).traverse_ { case (_, fa) => fa }
      })

    fs2.Stream.resource(Supervisor[F]).flatMap { sup =>
      fs2.Stream
        .resource(stateF)
        .flatMap { (state: Ref[F, SupervisorState[F]]) =>
          val nextId_ = state.modify(s => (s.copy(nextResourceOrder = s.nextResourceOrder + 1), s.nextResourceOrder))
          val nextResourceId = nextId_.map(ResourceToken(_))
          val nextStreamId = nextId_.map(StreamToken(_))

          fs2.Stream.eval(Queue.bounded[F, Chunk[(StreamToken, ResourceToken, Either[Throwable, ?])]](1024)).map { q =>
            new StreamSupervisor[F] {
              override def acquireAwait[A](stream: Stream[F, A]): F[(StreamToken, Either[Throwable, A])] =
                F.uncancelable { _ =>
                  for {
                    token <- nextStreamId

                    head <- F.deferred[Either[Throwable, A]]

                    start <- F.deferred[Unit]

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
                                  nextResourceId.flatMap { resourceToken =>
                                    state.update { s =>
                                      s.states.get(token) match {
                                        // We are closed for business
                                        case None => s
                                        case Some(state) =>
                                          val resourceEntry = (resourceToken, killSignal.complete(()).void)
                                          val newEntry = state.copy(
                                            allocatedResources = state.allocatedResources :+ resourceEntry
                                          )
                                          s.copy(states = (s.states + (token -> newEntry)))
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

                    _ <- state.update(s => s.copy(states = s.states + (token -> State(close, Vector.empty))))
                    _ <- start.complete(())
                  } yield head.get tupleLeft token
                }.flatten

              override def release(tokens: Set[StreamToken]): F[Unit] = ??? /*
                F.uncancelable { _ =>
                  state
                    .modify(m => (m -- tokens, tokens.toList.flatMap(m.get(_).toList)))
                    .flatMap(removeStates)
                }*/

              override def freeUnused(token: StreamToken, resource: ResourceToken): F[Unit] = ??? /*
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
                }*/

              override def changes: Stream[F, NonEmptyList[(StreamToken, ResourceToken, Either[Throwable, ?])]] =
                Stream
                  .fromQueueUnterminatedChunk(q)
                  .chunkN(128, allowFewer = true)
                  .map(_.toNel)
                  .unNone
                  // Only respect newest element for a given token pair
                  .map(_.toList.reverse.distinctBy { case (tok, _, _) => tok }.toNel)
                  .unNone
            }
          }
        }
    }
  }
}
