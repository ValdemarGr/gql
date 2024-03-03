/*
 * Copyright 2024 Valdemar Grange
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
package gql.relational

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.std._

trait LazyResource[F[_], A] { self =>
  def get: Resource[F, A]

  def forceClose: F[Unit]

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G)(implicit F: MonadCancelThrow[F]) = new LazyResource[G, A] {
    def get = self.get.mapK(fk)

    def forceClose = fk(self.forceClose)
  }
}

object LazyResource {
  def fromResource[F[_], A](res: Resource[F, A])(implicit F: Concurrent[F]): Resource[F, LazyResource[F, A]] = {
    val leases = Long.MaxValue

    case class State(
        value: A,
        users: Semaphore[F],
        clean: F[Unit]
    )

    Supervisor[F](await = true).flatMap { sup =>
      Resource.eval(Mutex[F]).flatMap { mtx =>
        def excl(s: State) = Resource.make(s.users.acquireN(leases))(_ => s.users.releaseN(leases))

        def forceClose0(state: Ref[F, Option[State]]) = {
          type Ret = F[Either[Throwable, Unit]]
          val noop: (Option[State], Ret) = (None, F.pure(Right(())))
          mtx.lock.surround {
            F.uncancelable { poll =>
              state
                .modify[Ret] {
                  // Nothing in the state, cool it is a noop!
                  case None => noop
                  // We found something, let's await that there are no more users
                  case Some(s) =>
                    // Since we remove the reference immidiately we must ensure that the cleanup is done
                    (None, sup.supervise(excl(s).surround(s.clean)).flatMap(fib => poll(fib.joinWithNever)).attempt)
                }
                .flatten
                .rethrow
            }
          }
        }

        val stateR =
          Resource.make(F.ref[Option[State]](None))(_.get.flatMap(_.traverse_(_.clean)))

        stateR.map { state =>
          new LazyResource[F, A] {
            override def forceClose: F[Unit] = forceClose0(state)

            override def get: Resource[F, A] =
              mtx.lock >>
                Resource
                  .eval {
                    state
                      .modify[F[State]] {
                        case Some(s) => (Some(s), F.pure(s))
                        case None =>
                          val newStateF = F.uncancelable { poll =>
                            poll(res.allocated).flatMap { case (value, clean) =>
                              Semaphore[F](leases).flatMap { users =>
                                val s = State(value, users, clean)
                                state.set(Some(s)) as s
                              }
                            }
                          }
                          (None, newStateF)
                      }
                      .flatten
                  }
                  .flatTap(_.users.permit)
                  .map(_.value)

          }
        }
      }
    }
  }

  implicit def functorForLazyResource[F[_]]: Functor[LazyResource[F, *]] = new Functor[LazyResource[F, *]] {
    override def map[A, B](fa: LazyResource[F, A])(f: A => B): LazyResource[F, B] = new LazyResource[F, B] {
      override def get: Resource[F, B] = fa.get.map(f)
      override def forceClose: F[Unit] = fa.forceClose
    }
  }
}
