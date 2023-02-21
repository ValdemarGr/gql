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

import cats.effect._
import cats.implicits._
import cats.effect.std._

trait BackpressureSignal[F[_], A, B] {
  // The outer resource leases the state
  // The inner effect can repeatedly pull the state during the lease
  def listen: Resource[F, BackpressureSignal.Signal[F, A]]

  // The outer F is the effect of publishing
  // The inner F is the effect of waiting for the element to be consumed
  def publish(b: B): F[F[Unit]]
}

object BackpressureSignal {
  trait Signal[F[_], A] {
    def awaitNonEmpty: F[Unit]

    def uncons: F[A]

    // Combination of awaitNonEmpty and uncons
    def awaitUncons: F[A]
  }

  final case class State[F[_], A](
      pullDone: Deferred[F, Unit],
      pushDone: Deferred[F, Unit],
      value: A
  )

  def apply[F[_], A, B](
      initial: A,
      tryPublish: (A, B) => Option[A]
  )(implicit F: Concurrent[F]) = {
    Semaphore[F](1).flatMap { sem =>
      (F.deferred[Unit], F.deferred[Unit])
        .flatMapN((pull, push) => F.ref(State(pull, push, initial)))
        .map { state =>
          def publish0(b: B): F[F[Unit]] =
            state.modify { current =>
              tryPublish(current.value, b) match {
                case None    => (current, current.pullDone.get >> publish0(b))
                case Some(a) => current.copy(value = a) -> current.pushDone.complete(()).void.as(current.pullDone.get)
              }
            }.flatten

          // def unconsAll0: F[A] =
          //   sem.permit.surround {
          //     (F.deferred[Unit], F.deferred[Unit]).flatMapN { (newPullDone, newPushDone) =>
          //       state.get.flatMap(_.pushDone.get) >>
          //         state.modify { current =>
          //           val fa = current.pullDone.complete(()).void
          //           State(newPullDone, newPushDone, initial) -> fa.as(current.value)
          //         }.flatten
          //     }
          //   }

          def listen0: Resource[F, Signal[F, A]] =
            sem.permit.as {
              new Signal[F, A] {
                def awaitNonEmpty: F[Unit] = state.get.flatMap(_.pushDone.get)

                def uncons: F[A] =
                  (F.deferred[Unit], F.deferred[Unit]).flatMapN { (newPullDone, newPushDone) =>
                    state.modify { current =>
                      val fa = current.pullDone.complete(()).void
                      State(newPullDone, newPushDone, initial) -> fa.as(current.value)
                    }.flatten
                  }

                def awaitUncons: F[A] = awaitNonEmpty >> uncons
              }
            }

          new BackpressureSignal[F, A, B] {
            def listen = listen0

            def publish(b: B): F[F[Unit]] = publish0(b)
          }
        }
    }
  }
}
