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
package gql.server.interpreter

import cats.implicits.*
import cats.effect.*
import cats.*

import ResourceSupervisor.*
import java.util.concurrent.atomic.AtomicInteger
final case class ResourceSupervisor[F[_]](
    state: Ref[F, State[F]]
)(implicit F: Async[F]) {

  def lease[A](resource: Resource[F, A]): F[(Lease, A)] =
    F.unique.map(Lease(_)).flatMap { token =>
      println(s"leasing $token")
      F.uncancelable { poll =>
        poll(resource.allocated).flatMap { case (a, free) =>
          state.modify {
            case State.Closed() =>
              (
                State.Closed(),
                free >> F.raiseError[(Lease, A)](
                  new IllegalStateException("ResourceSupervisor is closed")
                )
              )
            case State.Open(m) =>
              State.Open(m + (token -> free)) -> F.pure((token, a))
          }.flatten
        }
      }
    }

  def leaseAndForget[A](resource: Resource[F, A]): F[A] =
    lease(resource).map { case (_, a) => a }

  // if async, start the releaser as a fiber
  // we re-add the fiber to the map such that if the ResourceSupervisor is released or closed
  // it will await completion of the releaser
  def release_(token: Lease, async: Boolean): F[Unit] =
    F.uncancelable { _ =>
      println(s"releasing $lease")
      state
        .modify[F[Unit]] {
          case State.Closed() =>
            println("release on closed!!")
            State.Closed() -> F.raiseError[Unit](new IllegalStateException("ResourceSupervisor is closed"))
          case State.Open(m) =>
            m.get(token) match {
              case None => (State.Open(m), F.unit)
              case Some(free) =>
                if (!async) (State.Open(m - token), free)
                else {
                  (
                    State.Open(m - token),
                    F.start(free >> release_(token, async = false)).flatMap { fib =>
                      state.update {
                        case State.Closed() => State.Closed()
                        case State.Open(m)  => State.Open(m + (token -> fib.joinWithUnit))
                      }
                    }
                  )
                }
            }
        }
        .flatten
    }

  def release(token: Lease): F[Unit] =
    release_(token, async = false)

  def releaseAsync(token: Lease): F[Unit] =
    release_(token, async = true)

  def reset: F[Unit] = F.uncancelable { _ =>
    state.modify {
      case State.Closed() => State.Closed() -> F.unit
      case State.Open(m)  => State.Open(Map.empty) -> m.values.toList.sequence_
    }.flatten
  }
}

object ResourceSupervisor {
  final case class Lease(token: Unique.Token)
  object Lease {
    implicit val eq: Eq[Lease] = Eq.by(_.token)
  }

  sealed trait State[F[_]]
  object State {
    case class Closed[F[_]]() extends State[F]
    case class Open[F[_]](m: Map[Lease, F[Unit]]) extends State[F]
  }

  val ar = new AtomicInteger(0)
  def make[F[_]](implicit F: Async[F]): Resource[F, ResourceSupervisor[F]] =
    Resource.make {
      F.delay {
        val id = ar.incrementAndGet()
        println("Created RS, count is " + id)
      }
    } { _ =>
      F.delay {
        val id = ar.decrementAndGet()
        println("Dropped RS, count is " + id)
      }
    } *>
      Resource
        .make(Ref.of[F, State[F]](State.Open(Map.empty)))(_.modify { s =>
          println(s"cleaning up $s")
          val fa = s match {
            case State.Closed() => F.raiseError[Unit](new IllegalStateException("ResourceSupervisor is closed"))
            case State.Open(m)  => m.values.toList.sequence_
          }
          (State.Closed(), fa)
        }.flatten)
        .map(ResourceSupervisor(_))
}
