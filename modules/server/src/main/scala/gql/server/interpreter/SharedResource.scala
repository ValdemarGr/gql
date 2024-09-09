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
package gql.server.interpreter

import cats.effect._
import cats.implicits._
import cats.effect.kernel.Resource.ExitCase

object SharedResource {
  sealed trait State[F[_]]
  object State {
    final case class Closed[F[_]]() extends State[F]
    final case class Open[F[_]](leases: Int) extends State[F]
  }

  def make[F[_]](res: Resource[F, Unit])(implicit F: Async[F]): Resource[F, Resource[F, Option[Int]]] =
    Resource.applyFull { poll =>
      ((poll(res.allocated), F.ref[State[F]](State.Open(1))).tupled).flatMap { case ((_, release0), ref) =>
        F.deferred[Unit].map { d =>
          val release = release0 *> d.complete(()).void
          val open = ref.modify {
            case State.Closed() => (State.Closed[F](), None)
            case State.Open(n) =>
              val n2 = n + 1
              (State.Open(n2), Some(n2))
          }

          val close = ref.modify {
            case State.Closed() => (State.Closed[F](), F.unit)
            case State.Open(n) =>
              val n2 = n - 1
              if (n2 === 0) (State.Closed[F](), release)
              else (State.Open(n2), F.unit)
          }.flatten

          val api = Resource.make(open) {
            case None    => F.unit
            case Some(_) => close
          }

          (api, ((_: ExitCase) => close))
        }
      }
    }
}
