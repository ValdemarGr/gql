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
import java.util.concurrent.atomic.AtomicInteger

trait Arc[F[_], +A] { self =>
  implicit def F: Async[F]

  def value: A

  def shareOpt: Resource[F, Option[A]]

  def share: Resource[F, A] =
    shareOpt.flatMap {
      case Some(a) => Resource.pure(a)
      case None    => Resource.eval(F.raiseError(new Exception("Arc already closed")))
    }

  def map[B](f: A => B): Arc[F, B] =
    new Arc[F, B] {
      implicit def F: Async[F] = self.F

      def value: B = f(self.value)

      def shareOpt: Resource[F, Option[B]] =
        self.shareOpt.map(_.map(f))
    }
}

object Arc {
  def pure[F[_], A](a: A)(implicit F0: Async[F]): Arc[F, A] =
    new Arc[F, A] {
      implicit def F: Async[F] = F0

      def value: A = a

      def shareOpt: Resource[F, Option[A]] = Resource.pure(Some(a))
    }

  val ar = new AtomicInteger(0)
  def of[F[_], A](fa: Resource[F, A])(implicit F: Async[F]): Resource[F, Arc[F, A]] =
    Resource.eval(Ref[F].of(Option.empty[(Int, A, F[Unit])])).flatMap { state =>
      val release = state.modify {
        case None =>
          (None, F.raiseError[Unit](new Exception("Arc already closed during release?")))
        case Some((users, a, free)) =>
          // println(s"users $users")
          if (users === 1) {
            println("Arc released #" + ar.decrementAndGet())
            (None, free)
          } else (Some((users - 1, a, free)), F.unit)
      }.flatten

      val res = Resource.make {
        state.modify {
          case None => (None, None)
          case Some((users, a, free)) =>
            val users1 = users + 1
            (Some((users1, a, free)), Some(a))
        }
      } {
        case None    => F.unit
        case Some(_) => release
      }

      Resource
        .makeFull[F, A] { poll =>
          println("Arc allocated #" + ar.incrementAndGet())
          poll(fa.allocated).flatMap { case (a, free) =>
            state.set(Some((1, a, free))).as(a)
          }
        }(_ => release)
        .map { a =>
          val F0 = F
          new Arc[F, A] {
            implicit def F: Async[F] = F0

            def value: A = a

            def shareOpt: Resource[F, Option[A]] = res
          }
        }
    }
}
