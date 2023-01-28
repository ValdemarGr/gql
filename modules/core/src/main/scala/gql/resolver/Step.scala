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
package gql.resolver

import gql._
import cats.data._

sealed trait Step[F[_], -I, +O]

object Step {
  object Alg {
    final case class Lift[F[_], I, O](f: I => O) extends AnyRef with Step[F, I, O]

    final case class Effect[F[_], I, O](f: I => F[O]) extends AnyRef with Step[F, I, O]

    final case class Rethrow[F[_], I]() extends AnyRef with Step[F, Ior[String, I], I]

    final case class Argument[F[_], I, A](arg: Arg[A]) extends Step[F, I, A]

    final case class Compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]) extends Step[F, I, O]

    final case class Stream[F[_], I, O](f: I => fs2.Stream[F, O]) extends AnyRef with Step[F, I, O]

    final case class Skip[F[_], I, O](compute: Step[F, I, O]) extends Step[F, Either[I, O], O]

    final case class GetMeta[F[_], I]() extends Step[F, I, Meta]

    final case class First[F[_], A, B, C](step: Step[F, A, B]) extends Step[F, (A, C), (B, C)]

    final case class Batch[F[_], K, V](id: Int) extends Step[F, Set[K], Map[K, V]]
  }

  def lift[F[_], I, O](f: I => O): Step[F, I, O] =
    Alg.Lift(f)

  def effect[F[_], I, O](f: I => F[O]): Step[F, I, O] =
    Alg.Effect(f)

  def rethrow[F[_], I]: Step[F, Ior[String, I], I] =
    Alg.Rethrow()

  def argument[F[_], A](arg: Arg[A]): Step[F, Any, A] =
    Alg.Argument(arg)

  def compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]): Step[F, I, O] =
    Alg.Compose(left, right)

  def stream[F[_], I, O](f: I => fs2.Stream[F, O]): Step[F, I, O] =
    Alg.Stream(f)

  def skip[F[_], I, O](compute: Step[F, I, O]): Step[F, Either[I, O], O] =
    Alg.Skip(compute)

  def getMeta[F[_]]: Step[F, Any, Meta] =
    Alg.GetMeta()

  def first[F[_], A, B, C](step: Step[F, A, B]): Step[F, (A, C), (B, C)] =
    Alg.First(step)

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Step[F, Set[K], Map[K, V]]] =
    State { s =>
      val id = s.nextId
      (s.copy(nextId = id + 1, batchFunctions = s.batchFunctions + (id -> SchemaState.BatchFunction(f))), Alg.Batch(id))
    }

  import cats.arrow._
  implicit def arrowForStep[F[_]]: Arrow[Step[F, *, *]] = new Arrow[Step[F, *, *]] {
    override def compose[A, B, C](f: Step[F, B, C], g: Step[F, A, B]): Step[F, A, C] = Step.compose(g, f)

    override def first[A, B, C](fa: Step[F, A, B]): Step[F, (A, C), (B, C)] = Step.first(fa)

    override def lift[A, B](f: A => B): Step[F, A, B] = Step.lift(f)
  }
}
