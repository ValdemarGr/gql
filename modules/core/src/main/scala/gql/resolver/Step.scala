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

sealed trait Step[+F[_], -I, +O]

object Step {
  object Alg {
    final case class Lift[I, O](f: I => O) extends AnyRef with Step[Nothing, I, O]

    final case class EmbedEffect[F[_], I]() extends AnyRef with Step[F, F[I], I]

    final case class EmbedStream[F[_], I]() extends AnyRef with Step[F, fs2.Stream[F, I], I]

    final case class EmbedError[I]() extends AnyRef with Step[Nothing, Ior[String, I], I]

    final case class Argument[I, A](arg: Arg[A]) extends Step[Nothing, I, A]

    final case class Compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]) extends Step[F, I, O]

    final case class Choice[F[_], A, B, C](fac: Step[F, A, C], fab: Step[F, B, C]) extends Step[F, Either[A, B], C]

    final case class GetMeta[I]() extends Step[Nothing, I, Meta]

    final case class First[F[_], A, B, C](step: Step[F, A, B]) extends Step[F, (A, C), (B, C)]

    final case class Batch[F[_], K, V](id: BatchKey[K, V]) extends Step[F, Set[K], Map[K, V]]
  }

  def lift[F[_], I, O](f: I => O): Step[F, I, O] =
    Alg.Lift(f)

  def embedEffect[F[_], I]: Step[F, F[I], I] =
    Alg.EmbedEffect()

  def embedError[F[_], I]: Step[F, Ior[String, I], I] =
    Alg.EmbedError()

  def embedStream[F[_], I, O]: Step[F, fs2.Stream[F, I], I] =
    Alg.EmbedStream()

  def argument[F[_], A](arg: Arg[A]): Step[F, Any, A] =
    Alg.Argument(arg)

  def compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]): Step[F, I, O] =
    Alg.Compose(left, right)

  def choice[F[_], A, B, C](fac: Step[F, A, C], fab: Step[F, B, C]): Step[F, Either[A, B], C] =
    Alg.Choice(fac, fab)

  def getMeta[F[_]]: Step[F, Any, Meta] =
    Alg.GetMeta()

  def first[F[_], A, B, C](step: Step[F, A, B]): Step[F, (A, C), (B, C)] =
    Alg.First(step)

  final case class BatchKey[K, V](id: Int) extends AnyVal

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Step[F, Set[K], Map[K, V]]] =
    State { s =>
      val id = s.nextId
      val k = BatchKey[K, V](id)
      (s.copy(nextId = id + 1, batchFunctions = s.batchFunctions + (k -> SchemaState.BatchFunction(f))), Alg.Batch(k))
    }

  import cats.arrow._
  implicit def arrowForStep[F[_]]: Arrow[Step[F, *, *]] = new Arrow[Step[F, *, *]] {
    override def compose[A, B, C](f: Step[F, B, C], g: Step[F, A, B]): Step[F, A, C] = Step.compose(g, f)

    override def first[A, B, C](fa: Step[F, A, B]): Step[F, (A, C), (B, C)] = Step.first(fa)

    override def lift[A, B](f: A => B): Step[F, A, B] = Step.lift(f)
  }
}
