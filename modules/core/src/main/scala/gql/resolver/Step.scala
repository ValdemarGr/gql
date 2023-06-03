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

import cats._
import cats.implicits._
import gql._
import cats.data._

sealed trait Step[+F[_], -I, +O]

object Step {
  object Alg {
    final case class Lift[I, O](f: I => O) extends AnyRef with Step[Nothing, I, O]

    final case class Effect[F[_], I, O](f: I => F[O]) extends AnyRef with Step[F, I, O]

    final case class Stream[F[_], I, O](
        f: I => fs2.Stream[F, O],
        signal: Boolean
    ) extends AnyRef
        with Step[F, I, O]

    final case class EmbedError[I]() extends AnyRef with Step[Nothing, Ior[String, I], I]

    final case class Argument[I, A](arg: Arg[A]) extends Step[Nothing, I, A]

    final case class Compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]) extends Step[F, I, O]

    final case class Choose[F[_], A, B, C, D](
        fac: Step[F, A, C],
        fab: Step[F, B, D]
    ) extends Step[F, Either[A, B], Either[C, D]]

    // final case class Parallel[F[_], A, B, C]()

    final case class GetMeta[I]() extends Step[Nothing, I, FieldMeta]

    final case class First[F[_], A, B, C](step: Step[F, A, B]) extends Step[F, (A, C), (B, C)]

    final case class Batch[F[_], K, V](id: BatchKey[K, V]) extends Step[F, Set[K], Map[K, V]]
  }

  def lift[F[_], I, O](f: I => O): Step[F, I, O] =
    Alg.Lift(f)

  def embedEffect[F[_], I]: Step[F, F[I], I] = ???
  // Alg.EmbedEffect()

  def embedError[F[_], I]: Step[F, Ior[String, I], I] =
    Alg.EmbedError()

  def embedStreamFull[F[_], I, O](signal: Boolean): Step[F, fs2.Stream[F, I], I] = ???
  // Alg.EmbedStream(signal)

  def embedStream[F[_], I, O]: Step[F, fs2.Stream[F, I], I] =
    embedStreamFull(signal = true)

  def argument[F[_], A](arg: Arg[A]): Step[F, Any, A] =
    Alg.Argument(arg)

  def compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]): Step[F, I, O] =
    Alg.Compose(left, right)

  def choose[F[_], A, B, C, D](fac: Step[F, A, C], fab: Step[F, B, D]): Step[F, Either[A, B], Either[C, D]] =
    Alg.Choose(fac, fab)

  def getMeta[F[_]]: Step[F, Any, FieldMeta] =
    Alg.GetMeta()

  def first[F[_], A, B, C](step: Step[F, A, B]): Step[F, (A, C), (B, C)] =
    Alg.First(step)

  // Attempt attempt
  // def attempt[F[_], A, B](step: Step[F, A, B])(implicit F: ApplicativeThrow[F]): Step[F, A, Either[Throwable, B]] = {
  //   def gen[F[_], A, B](step: Step[F, A, B]): Step[F, A, B] = step

  //   step match {
  //     case Alg.Lift(f)              => Alg.Lift(f.andThen(_.asRight[Throwable]))
  //     case alg: Alg.Effect[F, A, B] => Alg.Effect[F, A, Either[Throwable, B]](alg.f.andThen(_.attempt))
  //     case alg: Alg.Stream[F, A, B] => Alg.Stream[F, A, Either[Throwable, B]](alg.f.andThen(_.attempt), alg.signal)
  //     case _: Alg.EmbedError[a]     => Alg.Compose(Alg.EmbedError[a](), Alg.Lift((b: B) => b.asRight[Throwable]))
  //     case alg: Alg.Argument[A, B]  => Alg.Argument(alg.arg.map(_.asRight[Throwable]))
  //     case alg: Alg.Compose[F, a, c, b] =>
  //       attempt(alg.left).andThen(
  //         gen(
  //           Alg.Choose[F, Throwable, c, Throwable, Either[Throwable, b]](
  //             Alg.Lift((t: Throwable) => t),
  //             attempt(alg.right)
  //           )
  //         ).map(_.flatten)
  //       )
  //     case alg: Alg.Choose[F, a, b, c, d] =>
  //       gen {
  //         Alg.Choose[F, a, b, Either[Throwable, c], Either[Throwable, d]](
  //           attempt(alg.fac),
  //           attempt(alg.fab)
  //         )
  //       }.map {
  //         case Left(Left(t))   => Left(t)
  //         case Right(Left(t))  => Left(t)
  //         case Right(Right(d)) => Right(Right(d))
  //         case Left(Right(c))  => Right(Left(c))
  //       }
  //     case _: Alg.GetMeta[A] => (Alg.GetMeta[A](): Step[Nothing, A, FieldMeta]).map(_.asRight[Throwable])
  //     case alg: Alg.First[F, a, b, c] =>
  //       gen(Alg.First[F, a, Either[Throwable, b], c](attempt(alg.step))).map{ case (e, c) => e.tupleRight(c) }
  //     case alg: Alg.Batch[F, k, v] =>

  //   }
  // }

  final case class BatchKey[K, V](id: Int) extends AnyVal

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Step[F, Set[K], Map[K, V]]] =
    State { s =>
      val id = s.nextId
      val k = BatchKey[K, V](id)
      (s.copy(nextId = id + 1, batchFunctions = s.batchFunctions + (k -> SchemaState.BatchFunction(f))), Alg.Batch(k))
    }

  import cats.arrow._
  implicit def arrowChoiceForStep[F[_]]: ArrowChoice[Step[F, *, *]] = new ArrowChoice[Step[F, *, *]] {
    override def choose[A, B, C, D](f: Step[F, A, C])(g: Step[F, B, D]): Step[F, Either[A, B], Either[C, D]] =
      Step.choose(f, g)

    override def compose[A, B, C](f: Step[F, B, C], g: Step[F, A, B]): Step[F, A, C] = Step.compose(g, f)

    override def first[A, B, C](fa: Step[F, A, B]): Step[F, (A, C), (B, C)] = Step.first(fa)

    override def lift[A, B](f: A => B): Step[F, A, B] = Step.lift(f)
  }
}
