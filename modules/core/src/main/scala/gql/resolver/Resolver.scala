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

import cats.data._
import cats._
import gql._

final class Resolver[F[_], -I, +O](private[gql] val underlying: Step[F, I, O]) {
  def andThen[O2](that: Resolver[F, O, O2]): Resolver[F, I, O2] =
    new Resolver(Step.compose(underlying, that.underlying))

  def compose[I1 <: I, I2](that: Resolver[F, I2, I1]): Resolver[F, I2, O] =
    that andThen this

  def map[O2](f: O => O2): Resolver[F, I, O2] =
    this andThen Resolver.lift(f)

  def contramap[I2](f: I2 => I): Resolver[F, I2, O] =
    Resolver.lift(f) andThen this

  def evalMap[O2](f: O => F[O2]): Resolver[F, I, O2] =
    this andThen Resolver.eval(f)

  def evalContramap[I1 <: I, I2](f: I2 => F[I1]): Resolver[F, I2, O] =
    Resolver.eval(f) andThen this

  def fallibleMap[O2](f: O => Ior[String, O2]): Resolver[F, I, O2] =
    this.map(f) andThen Resolver.rethrow

  def fallibleContraMap[I2](f: I2 => Ior[String, I]): Resolver[F, I2, O] =
    Resolver.rethrow.contramap[I2](f) andThen this

  def first[C]: Resolver[F, (I, C), (O, C)] =
    new Resolver(Step.first(underlying))

  def tupleIn[I1 <: I]: Resolver[F, I1, (O, I1)] =
    first[I1].contramap[I1](i => (i, i))

  def arg[A](arg: Arg[A]): Resolver[F, I, (A, O)] =
    this andThen Resolver.argument[F, O, A](arg).tupleIn

  def contraArg[A, I2](arg: Arg[A])(implicit ev: (A, I2) <:< I): Resolver[F, I2, O] =
    Resolver.id[F, I2].arg(arg) andThen this.contramap(ev.apply)

  def meta: Resolver[F, I, (Meta, O)] =
    this andThen Resolver.meta[F, O].tupleIn

  def stream[O2](f: O => fs2.Stream[F, O2]): Resolver[F, I, O2] =
    this andThen Resolver.stream[F, O, O2](f)

  def skippable[O1 >: O]: Resolver[F, Either[I, O1], O1] =
    new Resolver(Step.skip(this.underlying))
}

object Resolver extends ResolverInstances {
  def lift[F[_], I, O](f: I => O): Resolver[F, I, O] =
    new Resolver(Step.lift(f))

  def id[F[_], I]: Resolver[F, I, I] =
    lift(identity)

  def eval[F[_], I, O](f: I => F[O]): Resolver[F, I, O] =
    new Resolver(Step.effect(f))

  def rethrow[F[_], I]: Resolver[F, Ior[String, I], I] =
    new Resolver(Step.rethrow[F, I])

  def argument[F[_], I <: Any, A](arg: Arg[A]): Resolver[F, I, A] =
    new Resolver(Step.argument(arg))

  def meta[F[_], I <: Any]: Resolver[F, I, Meta] =
    new Resolver(Step.getMeta)

  def stream[F[_], I, O](f: I => fs2.Stream[F, O]): Resolver[F, I, O] =
    new Resolver(Step.stream(f))

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Resolver[F, Set[K], Map[K, V]]] =
    Step.batch[F, K, V](f).map(new Resolver(_))

  implicit class RethrowOps[F[_], I, O](private val self: Resolver[F, I, Ior[String, O]]) extends AnyVal {
    def rethrow: Resolver[F, I, O] =
      self andThen Resolver.rethrow
  }

  implicit class ContinueInvariantOps[F[_], I, O](private val self: Resolver[F, I, O]) extends AnyVal {
    def continue[O2](f: Resolver[F, O, O] => Resolver[F, O, O2]): Resolver[F, I, O2] =
      self andThen f(Resolver.id[F, O])
  }

  implicit class SkipThisInvariantOps[F[_], I, O](private val self: Resolver[F, I, O]) extends AnyVal {
    def skipThis[I2](verify: Resolver[F, I2, Either[I, O]]): Resolver[F, I2, O] =
      verify andThen self.skippable

    def skipThisWith[I2](f: Resolver[F, I2, I2] => Resolver[F, I2, Either[I, O]]): Resolver[F, I2, O] =
      skipThis[I2](f(Resolver.id[F, I2]))
  }

  implicit class SkipThatInvariantOps[F[_], I, I2, O](private val self: Resolver[F, I, Either[I2, O]]) extends AnyVal {
    def skipThat(compute: Resolver[F, I2, O]): Resolver[F, I, O] =
      compute skipThis self

    def skipThatWith(f: Resolver[F, I2, I2] => Resolver[F, I2, O]): Resolver[F, I, O] =
      skipThat(f(Resolver.id[F, I2]))
  }
}

trait ResolverInstances {
  import cats.arrow._
  implicit def arrowForResolver[F[_]]: Arrow[Resolver[F, *, *]] = new Arrow[Resolver[F, *, *]] {
    override def compose[A, B, C](f: Resolver[F, B, C], g: Resolver[F, A, B]): Resolver[F, A, C] =
      f.compose(g)

    override def first[A, B, C](fa: Resolver[F, A, B]): Resolver[F, (A, C), (B, C)] =
      fa.first[C]

    override def lift[A, B](f: A => B): Resolver[F, A, B] = Resolver.lift(f)
  }

  implicit def applicativeForResolver[F[_], I]: Applicative[Resolver[F, I, *]] = new Applicative[Resolver[F, I, *]] {
    override def ap[A, B](ff: Resolver[F, I, A => B])(fa: Resolver[F, I, A]): Resolver[F, I, B] =
      ff.tupleIn[I] andThen
        fa
          .contramap[(A => B, I)] { case (_, i) => i }
          .tupleIn[(A => B, I)]
          .map { case (a, (f, _)) => f(a) }

    override def pure[A](x: A): Resolver[F, I, A] =
      Resolver.lift(_ => x)
  }
}
