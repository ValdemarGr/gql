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
import cats.implicits._
import gql._

/** Resolver is one of the core abstractions of gql. The resolver class contains a collection of methods to aid comosition.
  *
  * A Resolver forms an [[cats.arrow.Arrow]]; it can lift a function I => O. Resolver also forms [[cats.arrow.Choice]] which allows
  * conditional branching.
  *
  * Resolver also forms an [[cats.Applicative]] instance that sequences the operations.
  *
  * Some methods are only available resolvers that have a certain shape. Consider taking a look at the companion object for more
  * information.
  */
final class Resolver[+F[_], -I, +O](private[gql] val underlying: Step[F, I, O]) {
  def andThen[F2[x] >: F[x], O2](that: Resolver[F2, O, O2]): Resolver[F2, I, O2] =
    new Resolver(Step.compose(underlying, that.underlying))

  def compose[F2[x] >: F[x], I1 <: I, I2](that: Resolver[F2, I2, I1]): Resolver[F2, I2, O] =
    that andThen this

  def map[O2](f: O => O2): Resolver[F, I, O2] =
    this andThen Resolver.lift(f)

  def evalMap[F2[x] >: F[x], O2](f: O => F2[O2]): Resolver[F2, I, O2] =
    this andThen Resolver.effect(f)

  def emap[O2](f: O => Ior[String, O2]): Resolver[F, I, O2] =
    this.map(f) andThen (new Resolver(Step.embedError))

  def first[C]: Resolver[F, (I, C), (O, C)] =
    new Resolver(Step.first(underlying))

  def arg[A](arg: Arg[A]): Resolver[F, I, (A, O)] =
    this andThen Resolver.argument[F, O, A](arg).tupleIn

  def contraArg[A, I2](arg: Arg[A])(implicit ev: (A, I2) <:< I): Resolver[F, I2, O] =
    Resolver.id[F, I2].arg(arg) andThen this.contramap(ev.apply)

  def meta[F2[x] >: F[x]]: Resolver[F2, I, (FieldMeta[F2], O)] =
    this andThen Resolver.meta[F2, O].tupleIn

  def streamMap[F2[x] >: F[x], O2](f: O => fs2.Stream[F2, O2]): Resolver[F2, I, O2] =
    this.map(f).embedStream

  def sequentialStreamMap[F2[x] >: F[x], O2](f: O => fs2.Stream[F2, O2]): Resolver[F2, I, O2] =
    this.map(f).embedSequentialStream

  def step: Step[F, I, O] = underlying

  def covaryAll[F2[x] >: F[x], O2 >: O]: Resolver[F2, I, O2] = this

  def optimize: Resolver[F, I, O] = new Resolver(Step.optimize(underlying))
}

object Resolver extends ResolverInstances {
  def liftFull[F[_], I, O](f: I => O): Resolver[F, I, O] =
    new Resolver(Step.lift(f))

  final class PartiallyAppliedLift[F[_], I](private val dummy: Boolean = true) extends AnyVal {
    def apply[O](f: I => O): Resolver[F, I, O] = liftFull(f)
  }

  def lift[F[_], I]: PartiallyAppliedLift[F, I] = new PartiallyAppliedLift[F, I]

  def id[F[_], I]: Resolver[F, I, I] =
    new Resolver(Step.identity)

  def effectFull[F[_], I, O](f: I => F[O]): Resolver[F, I, O] =
    liftFull(f).andThen(new Resolver(Step.embedEffect))

  final class PartiallAppliedEffect[F[_], I](private val dummy: Boolean = true) extends AnyVal {
    def apply[O](f: I => F[O]): Resolver[F, I, O] = effectFull(f)
  }

  def effect[F[_], I]: PartiallAppliedEffect[F, I] = new PartiallAppliedEffect[F, I]

  def argument[F[_], I <: Any, A](arg: Arg[A]): Resolver[F, I, A] =
    new Resolver(Step.argument(arg))

  def meta[F[_], I <: Any]: Resolver[F, I, FieldMeta[F]] =
    new Resolver(Step.getMeta)

  def streamFull[F[_], I, O](f: I => fs2.Stream[F, O]): Resolver[F, I, O] =
    liftFull(f).andThen(new Resolver(Step.embedStream))

  final class PartiallyAppliedStream[F[_], I](private val dummy: Boolean = true) extends AnyVal {
    def apply[O](f: I => fs2.Stream[F, O]): Resolver[F, I, O] = streamFull(f)
  }

  def stream[F[_], I]: PartiallyAppliedStream[F, I] = new PartiallyAppliedStream[F, I]

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Resolver[F, Set[K], Map[K, V]]] =
    Step.batch[F, K, V](f).map(new Resolver(_))

  def inlineBatch[F[_], K, V](f: Set[K] => F[Map[K, V]]): Resolver[F, Set[K], Map[K, V]] =
    new Resolver(Step.inlineBatch(f))

  implicit class ResolverInvariantOps[F[_], I, O](private val self: Resolver[F, I, O]) extends AnyVal {
    def choose[I2, O2](that: Resolver[F, I2, O2]): Resolver[F, Either[I, I2], Either[O, O2]] =
      new Resolver(Step.choose(self.underlying, that.underlying))

    def choice[I2](that: Resolver[F, I2, O]): Resolver[F, Either[I, I2], O] =
      choose[I2, O](that).map(_.merge)

    def skippable: Resolver[F, Either[I, O], O] =
      this.choice(Resolver.id[F, O])

    def through[O2](f: Resolver[F, O, O] => Resolver[F, O, O2]): Resolver[F, I, O2] =
      self andThen f(Resolver.id[F, O])

    def contramap[I2](f: I2 => I): Resolver[F, I2, O] =
      Resolver.lift(f) andThen self

    def tupleIn: Resolver[F, I, (O, I)] =
      self.first[I].contramap[I](i => (i, i))

    def batch[K, V](implicit ev: Set[K] =:= I, ev2: O =:= Map[K, V]): ResolverBatchOps[F, K, V] =
      new ResolverBatchOps[F, K, V](self.contramap(ev.apply(_)).map(ev2.apply(_)))

    def rethrow[O2](implicit ev: O <:< Ior[String, O2]): Resolver[F, I, O2] =
      self.map(ev.apply(_)) andThen (new Resolver(Step.embedError))
  }

  implicit class ResolverEitherOps[F[_], I, L, R](private val self: Resolver[F, I, Either[L, R]]) extends AnyVal {
    def leftThrough[O2](f: Resolver[F, L, L] => Resolver[F, L, O2]): Resolver[F, I, Either[O2, R]] =
      self andThen f(Resolver.id[F, L]).choose(Resolver.id[F, R])

    def rightThrough[O2](f: Resolver[F, R, R] => Resolver[F, R, O2]): Resolver[F, I, Either[L, O2]] =
      self andThen Resolver.id[F, L].choose(f(Resolver.id[F, R]))

    def bothThrough[O1, O2](fa: Resolver[F, L, O1])(fb: Resolver[F, R, O2]): Resolver[F, I, Either[O1, O2]] =
      self andThen fa.choose(fb)
  }

  implicit class ResolverStreamOps[F[_], I, O](private val self: Resolver[F, I, fs2.Stream[F, O]]) extends AnyVal {
    def embedStream: Resolver[F, I, O] =
      self andThen new Resolver(Step.embedStream)

    def embedSequentialStream: Resolver[F, I, O] =
      self andThen new Resolver(Step.embedStreamFull(signal = false))
  }

  implicit class ResolverBatchOps[F[_], K, V](private val r: Resolver[F, Set[K], Map[K, V]]) extends AnyVal {
    def all[G[_]: Foldable: Functor]: Resolver[F, G[K], G[Option[V]]] =
      r.contramap[G[K]](_.toList.toSet).tupleIn.map { case (m, g) => g.map(m.get) }

    def traversable[G[_]: Traverse](implicit
        S: ShowMissingKeys[K]
    ): Resolver[F, G[K], G[V]] =
      r.contramap[G[K]](_.toList.toSet).tupleIn.emap { case (m, gks) =>
        gks
          .traverse(k => m.get(k).toValidNel(k))
          .leftMap(S.showMissingKeys(_))
          .toIor
      }

    def opt: Resolver[F, K, Option[V]] = all[Id]

    def one(implicit S: ShowMissingKeys[K]): Resolver[F, K, V] =
      traversable[Id]
  }
}

trait ShowMissingKeys[A] {
  def showMissingKeys(xs: NonEmptyList[A]): String
}

object ShowMissingKeys {
  def apply[A](implicit ev: ShowMissingKeys[A]): ShowMissingKeys[A] = ev

  def showFull[A](show: NonEmptyList[A] => String): ShowMissingKeys[A] =
    new ShowMissingKeys[A] {
      def showMissingKeys(xs: NonEmptyList[A]): String = show(xs)
    }

  def showForKey[A: Show](prefix: String): ShowMissingKeys[A] =
    showFull(xs => s"$prefix: ${xs.map(_.show).mkString_(", ")}")

  def show[A](showKey: A => String, prefix: String): ShowMissingKeys[A] =
    showForKey[A](prefix)(Show.show(showKey))
}

trait ResolverInstances {
  import cats.arrow._
  implicit def arrowChoiceForResolver[F[_]]: ArrowChoice[Resolver[F, *, *]] = new ArrowChoice[Resolver[F, *, *]] {
    override def choose[A, B, C, D](f: Resolver[F, A, C])(g: Resolver[F, B, D]): Resolver[F, Either[A, B], Either[C, D]] =
      new Resolver(Step.choose(f.underlying, g.underlying))

    override def compose[A, B, C](f: Resolver[F, B, C], g: Resolver[F, A, B]): Resolver[F, A, C] =
      new Resolver(Step.compose(g.underlying, f.underlying))

    override def first[A, B, C](fa: Resolver[F, A, B]): Resolver[F, (A, C), (B, C)] =
      new Resolver(Step.first(fa.underlying))

    override def lift[A, B](f: A => B): Resolver[F, A, B] = Resolver.lift(f)

    override def id[A]: Resolver[F, A, A] = Resolver.id
  }
}
