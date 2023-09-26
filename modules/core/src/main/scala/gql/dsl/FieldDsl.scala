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
package gql.dsl

import gql.resolver._
import gql.ast._
import gql._
import cats.data._
import cats._
import gql.dsl.aliases._

trait FieldDsl[F[_]] {
  def fields[A](hd: (String, Field[F, A, ?]), tl: (String, Field[F, A, ?])*): Fields[F, A] =
    FieldDsl.fields(hd, tl: _*)

  def eff[I] = FieldDsl.eff[I]

  def lift[I] = FieldDsl.lift[I]

  def build[I] = FieldDsl.builder[F, I]

  def builder[I] = FieldDsl.builder[F, I]

  def abst[T](implicit tpe: => Out[F, T]): AbstractField[F, T] =
    FieldDsl.abst[F, T](tpe)

  def abstWith[T, A](arg: Arg[A])(implicit tpe: => Out[F, T]): AbstractField[F, T] =
    FieldDsl.abstWith[F, T, A](arg)(tpe)

  def abstGroup(
      hd: (String, AbstractField[F, ?]),
      tl: (String, AbstractField[F, ?])*
  ): AbstractFields[F] =
    FieldDsl.abstGroup(hd, tl: _*)

  def optType[A, B](resolver: Resolver[F, A, B])(implicit tpe: => Out[F, B]): Out[F, Option[A]] =
    FieldDsl.optType[F, A, B](resolver)(tpe)

  def optType[A](implicit tpe: => In[A]): In[Option[A]] =
    FieldDsl.optType[A](tpe)

  def arrType[A, C, B](toSeq: C => Seq[A])(resolver: Resolver[F, A, B])(implicit
      tpe: => Out[F, B]
  ): OutArr[F, A, C, B] =
    FieldDsl.arrType[F, A, C, B](toSeq)(resolver)(tpe)

  def arrType[A, G[x] <: Seq[x], B](resolver: Resolver[F, A, B])(implicit
      tpe: => Out[F, B]
  ): OutArr[F, A, G[A], B] =
    FieldDsl.arrType(resolver)(tpe)

  implicit def fieldDslFullFieldsOps[A](fields: Fields[F, A]): FieldDsl.FieldsOps[F, A] =
    FieldDsl.fieldDslFullFieldsOps[F, A](fields)
}

trait FieldDslFull {
  def fields[F[_], A](hd: (String, Field[F, A, ?]), tl: (String, Field[F, A, ?])*): Fields[F, A] =
    NonEmptyList[(String, Field[F, A, ?])](hd, tl.toList)

  def eff[I] = new FieldDsl.PartiallyAppliedEff[I]

  def lift[I] = new FieldDsl.PartiallyAppliedLift[I]

  def build[F[_], I] = new FieldBuilder[F, I] {}

  def builder[F[_], I] = new FieldDsl.PartiallyAppliedFieldBuilder[F, I]

  def abst[F[_], T](implicit tpe: => Out[F, T]): AbstractField[F, T] =
    AbstractField[F, T](None, Eval.later(tpe))

  def abstWith[F[_], T, A](arg: Arg[A])(implicit tpe: => Out[F, T]): AbstractField[F, T] =
    AbstractField[F, T](Some(arg), Eval.later(tpe))

  def abstGroup[F[_]](
      hd: (String, AbstractField[F, ?]),
      tl: (String, AbstractField[F, ?])*
  ): AbstractFields[F] =
    NonEmptyList(hd, tl.toList)

  def optType[F[_], A, B](resolver: Resolver[F, A, B])(implicit tpe: => Out[F, B]): Out[F, Option[A]] =
    OutOpt(tpe, resolver)

  def optType[A](implicit tpe: => In[A]): In[Option[A]] = InOpt(tpe)

  def arrType[F[_], A, C, B](toSeq: C => Seq[A])(resolver: Resolver[F, A, B])(implicit
      tpe: => Out[F, B]
  ): OutArr[F, A, C, B] =
    OutArr(tpe, toSeq, resolver)

  def arrType[F[_], A, G[x] <: Seq[x], B](resolver: Resolver[F, A, B])(implicit
      tpe: => Out[F, B]
  ): OutArr[F, A, G[A], B] =
    OutArr(tpe, identity, resolver)

  implicit def fieldDslFullFieldsOps[F[_], A](fields: Fields[F, A]): FieldDsl.FieldsOps[F, A] =
    new FieldDsl.FieldsOps[F, A](fields)
}

object FieldDsl extends FieldDslFull {
  final class PartiallyAppliedEff[I](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T, A](arg: Arg[A])(resolver: (A, I) => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.liftF[F, (A, I)] { case (a, i) => resolver(a, i) }.contraArg(arg), Eval.later(tpe))

    def apply[F[_], T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.liftF(resolver), Eval.later(tpe))
  }

  final class FieldsOps[F[_], A](private val fields: Fields[F, A]) extends AnyVal {
    def addFields(xs: (String, Field[F, A, ?])*) =
      fields concat xs.toList

    def addFieldsNel(xs: NonEmptyList[(String, Field[F, A, ?])]) =
      fields concat xs.toList

    def mapValues[B](f: Field[F, A, ?] => Field[F, B, ?]) =
      fields.map { case (k, v) => k -> f(v) }

    def resolveBy[I2](f: Resolver[F, A, A] => Resolver[F, I2, A]): Fields[F, I2] =
      mapValues(_.compose(f(Resolver.id)))

    def compose[I2](resolver: Resolver[F, I2, A]): Fields[F, I2] =
      resolveBy[I2](_.compose(resolver))
  }

  final class PartiallyAppliedFieldBuilder[F[_], I](private val dummy: Boolean = false) extends AnyVal {
    def apply[A](f: FieldBuilder[F, I] => A): A = f(build[F, I])
  }

  final class PartiallyAppliedLift[I](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T, A](arg: Arg[A])(resolver: (A, I) => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.lift[F, (A, I)] { case (a, i) => resolver(a, i) }.contraArg(arg), Eval.later(tpe))

    def apply[F[_], T](resolver: I => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.lift[F, I](resolver), Eval.later(tpe))
  }
}

trait FieldBuilder[F[_], I] {
  def tpe(
      name: String,
      hd: (String, Field[F, I, ?]),
      tl: (String, Field[F, I, ?])*
  ): Type[F, I] = TypeDsl.tpe[F, I](name, hd, tl: _*)

  def fields(
      hd: (String, Field[F, I, ?]),
      tl: (String, Field[F, I, ?])*
  ): Fields[F, I] = FieldDsl.fields[F, I](hd, tl: _*)

  def from[T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
    Field[F, I, T](resolver, Eval.later(tpe))

  def apply[T](f: Resolver[F, I, I] => Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
    Field[F, I, T](f(Resolver.id[F, I]), Eval.later(tpe))

  def lift = new FieldDsl.PartiallyAppliedLift[I]

  def eff[T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
    Field(Resolver.liftF(resolver), Eval.later(tpe))
}
