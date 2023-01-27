/*
 * Copyright 2022 Valdemar Grange
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
package gql

import cats._
import cats.implicits._
import gql.ast._
import gql.resolver._
import cats.data._
import scala.reflect.ClassTag

object dsl { 
  def tpeNel[F[_], A](
      name: String,
      entries: NonEmptyList[(String, Field[F, A, ?])]
  ) = Type[F, A](name, entries, Nil)

  def tpe[F[_], A](
      name: String,
      hd: (String, Field[F, A, ?]),
      tl: (String, Field[F, A, ?])*
  ) = Type[F, A](name, NonEmptyList(hd, tl.toList), Nil)

  def fields[F[_], A, B](f: B => A)(
      hd: (String, Field[F, A, ?]),
      tl: (String, Field[F, A, ?])*
  ): NonEmptyList[(String, Field[F, B, ?])] =
    NonEmptyList[(String, Field[F, A, ?])](hd, tl.toList).map { case (k, v) => (k, v.contramap(f)) }

  def fields[F[_], A](
      hd: (String, Field[F, A, ?]),
      tl: (String, Field[F, A, ?])*
  ): NonEmptyList[(String, Field[F, A, ?])] =
    NonEmptyList[(String, Field[F, A, ?])](hd, tl.toList)

  def input[A](
      name: String,
      fields: Arg[A]
  ): Input[A] = Input(name, fields)

  def arg[A](name: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), None, None))

  def arg[A](name: String, description: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), None, Some(description)))

  def arg[A](name: String, default: Value)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), Some(default), None))

  def arg[A](name: String, default: Value, description: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), Some(default), Some(description)))
  /*
  final case class PartiallyAppliedArgFull[A](private val dummy: Boolean = false) extends AnyVal {
    def apply[B](name: String, default: Option[Value], description: Option[String])(
        f: ArgParam[A] => Either[String, B]
    )(implicit tpe: => In[A]): Arg[B] =
      Arg.makeFrom[A, B](ArgValue(name, Eval.later(tpe), default, description))(f)
  }*/

  //def argFull[A] = new PartiallyAppliedArgFull[A]

  object value {
    def scalar[F[_], A](value: A)(implicit tpe: => Scalar[F, A]) =
      tpe.encoder(value)

    def fromEnum[F[_], A](value: A)(implicit tpe: => Enum[F, A]) =
      tpe.revm.get(value).map(enumValue)

    def enumValue(value: String) = Value.EnumValue(value)

    def arr(xs: Value*) = Value.ArrayValue(xs.toVector)

    def obj(xs: (String, Value)*) = Value.ObjectValue(xs.toMap)

    def nullValue = Value.NullValue
  }

  final class PartiallyAppliedField[I](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field[F, I, T](resolver, Eval.later(tpe))
  }

  def field[I] = new PartiallyAppliedField[I]

  final class PartiallyAppliedEff[I](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], T, A](arg: Arg[A])(resolver: (A, I) => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.eval[F, (A, I), T] { case (a, i) => resolver(a, i) }.contraArg(arg), Eval.later(tpe))

    def apply[F[_], T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.eval(resolver), Eval.later(tpe))
  }

  def eff[I] = new PartiallyAppliedEff[I]

  final class PartiallyAppliedLift[F[_], I](private val dummy: Boolean = false) extends AnyVal {
    def apply[T, A](arg: Arg[A])(resolver: (A, I) => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.lift[F, (A, I), T] { case (a, i) => resolver(a, i) }.contraArg(arg), Eval.later(tpe))

    def apply[T](resolver: I => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.lift[F, I, T](resolver), Eval.later(tpe))
  }

  def lift[F[_], I] = new PartiallyAppliedLift[F, I]

  final class FieldBuilder[F[_], I](private val dummy: Boolean = false) extends AnyVal {
    def tpe(
      name: String,
      hd: (String, Field[F, I, ?]),
      tl: (String, Field[F, I, ?])*
    ): Type[F, I] = gql.dsl.tpe[F, I](name, hd, tl: _*)

    def fields(
      hd: (String, Field[F, I, ?]),
      tl: (String, Field[F, I, ?])*
    ): NonEmptyList[(String, Field[F, I, ?])] = gql.dsl.fields[F, I](hd, tl: _*)

    def from[T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field[F, I, T](resolver, Eval.later(tpe))

    def apply[T](f: Resolver[F, I, I] => Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field[F, I, T](f(Resolver.id[F, I]), Eval.later(tpe))

    def lift = new PartiallyAppliedLift[F, I]

    def eff[T, A](arg: Arg[A])(resolver: (A, I) => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.eval[F, (A, I), T] { case (a, i) => resolver(a, i) }.contraArg(arg), Eval.later(tpe))

    def eff[T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T] =
      Field(Resolver.eval(resolver), Eval.later(tpe))
  }
  
  def builder[F[_], I] = new FieldBuilder[F, I]

  final class PartiallyAppliedFieldBuilder[F[_], I](private val dummy: Boolean = false) extends AnyVal {
    def apply[A](f: FieldBuilder[F, I] => A): A = f(builder[F, I])
  }

  def build[F[_], I] = new PartiallyAppliedFieldBuilder[F, I]

  def abst[F[_], T](implicit tpe: => Out[F, T]): AbstractField[F, T] =
    AbstractField[F, T](Applicative[Arg].unit, Eval.later(tpe))

  def abstWith[F[_], T, A](arg: Arg[A])(implicit tpe: => Out[F, T]): AbstractField[F, T] =
    AbstractField[F, T](arg, Eval.later(tpe))

  def abstGroup[F[_]](
      hd: (String, AbstractField[F, ?]),
      tl: (String, AbstractField[F, ?])*
  ): NonEmptyList[(String, AbstractField[F, ?])] =
    NonEmptyList(hd, tl.toList)

  def enumVal[A](value: A): EnumValue[A] =
    EnumValue(value)

  def enumType[F[_], A](name: String, hd: (String, EnumValue[? <: A]), tl: (String, EnumValue[? <: A])*) =
    Enum[F, A](name, NonEmptyList(hd, tl.toList))

  def interfaceNel[F[_], A](
      name: String,
      fields: NonEmptyList[(String, AbstractField[F, ?])]
  ): Interface[F, A] = Interface[F, A](name, fields, Nil)

  def interface[F[_], A](
      name: String,
      hd: (String, AbstractField[F, ?]),
      tl: (String, AbstractField[F, ?])*
  ): Interface[F, A] = interfaceNel[F, A](name, NonEmptyList(hd, tl.toList))

  def interfaceFromNel[F[_], A](
      name: String,
      fields: NonEmptyList[(String, Field[F, A, ?])]
  ): Interface[F, A] = interfaceNel[F, A](name, fields.map { case (k, v) => k -> v.asAbstract })

  def interfaceFrom[F[_], A](
      name: String,
      hd: (String, Field[F, A, ?]),
      tl: (String, Field[F, A, ?])*
  ): Interface[F, A] = interfaceFromNel[F, A](name, NonEmptyList(hd, tl.toList))

  def union[F[_], A](name: String) = PartiallyAppliedUnion0[F, A](name)

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

  def arrType[A](implicit tpe: => In[A]): In[Seq[A]] = InArr[A, Seq[A]](tpe, _.asRight)

  implicit class TypeSyntax[F[_], A](private val tpe: Type[F, A]) extends AnyVal {
    def implements[B](pf: PartialFunction[B, A])(implicit interface: => Interface[F, B]): Type[F, A] =
      tpe.copy(implementations = Implementation(Eval.later(interface))(pf.lift) :: tpe.implementations)

    def subtypeOf[B](implicit ev: A <:< B, tag: ClassTag[A], interface: => Interface[F, B]): Type[F, A] =
      implements[B] { case a: A => a }(interface)

    def addFields(xs: (String, Field[F, A, ?])*) =
      tpe.copy(fields = tpe.fields concat xs.toList)
  }

  implicit class InterfaceSyntax[F[_], A](private val tpe: Interface[F, A]) extends AnyVal {
    def implements[B](implicit interface: => Interface[F, B]): Interface[F, A] =
      tpe.copy(implementations = Eval.later(interface) :: tpe.implementations)

    def addAbstractFields(xs: (String, AbstractField[F, ?])*): Interface[F, A] =
      tpe.copy(fields = tpe.fields concat xs.toList)

    def addFields(xs: (String, Field[F, A, ?])*): Interface[F, A] =
      tpe.copy(fields = tpe.fields concat xs.toList.map { case (k, v) => k -> v.asAbstract })
  }

  implicit class UnionSyntax[F[_], A](private val tpe: Union[F, A]) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      tpe.copy(types = Variant[F, A, B](Eval.later(innerTpe))(pf.lift) :: tpe.types)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class PartiallyAppliedUnion1[F[_], A](name: String, hd: Variant[F, A, ?]) {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      Union[F, A](name, NonEmptyList.of(hd, Variant[F, A, B](Eval.later(innerTpe))(pf.lift)), None)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class PartiallyAppliedUnion0[F[_], A](name: String) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      PartiallyAppliedUnion1[F, A](name, Variant[F, A, B](Eval.later(innerTpe))(pf.lift))

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }
}
