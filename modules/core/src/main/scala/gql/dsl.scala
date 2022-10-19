package gql

import cats._
import cats.implicits._
import gql.ast._
import gql.resolver._
import cats.data._
import scala.reflect.ClassTag

object dsl {
  def tpe[F[_], A](
    name: String,
    hd: (String, Field[F, A, _, _]),
    tl: (String, Field[F, A, _, _])*
  ) = Type[F, A](name, NonEmptyList(hd, tl.toList), Nil)

  def input[A](
    name: String,
    fields: NonEmptyArg[A]
  ): Input[A] = Input(name, fields)

  def arg[A](name: String)(implicit tpe: => In[A]): NonEmptyArg[A] = {
    implicit lazy val t0 = tpe
    Arg.make[A](name, None)
  }

  def arg[A](name: String, default: Value)(implicit tpe: => In[A]): NonEmptyArg[A] = {
    implicit lazy val t0 = tpe
    Arg.make[A](name, Some(default))
  }

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

  def field[F[_], I, T, A](arg: Arg[A])(resolver: Resolver[F, (I, A), T])(implicit
    tpe: => Out[F, T]
  ): Field[F, I, T, A] =
    Field[F, I, T, A](arg, resolver, Eval.later(tpe))

  def field[F[_], I, T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
    Field[F, I, T, Unit](Applicative[Arg].unit, resolver.contramap[(I, Unit)] { case (i, _) => i }, Eval.later(tpe))

  def stream[F[_], I, T](f: I => fs2.Stream[F, T]): StreamResolver[F, I, T, T] =
    streamFallible[F, I, T](i => f(i).map(_.rightIor))

  def streamFallible[F[_], I, T](f: I => fs2.Stream[F, IorNec[String, T]]): StreamResolver[F, I, T, T] =
    StreamResolver(f)

  def fallible[F[_], I, T, A](
    arg: Arg[A]
  )(resolver: (I, A) => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    field(arg)(FallibleResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })
  }

  def fallible[F[_], I, T](resolver: I => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    fallible[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))
  }

  def eff[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    field(arg)(EffectResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })
  }

  def eff[F[_], I, T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    eff[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))
  }

  // Not sure if this is a compiler bug or something since all type parameters except I are invariant?
  def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    field(arg)(PureResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })
  }

  def pure[F[_], I, T](resolver: I => Id[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    field(PureResolver[F, I, T](resolver))
  }

  def enumVal[A](value: A): EnumValue[A] =
    EnumValue(value)

  def enumType[F[_], A](name: String, hd: (String, EnumValue[A]), tl: (String, EnumValue[A])*) =
    Enum[F, A](name, NonEmptyList(hd, tl.toList))

  def interface[F[_], A](
    name: String,
    hd: (String, Field[F, A, _, _]),
    tl: (String, Field[F, A, _, _])*
  ) = Interface[F, A](name, NonEmptyList(hd, tl.toList), Nil)

  def union[F[_], A](name: String) = PartiallyAppliedUnion0[F, A](name)

  implicit class TypeSyntax[F[_], A](val tpe: Type[F, A]) extends AnyVal {
    def implements[B](pf: PartialFunction[B, A])(implicit interface: => Interface[F, B]): Type[F, A] =
      tpe.copy(implementations = Implementation(Eval.later(interface))(pf.lift) :: tpe.implementations)

    def subtypeOf[B](implicit tag: ClassTag[A], interface: => Interface[F, B]): Type[F, A] =
      implements[B] { case a: A => a }(interface)

    def addFields(xs: (String, Field[F, A, _, _])*) =
      tpe.copy(fields = tpe.fields concat xs.toList)
  }

  implicit class InterfaceSyntax[F[_], A](val tpe: Interface[F, A]) extends AnyVal {
    def implements[B](pf: PartialFunction[B, A])(implicit interface: => Interface[F, B]): Interface[F, A] =
      tpe.copy(implementations = Implementation(Eval.later(interface))(pf.lift) :: tpe.implementations)

    def subtypeOf[B](implicit tag: ClassTag[A], interface: => Interface[F, B]): Interface[F, A] =
      implements[B] { case a: A => a }(interface)

    def addFields(hd: (String, Field[F, A, _, _]), tl: (String, Field[F, A, _, _])*) =
      tpe.copy(fields = tpe.fields concat (hd :: tl.toList))
  }

  implicit class UnionSyntax[F[_], A](val tpe: Union[F, A]) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      tpe.copy(types = Variant[F, A, B](Eval.later(innerTpe))(pf.lift) :: tpe.types)

    def subtype[B: ClassTag](implicit innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class PartiallyAppliedUnion1[F[_], A](name: String, hd: Variant[F, A, _]) {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      Union[F, A](name, NonEmptyList.of(hd, Variant[F, A, B](Eval.later(innerTpe))(pf.lift)), None)

    def subtype[B: ClassTag](implicit innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class PartiallyAppliedUnion0[F[_], A](name: String) {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      PartiallyAppliedUnion1[F, A](name, Variant[F, A, B](Eval.later(innerTpe))(pf.lift))

    def subtype[B: ClassTag](implicit innerTpe: => Type[F, B]): PartiallyAppliedUnion1[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }
}
