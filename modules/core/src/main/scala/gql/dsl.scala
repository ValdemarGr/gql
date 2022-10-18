package gql

import cats._
import cats.implicits._
import gql.ast._
import gql.resolver._
import cats.data._

object dsl {
  def tpe[F[_], A](
      name: String,
      hd: (String, Field[F, A, _, _]),
      tl: (String, Field[F, A, _, _])*
  ) = Type[F, A](name, NonEmptyList(hd, tl.toList))

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

  object Internal {
    def inst[F[_], A, B](pf: PartialFunction[A, B], s: => Selectable[F, B]): Instance[F, A, Any] =
      Instance[F, A, B](Eval.later(s))(pf.lift).asInstanceOf[Instance[F, A, Any]]
  }

  implicit class InterfaceSyntax[F[_], A](val tpe: Interface[F, A]) extends AnyVal {
    def instance[B](pf: PartialFunction[A, B])(implicit s: => Selectable[F, B]): Interface[F, A] =
      tpe.copy(instances = Internal.inst[F, A, B](pf, s) :: tpe.instances)
  }

  def interface[F[_], A](
      name: String,
      hd: (String, Field[F, A, _, _]),
      tl: (String, Field[F, A, _, _])*
  ) = Interface[F, A](name, Nil, NonEmptyList(hd, tl.toList))

  implicit class UnionSyntax[F[_], A](val tpe: Union[F, A]) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit s: => Selectable[F, B]): Union[F, A] =
      tpe.copy(types = Internal.inst[F, A, B](pf, s) :: tpe.types)
  }

  final case class PartiallyAppliedUnion1[F[_], A](name: String, hd: Instance[F, A, Any]) {
    def variant[B](pf: PartialFunction[A, B])(implicit s: => Selectable[F, B]): Union[F, A] =
      Union[F, A](name, NonEmptyList.of(hd, Internal.inst[F, A, B](pf, s)), None)
  }

  final case class PartiallyAppliedUnion0[F[_], A](name: String) {
    def variant[B](pf: PartialFunction[A, B])(implicit s: => Selectable[F, B]): PartiallyAppliedUnion1[F, A] =
      PartiallyAppliedUnion1[F, A](name, Internal.inst[F, A, B](pf, s))
  }

  def union[F[_], A](name: String) = PartiallyAppliedUnion0[F, A](name)
}
