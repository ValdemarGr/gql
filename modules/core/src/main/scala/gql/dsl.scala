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

  def arg[A](name: String, defaultValue: A)(implicit tpe: => InLeaf[A]): NonEmptyArg[A] = {
    implicit lazy val t0 = tpe
    Arg.make[A](name, Some(default(defaultValue)))
  }

  def arg[A](name: String, default: DefaultValue[A])(implicit tpe: => In[A]): NonEmptyArg[A] = {
    implicit lazy val t0 = tpe
    Arg.make[A](name, Some(default))
  }

  object default {
    def apply[A](value: A)(implicit tpe: => InLeaf[A]): DefaultValue.Primitive[A] =
      DefaultValue.Primitive(value, tpe)

    def obj(hd: (String, DefaultValue[_]), tl: (String, DefaultValue[_])*): DefaultValue.Obj =
      DefaultValue.Obj(NonEmptyChain.of(hd, tl: _*))

    def arr[A](xs: Seq[DefaultValue[A]]): DefaultValue.Arr[A] =
      DefaultValue.Arr(xs)

    def none: DefaultValue.Null.type = DefaultValue.Null
  }

  def field[F[_], I, T, A](arg: Arg[A])(resolver: Resolver[F, (I, A), T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
    Field[F, I, T, A](arg, resolver, Eval.later(tpe))

  def field[F[_], I, T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
    Field[F, I, T, Unit](Applicative[Arg].unit, resolver.contramap[(I, Unit)] { case (i, _) => i }, Eval.later(tpe))

  // def stream[F[_], I, R, T](resolver: LeafResolver[F, (I, R), T])(f: I => fs2.Stream[F, R]): StreamResolver[F, I, R, T] =
  //   streamFallible(resolver)(i => f(i).map(_.rightIor))

  def stream[F[_]: Applicative, I, T](f: I => fs2.Stream[F, T]): StreamResolver[F, I, T, T] =
    streamFallible[F, I, T](i => f(i).map(_.rightIor))

  // def streamFallible[F[_], I, R, T](resolver: LeafResolver[F, (I, R), T])(
  //     f: I => fs2.Stream[F, IorNec[String, R]]
  // ): StreamResolver[F, I, R, T] =
  //   StreamResolver[F, I, R, T](resolver, f)

  def streamFallible[F[_], I, T](f: I => fs2.Stream[F, IorNec[String, T]])(implicit F: Applicative[F]): StreamResolver[F, I, T, T] =
    StreamResolver(f)

  def fallible[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    field(arg)(EffectResolver[F, (I, A), T] { case (i, a) => resolver(i, a) })
  }

  def fallible[F[_], I, T](resolver: I => F[Ior[String, T]])(implicit F: Functor[F], tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    fallible[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))
  }

  def eff[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit F: Functor[F], tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    fallible[F, I, T, A](arg)((i, a) => resolver(i, a).map(_.rightIor))
  }

  def eff[F[_], I, T](resolver: I => F[T])(implicit F: Functor[F], tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    eff[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))
  }

  // Not sure if this is a compiler bug or something since all type parameters except I are invariant?
  def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => Id[T])(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    eff[F, I, T, A](arg)((i, a) => F.pure(resolver(i, a)))
  }

  def pure[F[_], I, T](resolver: I => T)(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    eff[F, I, T](i => F.pure(resolver(i)))
  }

  def enum[F[_], A](name: String, hd: (String, A), tl: (String, A)*) = Enum[F, A](name, NonEmptyList(hd, tl.toList))

  final case class PartiallyAppliedInstance[B](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](pf: PartialFunction[A, B])(implicit s: => Selectable[F, B]): Instance[F, A, B] =
      Instance[F, A, B](Eval.later(s))(pf.lift)
  }

  def instance[B]: PartiallyAppliedInstance[B] = PartiallyAppliedInstance[B]()

  def interface[F[_], A](
      name: String,
      hd: (String, Field[F, A, _, _]),
      tl: (String, Field[F, A, _, _])*
  )(
      instanceHd: Instance[F, A, _],
      instanceTl: Instance[F, A, _]*
  ) =
    Interface(
      name,
      instanceHd.asInstanceOf[Instance[F, A, Any]] :: instanceTl.toList.asInstanceOf[List[Instance[F, A, Any]]],
      NonEmptyList(hd, tl.toList)
    )

  def union[F[_], A](
      name: String,
      instanceHd: Instance[F, A, _],
      instanceTl: Instance[F, A, _]*
  ) =
    Union(
      name,
      NonEmptyList(instanceHd.asInstanceOf[Instance[F, A, Any]], instanceTl.toList.asInstanceOf[List[Instance[F, A, Any]]])
    )
}
