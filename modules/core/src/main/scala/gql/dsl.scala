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

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: In[A]): Arg[A] =
    Arg.initial[A](ArgParam(name, tpe, default))

  def field[F[_], I, T, A](arg: Arg[A])(resolver: Resolver[F, (I, A), T])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
    Field[F, I, T, A](arg, resolver, Eval.later(tpe))

  def field[F[_], I, T](resolver: Resolver[F, I, T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
    Field[F, I, T, Unit](Applicative[Arg].unit, resolver.contramap[(I, Unit)] { case (i, _) => i }, Eval.later(tpe))

  // final class PartiallyAppliedStream[F[_], I, I2, A](arg: Arg[A], adjust: (I, A) => I2) {
  //   def apply[T](f: I2 => fs2.Stream[F, T])(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, A] =
  //     Field(
  //       arg,
  //       StreamResolver[F, (I, A), T, T](
  //         EffectResolver[F, ((I, A), T), T] { case ((_, _), t) => F.pure(t.rightIor) },
  //         { case (i, a) => f(adjust(i, a)).map(_.rightIor) }
  //       ),
  //       Eval.later(tpe)
  //     )
  // }

  // def stream[F[_], I] = new PartiallyAppliedStream[F, I, I, Unit](Applicative[Arg].unit, (i, _) => i)

  // def stream[F[_], I, A](arg: Arg[A]) = new PartiallyAppliedStream[F, I, (I, A), A](arg, (i, a) => (i, a))

  // def stream[F[_], I] = new PartiallyAppliedStream[F, I, I, Unit](Applicative[Arg].unit, (i, _) => i)

  // def stream[F[_], I, A](arg: Arg[A]) = new PartiallyAppliedStream[F, I, (I, A), A](arg, (i, a) => (i, a))

  // def stream[F[_], I, T, A](arg: Arg[A])(s: (I, A) => fs2.Stream[F, IorNec[String, T]])(implicit F: Applicative[F], tpe: => Out[F, T]) = {
  //   implicit lazy val t0 = tpe
  //   field[F, I, T, A](arg)(
  //     StreamResolver[F, (I, A), T, T](EffectResolver[F, ((I, A), T), T] { case (_, t) => F.pure(t.rightIor) }, { case (i, a) => s(i, a) })
  //   )
  // }

  def fallible[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[Ior[String, T]])(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
    Field[F, I, T, A](
      arg,
      EffectResolver[F, (I, A), T] { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

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

  def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, A] = {
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
