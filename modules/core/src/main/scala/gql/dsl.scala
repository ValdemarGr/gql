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

  final class PartiallyAppliedSignalFinal[F[_], I, I2, R, A](
      arg: Arg[A],
      ref: StreamRef[F, I2, R],
      head: I2 => F[Ior[String, R]],
      adjust: (I, A) => I2
  ) {
    def apply[T](resolver: LeafResolver[F, (I2, R), T])(implicit tpe: => Out[F, T]) =
      Field[F, I, T, A](
        arg,
        SignalResolver[F, (I, A), R, T](
          resolver.contramap[((I, A), R)] { case ((i, a), r) => (adjust(i, a), r) },
          { case (i, a) => IorT(head(adjust(i, a))) },
          ref.contramap { case (i, a) => adjust(i, a) }
        ),
        Eval.later(tpe)
      )

    def fallible[T](resolver: (I2, R) => F[Ior[String, T]])(implicit F: Functor[F], tpe: => Out[F, T]) = {
      implicit lazy val t0 = tpe
      apply(EffectResolver[F, (I2, R), T]({ case (i2, r) => resolver(i2, r) }))
    }

    def eff[T](resolver: (I2, R) => F[T])(implicit F: Functor[F], tpe: => Out[F, T]) = {
      implicit lazy val t0 = tpe
      fallible[T]((i, r) => resolver(i, r).map(_.rightIor))
    }

    def pure[T](resolver: (I2, R) => T)(implicit F: Applicative[F], tpe: => Out[F, T]) = {
      implicit lazy val t0 = tpe
      eff[T] { case (i2, r) => F.pure(resolver(i2, r)) }
    }
  }

  final class PartiallyAppliedSignalHead[F[_], I, I2, R, A](arg: Arg[A], ref: StreamRef[F, I2, R], adjust: (I, A) => I2) {
    def apply[T](head: I2 => F[Ior[String, R]]) =
      new PartiallyAppliedSignalFinal[F, I, I2, R, A](arg, ref, head, adjust)

    def eff[T](head: I2 => F[R])(implicit F: Functor[F]) =
      apply[T](i2 => head(i2).map(_.rightIor))

    def pure[T](head: I2 => R)(implicit F: Applicative[F]) =
      eff[T](i2 => F.pure(head(i2)))
  }

  def signal[F[_], I, R, A](arg: Arg[A], sr: StreamRef[F, (I, A), R]): PartiallyAppliedSignalHead[F, I, (I, A), R, A] =
    new PartiallyAppliedSignalHead[F, I, (I, A), R, A](arg, sr, (i, a) => (i, a))

  def signal[F[_], I, R](sr: StreamRef[F, I, R]): PartiallyAppliedSignalHead[F, I, I, R, Unit] =
    new PartiallyAppliedSignalHead[F, I, I, R, Unit](Applicative[Arg].unit, sr, (i, _) => i)

  def pure[F[_], I, T](resolver: I => T)(implicit F: Applicative[F]): EffectResolver[F, I, T] =
    EffectResolver[F, I, T](i => F.pure(resolver(i).rightIor))

  def eff[F[_], I, T](resolver: I => F[T])(implicit F: Applicative[F]): EffectResolver[F, I, T] =
    EffectResolver[F, I, T](i => resolver(i).map(_.rightIor))

  def eff2[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit F: Functor[F], tpe: => Out[F, T]): Field[F, I, T, A] =
    Field[F, I, T, A](
      arg,
      EffectResolver[F, (I, A), T] { case (i, a) => resolver(i, a).map(_.rightIor) },
      Eval.later(tpe)
    )

  def eff2[F[_], I, T](resolver: I => F[T])(implicit F: Functor[F], tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    eff2[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))
  }

  def pure2[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, A] = {
    implicit lazy val t0 = tpe
    eff2[F, I, T, A](arg)((i, a) => F.pure(resolver(i, a)))
  }

  def pure2[F[_], I, T](resolver: I => T)(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, Unit] = {
    implicit lazy val t0 = tpe
    eff2[F, I, T](i => F.pure(resolver(i)))
  }

  // def fallible[F[_], I, T](resolver: I => F[T])(implicit F: Applicative[F], tpe: => Out[F, T]): Field[F, I, T, Unit] = {
  //   implicit lazy val t0 = tpe
  //   eff2[F, I, T, Unit](Applicative[Arg].unit)((i, _) => resolver(i))
  // }

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
