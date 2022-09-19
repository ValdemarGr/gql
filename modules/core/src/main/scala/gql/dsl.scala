package gql

import cats._
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

  def pure[F[_]: Applicative, I, T](resolver: I => T)(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
    pure[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(implicitly, tpe)

  def pure[F[_]: Applicative, I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit tpe: => Out[F, T]): Field[F, I, T, A] =
    Field[F, I, T, A](
      arg,
      EffectResolver { case (i, a) => IorT.pure(resolver(i, a)) },
      Eval.later(tpe)
    )

  def eff[F[_]: Applicative, I, T](resolver: I => F[T])(implicit tpe: => Out[F, T]): Field[F, I, T, Unit] =
    eff[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(implicitly, tpe)

  def eff[F[_]: Applicative, I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Out[F, T]
  ): Field[F, I, T, A] =
    Field[F, I, T, A](
      arg,
      EffectResolver { case (i, a) => IorT.liftF(resolver(i, a)) },
      Eval.later(tpe)
    )

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
}
