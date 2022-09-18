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

  def enum[A](name: String, hd: (String, A), tl: (String, A)*) = Enum(name, NonEmptyList(hd, tl.toList))
}
