package gql

import cats.implicits._
import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag
import gql.resolver._

class FieldBuilder[F[_], I] {
  def apply[A](r: Resolver[F, I, A])(implicit tpe: => Output[F, A]): Output.Field[F, I, A, Unit] = {
    implicit lazy val t = tpe
    apply[Unit, A](Applicative[Arg].unit)(r.contramap[(I, Unit)] { case (i, _) => i })
  }

  def apply[Ag, A](arg: Arg[Ag])(r: Resolver[F, (I, Ag), A])(implicit tpe: => Output[F, A]): Output.Field[F, I, A, Ag] =
    Output.Field(
      arg,
      r,
      Eval.later(tpe)
    )
}

abstract class OutputSyntax {
  def fields[F[_], I](
      hd: (String, Output.Field[F, I, _, _]),
      tl: (String, Output.Field[F, I, _, _])*
  ): NonEmptyList[(String, Output.Field[F, I, _, _])] =
    NonEmptyList(hd, tl.toList)

  def field[F[_], I, A](r: Resolver[F, I, A])(implicit tpe: => Output[F, A]) =
    Output.Field(
      Applicative[Arg].unit,
      r.contramap[(I, Unit)] { case (i, _) => i },
      Eval.later(tpe)
    )

  def obj2[F[_], I](name: String)(build: FieldBuilder[F, I] => NonEmptyList[(String, Output.Field[F, I, _, _])]) =
    Output.Obj[F, I](name, build(new FieldBuilder[F, I]))

  def obj[F[_], A](
      name: String,
      hd: (String, Output.Field[F, A, _, _]),
      tl: (String, Output.Field[F, A, _, _])*
  ) = Output.Obj[F, A](name, NonEmptyList(hd, tl.toList))

  def union[F[_], A](
      name: String,
      hd: Output.Unification.Instance[F, A, _],
      tl: Output.Unification.Instance[F, A, _]*
  ) = Output.Union[F, A](
    name,
    NonEmptyList(hd, tl.toList).map(x => x.ol.name -> x.asInstanceOf[Output.Unification.Instance[F, A, Any]]).toNem
  )

  def interface[F[_], A](
      o: Output.Obj[F, A],
      hd: Output.Unification.Instance[F, A, _],
      tl: Output.Unification.Instance[F, A, _]*
  ) = Output.Interface[F, A](
    o.name,
    NonEmptyList(hd, tl.toList).map(x => x.ol.name -> x.asInstanceOf[Output.Unification.Instance[F, A, Any]]).toList.toMap,
    o.fields
  )

  def contra[B] = OutputSyntax.PartiallyAppliedContra[B]()

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Output.Field[F, I, T, Unit] =
    effect[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(tpe)

  def effect[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Output[F, T]
  ): Output.Field[F, I, T, A] =
    Output.Field[F, I, T, A](
      arg,
      EffectResolver { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def eff[F[_], I, T](resolver: I => F[T]) =
    EffectResolver[F, I, T](resolver)

  def pur[F[_], I, T](resolver: I => T) =
    PureResolver[F, I, T](resolver)

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Output.Field[F, I, T, Unit] =
    pure[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(tpe)

  def pure[F[_], I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit tpe: => Output[F, T]): Output.Field[F, I, T, A] =
    Output.Field[F, I, T, A](
      arg,
      PureResolver { case (i, a) => resolver(i, a) },
      Eval.later(tpe)
    )

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Arg[A] =
    Arg.initial[A](ArgParam(name, tpe, default))
}

object OutputSyntax {
  case class PartiallyAppliedContra[B](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](pf: PartialFunction[A, B])(implicit ol: ObjectLike[F, B]): Output.Unification.Instance[F, A, B] =
      Output.Unification.Instance[F, A, B](ol)(Output.Unification.Specify.make(pf.lift))
  }
}
