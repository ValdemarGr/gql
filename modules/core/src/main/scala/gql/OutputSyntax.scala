package gql

import cats.implicits._
import cats.data._
import cats._
import shapeless.Lazy
import scala.reflect.ClassTag
import gql.resolver._
import gql.out._

class FieldBuilder[F[_], I] {
  def apply[A](r: Resolver[F, I, A])(implicit tpe: => Output[F, A]): Field[F, I, A, Unit] = {
    implicit lazy val t = tpe
    apply[Unit, A](Applicative[Arg].unit)(r.contramap[(I, Unit)] { case (i, _) => i })
  }

  def apply[Ag, A](arg: Arg[Ag])(r: Resolver[F, (I, Ag), A])(implicit tpe: => Output[F, A]): Field[F, I, A, Ag] =
    Field(
      arg,
      r,
      Eval.later(tpe)
    )
}

abstract class OutputSyntax {
  def fields[F[_], I](
      hd: (String, Field[F, I, _, _]),
      tl: (String, Field[F, I, _, _])*
  ): NonEmptyList[(String, Field[F, I, _, _])] =
    NonEmptyList(hd, tl.toList)

  def field[F[_], I, A](r: Resolver[F, I, A])(implicit tpe: => Output[F, A]) =
    Field(
      Applicative[Arg].unit,
      r.contramap[(I, Unit)] { case (i, _) => i },
      Eval.later(tpe)
    )

  def obj2[F[_], I](name: String)(build: FieldBuilder[F, I] => NonEmptyList[(String, Field[F, I, _, _])]) =
    Obj[F, I](name, build(new FieldBuilder[F, I]))

  def obj[F[_], A](
      name: String,
      hd: (String, Field[F, A, _, _]),
      tl: (String, Field[F, A, _, _])*
  ) = Obj[F, A](name, NonEmptyList(hd, tl.toList))

  def union[F[_], A](
      name: String,
      hd: Instance[F, A, _],
      tl: Instance[F, A, _]*
  ) = Union[F, A](
    name,
    NonEmptyList(hd, tl.toList).asInstanceOf[NonEmptyList[Instance[F, A, Any]]]
  )

  def interface[F[_], A](
      o: Obj[F, A],
      xs: Instance[F, A, _]*
  ) = Interface[F, A](
    o.name,
    xs.toList.asInstanceOf[List[Instance[F, A, Any]]],
    o.fields
  )

  def contra[B] = OutputSyntax.PartiallyAppliedContra[B]()

  def effect[F[_]: Applicative, I, T](resolver: I => F[T])(implicit tpe: => Output[F, T]): Field[F, I, T, Unit] =
    effect[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(implicitly, tpe)

  def full2[F[_]: Applicative, I, T](resolver: I => IorT[F, String, T])(implicit
      tpe: => Output[F, T]
  ): Field[F, I, T, Unit] =
    Field[F, I, T, Unit](
      Applicative[Arg].unit,
      EffectResolver { case (i, _) => resolver(i) },
      Eval.later(tpe)
    )

  def effect[F[_]: Applicative, I, T, A](arg: Arg[A])(resolver: (I, A) => F[T])(implicit
      tpe: => Output[F, T]
  ): Field[F, I, T, A] =
    Field[F, I, T, A](
      arg,
      EffectResolver { case (i, a) => IorT.liftF(resolver(i, a)) },
      Eval.later(tpe)
    )

  def full[F[_], I, T](resolver: I => IorT[F, String, T]) =
    EffectResolver[F, I, T](resolver)

  def eff[F[_]: Applicative, I, T](resolver: I => F[T]) =
    EffectResolver[F, I, T](x => IorT.liftF(resolver(x)))

  def pur[F[_], I, T](resolver: I => T)(implicit F: Applicative[F]) =
    EffectResolver[F, I, T](resolver.andThen(IorT.pure(_)))

  def pure[F[_]: Applicative, I, T](resolver: I => T)(implicit tpe: => Output[F, T]): Field[F, I, T, Unit] =
    pure[F, I, T, Unit](Applicative[Arg].unit) { case (i, _) => resolver(i) }(implicitly, tpe)

  def pure[F[_]: Applicative, I, T, A](arg: Arg[A])(resolver: (I, A) => T)(implicit tpe: => Output[F, T]): Field[F, I, T, A] =
    Field[F, I, T, A](
      arg,
      EffectResolver { case (i, a) => IorT.pure(resolver(i, a)) },
      Eval.later(tpe)
    )

  def arg[A](name: String, default: Option[A] = None)(implicit tpe: Input[A]): Arg[A] =
    Arg.initial[A](ArgParam(name, tpe, default))
}

object OutputSyntax {
  case class PartiallyAppliedContra[B](val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](pf: PartialFunction[A, B])(implicit ol: ObjLike[F, B]): Instance[F, A, B] =
      Instance[F, A, B](ol)(pf.lift)
  }
}
