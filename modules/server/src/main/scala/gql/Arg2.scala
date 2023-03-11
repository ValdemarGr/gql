package gql

import gql.std.FreeApply

import cats.data._
import cats._
import cats.implicits._

final case class Arg2[+A](impl: Arg2.Impl[?, A]) {
  def emap[B](f: A => Either[String, B]): Arg2[B] =
    Arg2(impl.emap(f))
}

object Arg2 {
  final case class Impl[A, +B](
      underlying: FreeApply[ArgValue, A],
      emapping: A => ValidatedNec[String, B]
  ) {
    def emap[B2](f: B => Either[String, B2]): Impl[A, B2] =
      Impl(
        underlying,
        emapping.andThen(_.toEither.flatMap(f(_).leftMap(NonEmptyChain.one(_))).toValidated)
      )
  }

  implicit val applyForArg2: Apply[Arg2] = {
    new Apply[Arg2] {
      override def map[A, B](fa: Arg2[A])(f: A => B): Arg2[B] = Arg2 {
        fa.impl match {
          case impl: Impl[a, A] => Impl(impl.underlying, impl.emapping.andThen(_.map(f)))
        }
      }

      override def ap[A, B](ff: Arg2[A => B])(fa: Arg2[A]): Arg2[B] = Arg2 {
        (ff.impl, fa.impl) match {
          case (implf: Impl[a1, A => B], impla: Impl[a2, A]) =>
            Impl[ValidatedNec[String, B], B](
              (implf.underlying.map(implf.emapping), impla.underlying.map(impla.emapping)).mapN((_, _).mapN(_(_))),
              x => x
            )
        }
      }
    }
  }

  final case class ArgValue[A](
      name: String,
      defaultValue: Option[Value],
      description: Option[String],
      embedding: Eval[gql.ast.In[A]]
  ) {
    def document(description: String) = copy(description = Some(description))

    def default(value: Value) = copy(defaultValue = Some(value))
  }
}
