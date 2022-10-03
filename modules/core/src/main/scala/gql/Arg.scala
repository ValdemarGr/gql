package gql

import cats.data._
import cats._
import cats.implicits._
import gql.ast._

trait Arg[A] {
  def entries: Chain[ArgValue[_]]

  def decode: Map[String, _] => A
}

object Arg {
  def onePrimitive[A](name: String, default: Option[A] = None)(implicit input: => InLeaf[A]): NonEmptyArg[A] =
    NonEmptyArg[A](
      NonEmptyChain.one(ArgValue(name, Eval.later(input), default, default.map(DefaultValue.Primitive(_, input)))),
      _(name).asInstanceOf[A]
    )

  def one[A](name: String)(implicit input: => In[A]): NonEmptyArg[A] =
    NonEmptyArg[A](NonEmptyChain.one(ArgValue(name, Eval.later(input), None, None)), _(name).asInstanceOf[A])

  def make[A](name: String, default: Option[DefaultValue[A]])(implicit input: => In[A]): NonEmptyArg[A] =
    NonEmptyArg[A](NonEmptyChain.one(ArgValue(name, Eval.later(input), None, default)), _(name).asInstanceOf[A])

  implicit lazy val applicativeInstanceForArg: Applicative[Arg] = new Applicative[Arg] {
    override def pure[A](x: A): Arg[A] = PureArg(x)

    override def ap[A, B](ff: Arg[A => B])(fa: Arg[A]): Arg[B] =
      (ff, fa) match {
        case (NonEmptyArg(entries1, decode1), NonEmptyArg(entries2, decode2)) =>
          NonEmptyArg(entries1 ++ entries2, m => decode1(m)(decode2(m)))
        case (NonEmptyArg(entries, decode), PureArg(value)) =>
          NonEmptyArg(entries, decode.andThen(_(value)))
        case (PureArg(value), NonEmptyArg(entries, decode)) =>
          NonEmptyArg(entries, decode.andThen(value(_)))
        case (PureArg(f), PureArg(value)) => PureArg(f(value))
      }
  }
}

sealed trait DefaultValue[+A]
object DefaultValue {
  final case class Primitive[A](value: A, in: InLeaf[A]) extends DefaultValue[A] //{
  final case class Arr[A](values: Seq[DefaultValue[A]]) extends DefaultValue[Seq[A]]
  final case class Obj(fields: NonEmptyChain[(String, DefaultValue[_])]) extends DefaultValue[Nothing]
  case object Null extends DefaultValue[Nothing]
}

final case class ArgValue[A](
    name: String,
    input: Eval[In[A]],
    default: Option[A] = None,
    defaultValue: Option[DefaultValue[A]] = None
)

final case class NonEmptyArg[A](
    nec: NonEmptyChain[ArgValue[_]],
    decode: Map[String, _] => A
) extends Arg[A] {
  def entries = nec.toChain
}
object NonEmptyArg {
  implicit lazy val applicativeInstanceForNonEmptyArg: Apply[NonEmptyArg] = new Apply[NonEmptyArg] {
    override def map[A, B](fa: NonEmptyArg[A])(f: A => B): NonEmptyArg[B] =
      NonEmptyArg(fa.nec, fa.decode.andThen(f))

    override def ap[A, B](ff: NonEmptyArg[A => B])(fa: NonEmptyArg[A]): NonEmptyArg[B] =
      NonEmptyArg(ff.nec ++ fa.nec, m => ff.decode(m) apply fa.decode(m))
  }
}

final case class PureArg[A](value: A) extends Arg[A] {
  def entries = Chain.empty
  def decode = _ => value
}
