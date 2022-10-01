package gql

import cats.data._
import cats._
import cats.implicits._
import gql.ast._

final case class ArgParam[A](
    name: String,
    input: In[A],
    default: Option[A] = None
)

// TODO Look into free applicatives, I think they might do the same
final case class Arg[A](
    entries: Vector[ArgParam[_]],
    decode: List[_] => (List[_], A)
) {
  def apply[F[_], I, O](f: Field[F, (I, A), O, Unit]): Field[F, I, O, A] =
    Field(
      f.args *> this,
      f.resolve.contramap[(I, A)] { case (i, ag) => ((i, ag), ()) },
      f.output
    )
}

trait Arg2[A] {
  def entries: Chain[ArgValue[_]]

  def decode: Map[String, _] => A
}

object Arg2 {
  def one[A](name: String)(implicit input: => In[A]): NonEmptyArg[A] =
    NonEmptyArg[A](NonEmptyChain.one(ArgValue(name, Eval.later(input))), _(name).asInstanceOf[A])

  implicit lazy val applicativeInstanceForArg: Apply[Arg2] = new Applicative[Arg2] {
    override def pure[A](x: A): Arg2[A] = PureArg(x)

    override def ap[A, B](ff: Arg2[A => B])(fa: Arg2[A]): Arg2[B] =
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

final case class ArgValue[A](
    name: String,
    input: Eval[In[A]],
    default: Option[A] = None
)

final case class NonEmptyArg[A](
    nec: NonEmptyChain[ArgValue[_]],
    decode: Map[String, _] => A
) extends Arg2[A] {
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

final case class PureArg[A](value: A) extends Arg2[A] {
  def entries = Chain.empty
  def decode = _ => value
}

object Arg {
  def initial[A](entry: ArgParam[A]): Arg[A] =
    Arg(Vector(entry), { s => (s.tail, s.head.asInstanceOf[A]) })

  implicit lazy val applicativeForArgs: Applicative[Arg] = new Applicative[Arg] {
    override def pure[A](a: A): Arg[A] =
      Arg(Vector.empty, (_, a))

    override def ap[A, B](ff: Arg[A => B])(fa: Arg[A]): Arg[B] =
      Arg(
        ff.entries ++ fa.entries,
        { s1 =>
          val (s2, f) = ff.decode(s1)
          val (s3, a) = fa.decode(s2)
          (s3, f(a))
        }
      )
  }
}
