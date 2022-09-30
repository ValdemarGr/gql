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

object Arg {
  final case class Value[A](
      name: String,
      input: In[A],
      default: Option[A] = None
  )

  trait Arg2[A]
  final case class NonEmpty[A](
      entries: NonEmptyChain[Value[_]],
      decode: Map[String, _] => A
  ) extends Arg2[A]
  final case class Pure[A](value: A) extends Arg2[A]

  implicit lazy val applicativeInstanceForArg: Apply[Arg2] = new Apply[Arg2] {
    override def map[A, B](fa: Arg2[A])(f: A => B): Arg2[B] =
      fa match {
        case NonEmpty(entries, decode) => NonEmpty(entries, decode.andThen(f))
        case Pure(value)               => Pure(f(value))
      }

    override def ap[A, B](ff: Arg2[A => B])(fa: Arg2[A]): Arg2[B] =
      (ff, fa) match {
        case (NonEmpty(entries1, decode1), NonEmpty(entries2, decode2)) =>
          NonEmpty(entries1 ++ entries2, m => decode1(m)(decode2(m)))
        case (NonEmpty(entries, decode), Pure(value)) =>
          NonEmpty(entries, decode.andThen(_(value)))
        case (Pure(value), NonEmpty(entries, decode)) =>
          NonEmpty(entries, decode.andThen(value(_)))
        case (Pure(f), Pure(value)) => Pure(f(value))
      }
  }

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
