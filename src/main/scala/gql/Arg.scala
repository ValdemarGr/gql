package gql

import cats._

final case class ArgParam[A](
    name: String,
    input: Input[A],
    default: Option[A] = None
)

final case class Arg[A](
    entries: Vector[ArgParam[_]],
    decode: List[_] => (List[_], A)
)

object Arg {
  def initial[A](entry: ArgParam[A]): Arg[A] =
    Arg(Vector(entry), { s => (s.tail, s.head.asInstanceOf[A]) })

  implicit lazy val applicativeForArgs = new Applicative[Arg] {
    override def pure[A](a: A): Arg[A] =
      Arg(Vector.empty, (_, a))
    // override def map[A, B](fa: Arg[A])(f: A => B): Arg[B] =
    //   fa.copy(decode = fa.decode andThen { case (s, a) => (s, f(a)) })

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
