package gql.resolver

import gql._
import cats.data._
import cats._

sealed trait Step[F[_], -I, O] {}

object Step {
  object Alg {
    final case class Pure[F[_], I, O](f: I => O) extends AnyRef with Step[F, I, O]

    final case class Effect[F[_], I, O](f: I => F[O]) extends AnyRef with Step[F, I, O]

    final case class Fallible[F[_], I, O](f: I => F[Ior[String, O]]) extends AnyRef with Step[F, I, O]

    final case class Argument[F[_], A](arg: Arg[A]) extends Step[F, Any, A]

    final case class Compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]) extends Step[F, I, O]

    final case class Stream[F[_], I, O](f: I => fs2.Stream[F, IorNec[String, O]]) extends AnyRef with Step[F, I, O]

    final case class Skip[F[_], I, I2, O](check: I => F[Either[I2, O]], step: Step[F, I2, O]) extends Step[F, I, O]

    final case class GetMeta[F[_]]() extends Step[F, Any, Meta]

    final case class First[F[_], A, B, C](step: Step[F, A, B]) extends Step[F, (A, C), (B, C)]

    final case class MapK[F[_], G[_], -I, O](step: Step[F, I, O], fk: F ~> G) extends Step[G, I, O]
  }

  def pure[F[_], I, O](f: I => O): Step[F, I, O] =
    Alg.Pure(f)

  def effect[F[_], I, O](f: I => F[O]): Step[F, I, O] =
    Alg.Effect(f)

  def fallible[F[_], I, O](f: I => F[Ior[String, O]]): Step[F, I, O] =
    Alg.Fallible(f)

  def argument[F[_], A](arg: Arg[A]): Step[F, Any, A] =
    Alg.Argument(arg)

  def compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]): Step[F, I, O] =
    Alg.Compose(left, right)

  def stream[F[_], I, O](f: I => fs2.Stream[F, IorNec[String, O]]): Step[F, I, O] =
    Alg.Stream(f)

  def skip[F[_], I, I2, O](check: I => F[Either[I2, O]], step: Step[F, I2, O]): Step[F, I, O] =
    Alg.Skip(check, step)

  def getMeta[F[_]]: Step[F, Any, Meta] =
    Alg.GetMeta()

  def first[F[_], A, B, C](step: Step[F, A, B]): Step[F, (A, C), (B, C)] =
    Alg.First(step)

  def mapK[F[_], G[_], I, O](step: Step[F, I, O], fk: F ~> G): Step[G, I, O] =
    Alg.MapK[F, G, I, O](step, fk)
}

// final class Step[F[_], -I, O](val resolver: Resolver[F, I, O]) extends AnyRef {}

// object Step {
//   import cats.arrow._
//   implicit def arrowForStep[F[_]: Functor] = new Arrow[Step[F, *, *]] {
//     val A = Arrow[Resolver[F, *, *]]

//     override def compose[A, B, C](f: Step[F, B, C], g: Step[F, A, B]): Step[F, A, C] =
//       new Step(A.compose(f.resolver, g.resolver))

//     override def first[A, B, C](fa: Step[F, A, B]): Step[F, (A, C), (B, C)] =
//       new Step(A.first(fa.resolver))

//     override def lift[A, B](f: A => B): Step[F, A, B] =
//       new Step(A.lift(f))
//   }
// }
