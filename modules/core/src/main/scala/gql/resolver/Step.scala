package gql.resolver

import gql._
import cats.data._

sealed trait Step[F[_], -I, +O]

object Step {
  object Alg {
    final case class Lift[F[_], I, O](f: I => O) extends AnyRef with Step[F, I, O]

    final case class Effect[F[_], I, O](f: I => F[O]) extends AnyRef with Step[F, I, O]

    final case class Raise[F[_], I, O](f: I => Ior[String, O]) extends AnyRef with Step[F, I, O]

    final case class Argument[F[_], I, A](arg: Arg[A]) extends Step[F, I, A]

    final case class Compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]) extends Step[F, I, O]

    final case class Stream[F[_], I, O](f: I => fs2.Stream[F, O]) extends AnyRef with Step[F, I, O]

    final case class Skip[F[_], I, O](compute: Step[F, I, O]) extends Step[F, Either[I, O], O]

    final case class GetMeta[F[_], I]() extends Step[F, I, Meta]

    final case class First[F[_], A, B, C](step: Step[F, A, B]) extends Step[F, (A, C), (B, C)]

    final case class Batch[F[_], K, V](id: Int) extends Step[F, Set[K], Map[K, V]]
  }

  def lift[F[_], I, O](f: I => O): Step[F, I, O] =
    Alg.Lift(f)

  def effect[F[_], I, O](f: I => F[O]): Step[F, I, O] =
    Alg.Effect(f)

  def raise[F[_], I, O](f: I => Ior[String, O]): Step[F, I, O] =
    Alg.Raise(f)

  def argument[F[_], A](arg: Arg[A]): Step[F, Any, A] =
    Alg.Argument(arg)

  def compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]): Step[F, I, O] =
    Alg.Compose(left, right)

  def stream[F[_], I, O](f: I => fs2.Stream[F, O]): Step[F, I, O] =
    Alg.Stream(f)

  def skip[F[_], I, O](compute: Step[F, I, O]): Step[F, Either[I, O], O] =
    Alg.Skip(compute)

  def getMeta[F[_]]: Step[F, Any, Meta] =
    Alg.GetMeta()

  def first[F[_], A, B, C](step: Step[F, A, B]): Step[F, (A, C), (B, C)] =
    Alg.First(step)

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Step[F, Set[K], Map[K, V]]] =
    State { s =>
      val id = s.nextId
      (s.copy(nextId = id + 1, batchFunctions = s.batchFunctions + (id -> SchemaState.BatchFunction(f))), Alg.Batch(id))
    }

  import cats.arrow._
  implicit def arrowForStep[F[_]] = new Arrow[Step[F, *, *]] {
    override def compose[A, B, C](f: Step[F, B, C], g: Step[F, A, B]): Step[F, A, C] = compose(f, g)

    override def first[A, B, C](fa: Step[F, A, B]): Step[F, (A, C), (B, C)] = first(fa)

    override def lift[A, B](f: A => B): Step[F, A, B] = lift(f)
  }
}
