/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql.resolver

import gql._
import cats.data._
import cats.implicits._
import cats._

/** A step is a composable task that takes an input and produces an output.
  */
sealed trait Step[+F[_], -I, +O]

object Step {
  object Alg {
    final case class Identity[I]() extends AnyRef with Step[Nothing, I, I] {
      override def toString: String = s"Identity"
    }

    final case class Lift[I, O](f: I => O) extends AnyRef with Step[Nothing, I, O] {
      override def toString: String = s"Lift(...)"
    }

    final case class EmbedEffect[F[_], I]() extends AnyRef with Step[F, F[I], I] {
      override def toString: String = s"EmbedEffect"
    }

    final case class EmbedStream[F[_], I](
        signal: Boolean
    ) extends AnyRef
        with Step[F, fs2.Stream[F, I], I] {
      override def toString: String = s"EmbedStream(signal=$signal)"
    }

    final case class EmbedError[I]() extends AnyRef with Step[Nothing, Ior[String, I], I] {
      override def toString: String = s"EmbedError"
    }

    final case class Argument[I, A](arg: Arg[A]) extends Step[Nothing, I, A] {
      override def toString: String = s"Argument(${arg.entries.map(_.name).mkString_(", ")})"
    }

    final case class Compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]) extends Step[F, I, O] {
      override def toString: String = s"Compose($left, $right)"
    }

    final case class Choose[F[_], A, B, C, D](
        fac: Step[F, A, C],
        fab: Step[F, B, D]
    ) extends Step[F, Either[A, B], Either[C, D]] {
      override def toString: String = s"Choose($fac, $fab)"
    }

    final case class GetMeta[F[_], I]() extends Step[Nothing, I, FieldMeta[F]] {
      override def toString: String = s"GetMeta"
    }

    final case class First[F[_], A, B, C](step: Step[F, A, B]) extends Step[F, (A, C), (B, C)] {
      override def toString: String = s"First($step)"
    }

    final case class Batch[F[_], K, V](id: BatchKey[K, V]) extends Step[F, Set[K], Map[K, V]] {
      override def toString: String = s"Batch($id)"
    }

    final case class InlineBatch[F[_], K, V](run: Set[K] => F[Map[K, V]]) extends Step[F, Set[K], Map[K, V]] {
      override def toString: String = s"InlineBatch"
    }
  }

  def identity[I]: Step[Nothing, I, I] =
    Alg.Identity()

  def lift[F[_], I, O](f: I => O): Step[F, I, O] =
    Alg.Lift(f)

  def embedEffect[F[_], I]: Step[F, F[I], I] =
    Alg.EmbedEffect()

  def embedError[F[_], I]: Step[F, Ior[String, I], I] =
    Alg.EmbedError()

  def embedStreamFull[F[_], I, O](signal: Boolean): Step[F, fs2.Stream[F, I], I] =
    Alg.EmbedStream(signal)

  def embedStream[F[_], I, O]: Step[F, fs2.Stream[F, I], I] =
    embedStreamFull(signal = true)

  def argument[F[_], A](arg: Arg[A]): Step[F, Any, A] =
    Alg.Argument(arg)

  def compose[F[_], I, A, O](left: Step[F, I, A], right: Step[F, A, O]): Step[F, I, O] =
    Alg.Compose(left, right)

  def choose[F[_], A, B, C, D](fac: Step[F, A, C], fab: Step[F, B, D]): Step[F, Either[A, B], Either[C, D]] =
    Alg.Choose(fac, fab)

  def getMeta[F[_]]: Step[F, Any, FieldMeta[F]] =
    Alg.GetMeta()

  def first[F[_], A, B, C](step: Step[F, A, B]): Step[F, (A, C), (B, C)] =
    Alg.First(step)

  final case class BatchKey[K, V](id: Int) extends AnyVal

  def batch[F[_], K, V](f: Set[K] => F[Map[K, V]]): State[gql.SchemaState[F], Step[F, Set[K], Map[K, V]]] =
    State { s =>
      val id = s.nextId
      val k = BatchKey[K, V](id)
      (s.copy(nextId = id + 1, batchFunctions = s.batchFunctions + (k -> SchemaState.BatchFunction(f))), Alg.Batch(k))
    }

  def inlineBatch[F[_], K, V](f: Set[K] => F[Map[K, V]]): Step[F, Set[K], Map[K, V]] =
    Alg.InlineBatch(f)

  def optimize[F[_], I, O](step: Step[F, I, O]): Step[F, I, O] = {
    sealed trait CurrentContinuation[A, B] {
      def apply[A0](s: Step[F, A0, A]): Step[F, A0, B]
      def push[C](f: Step[F, C, A]): CurrentContinuation[C, B] =
        CurrentContinuation.CC(f, this)
    }
    object CurrentContinuation {
      final case class Identity[A]() extends CurrentContinuation[A, A] {
        def apply[A0](s: Step[F, A0, A]): Step[F, A0, A] = s
      }
      final case class CC[A, B, C](
          l: Step[F, A, B],
          r: CurrentContinuation[B, C]
      ) extends CurrentContinuation[A, C] {
        def apply[A0](s: Step[F, A0, A]): Step[F, A0, C] =
          r(Step.Alg.Compose(s, l))
      }
    }

    def optimizeCompose[A1, B1](s: Step[F, A1, B1]): Step[F, A1, B1] = s match {
      case alg: Alg.Compose[F, A1, a, B1] =>
        type A = a
        val e = (alg.left, alg.right) match {
          case (_: Alg.Identity[i], r)                => r.some
          case (l, _: Alg.Identity[o])                => l.some
          case (l: Alg.Lift[i, A], r: Alg.Lift[A, o]) => Alg.Lift(r.f.compose(l.f)).some
          case (l: Alg.Lift[i, A], rc: Alg.Compose[F, A, a2, o]) =>
            rc.left match {
              case rl: Alg.Lift[A, o] => Alg.Compose(Alg.Lift(rl.f.compose(l.f)), rc.right).some
              case _                  => None
            }
          case _ => None
        }
        e.map(_.asInstanceOf[Step[F, A1, B1]]).getOrElse(alg)
      case _ => s
    }

    def aux[A0, B0, C0](
        step: Step[F, A0, B0],
        cc: CurrentContinuation[B0, C0]
    ): Eval[Step[F, A0, C0]] = Eval.defer[Step[F, A0, C0]] {
      step match {
        case alg: Alg.Compose[F, i, a, o] =>
          aux(alg.right, cc).flatMap { r =>
            aux(alg.left, CurrentContinuation.CC(r, CurrentContinuation.Identity[C0]())).map(optimizeCompose)
          }
        case alg: Alg.Choose[F, a, b, c, d] =>
          (
            aux(alg.fac, CurrentContinuation.Identity[c]()),
            aux(alg.fab, CurrentContinuation.Identity[d]())
          ).mapN(Alg.Choose[F, a, b, c, d](_, _)).map(x => optimizeCompose(cc(x)))

        case alg: Alg.First[F, a, b, c] =>
          type A = a
          type B = b
          aux(alg.step, CurrentContinuation.Identity[b]())
            .map { x =>
              val e = x match {
                case _: Alg.Identity[?]  => Alg.Identity[(A, c)]().some
                case alg: Alg.Lift[A, B] => Alg.Lift[(A, c), (B, c)] { case (a, c) => (alg.f(a), c) }.some
                case _                   => None
              }
              e.map(_.asInstanceOf[Step[F, (A, c), (B, c)]]).getOrElse(Alg.First[F, a, b, c](x))
            }
            .map(x => optimizeCompose(cc(x)))
        case _ => Eval.now(optimizeCompose(cc(step)))
      }
    }
    aux(step, CurrentContinuation.Identity[O]()).value
  }

  import cats.arrow._
  implicit def arrowChoiceForStep[F[_]]: ArrowChoice[Step[F, *, *]] = new ArrowChoice[Step[F, *, *]] {
    override def choose[A, B, C, D](f: Step[F, A, C])(g: Step[F, B, D]): Step[F, Either[A, B], Either[C, D]] =
      Step.choose(f, g)

    override def compose[A, B, C](f: Step[F, B, C], g: Step[F, A, B]): Step[F, A, C] = Step.compose(g, f)

    override def first[A, B, C](fa: Step[F, A, B]): Step[F, (A, C), (B, C)] = Step.first(fa)

    override def lift[A, B](f: A => B): Step[F, A, B] = Step.lift(f)

    // optimization
    override def id[A]: Step[F, A, A] = Step.identity
  }
}
