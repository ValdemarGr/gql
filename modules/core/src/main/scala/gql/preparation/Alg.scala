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
package gql.preparation

import gql._
import cats.implicits._
import cats._
import cats.arrow.FunctionK
import cats.data._
import org.typelevel.scalaccompat.annotation._

sealed trait Alg[+C, +A] {
  def run[C2 >: C]: EitherNec[PositionalError[C2], A] = Alg.run(this)
}
object Alg {
  case object NextId extends Alg[Nothing, Int]

  final case class UseVariable(name: String) extends Alg[Nothing, Unit]
  final case class UsedVariables() extends Alg[Nothing, Set[String]]

  case object CycleAsk extends Alg[Nothing, Set[String]]
  final case class CycleOver[C, A](name: String, fa: Alg[C, A]) extends Alg[C, A]

  case object CursorAsk extends Alg[Nothing, Cursor]
  final case class CursorOver[C, A](cursor: Cursor, fa: Alg[C, A]) extends Alg[C, A]

  final case class RaiseError[C](pe: NonEmptyChain[PositionalError[C]]) extends Alg[C, Nothing]

  final case class Pure[A](a: A) extends Alg[Nothing, A]
  final case class FlatMap[C, A, B](
      fa: Alg[C, A],
      f: A => Alg[C, B]
  ) extends Alg[C, B]
  final case class ParAp[C, A, B](
      fa: Alg[C, A],
      fab: Alg[C, A => B]
  ) extends Alg[C, B]

  final case class Attempt[C, A](
      fa: Alg[C, A]
  ) extends Alg[C, EitherNec[PositionalError[C], A]]

  implicit def monadErrorForPreparationAlg[C]: MonadError[Alg[C, *], NonEmptyChain[PositionalError[C]]] =
    new MonadError[Alg[C, *], NonEmptyChain[PositionalError[C]]] {
      override def pure[A](x: A): Alg[C, A] = Ops[C].pure(x)

      override def raiseError[A](e: NonEmptyChain[PositionalError[C]]): Alg[C, A] =
        Alg.RaiseError(e)

      override def handleErrorWith[A](fa: Alg[C, A])(
          f: NonEmptyChain[PositionalError[C]] => Alg[C, A]
      ): Alg[C, A] =
        Ops[C].flatMap(Ops[C].attempt(fa)) {
          case Left(pe) => f(pe)
          case Right(a) => Ops[C].pure(a)
        }

      override def flatMap[A, B](fa: Alg[C, A])(f: A => Alg[C, B]): Alg[C, B] =
        Ops[C].flatMap(fa)(f)

      override def tailRecM[A, B](a: A)(f: A => Alg[C, Either[A, B]]): Alg[C, B] =
        Ops[C].flatMap(f(a)) {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => Ops[C].pure(b)
        }
    }

  implicit def parallelForPreparationAlg[C]: Parallel[Alg[C, *]] =
    new Parallel[Alg[C, *]] {
      type F[A] = Alg[C, A]

      override def sequential: F ~> F = FunctionK.id[F]

      override def parallel: F ~> F = FunctionK.id[F]

      override def applicative: Applicative[F] =
        new Applicative[F] {
          override def pure[A](x: A): F[A] = Ops[C].pure(x)

          override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = Ops[C].parAp(fa)(ff)
        }

      override def monad: Monad[Alg[C, *]] = monadErrorForPreparationAlg[C]
    }

  def run[C, A](fa: Alg[C, A]): EitherNec[PositionalError[C], A] = {
    final case class State(
        nextId: Int,
        usedVariables: Set[String],
        cycleSet: Set[String],
        cursor: Cursor
    )
    sealed trait Outcome[+B] {
      def modifyState(f: State => State): Outcome[B]
    }
    object Outcome {
      final case class Result[B](value: B, state: State) extends Outcome[B] {
        def modifyState(f: State => State): Outcome[B] = Result(value, f(state))
      }
      final case class Errors(pe: NonEmptyChain[PositionalError[C]]) extends Outcome[Nothing] {
        def modifyState(f: State => State): Outcome[Nothing] = this
      }
    }
    @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
    def go[B](
        fa: Alg[C, B],
        state: State
    ): Eval[Outcome[B]] = Eval.defer {
      fa match {
        case NextId =>
          val s = state.copy(nextId = state.nextId + 1)
          Eval.now(Outcome.Result(s.nextId, s))
        case Pure(a) => Eval.now(Outcome.Result(a, state))
        case bind: FlatMap[C, a, B] =>
          go[a](bind.fa, state).flatMap {
            case Outcome.Errors(pes)      => Eval.now(Outcome.Errors(pes))
            case Outcome.Result(a, state) => go(bind.f(a), state)
          }
        case parAp: ParAp[C, a, B] =>
          go(parAp.fa, state).flatMap {
            case Outcome.Errors(pes1) =>
              go(parAp.fab, state).flatMap {
                case Outcome.Errors(pes2) => Eval.now(Outcome.Errors(pes1 ++ pes2))
                case Outcome.Result(_, _) => Eval.now(Outcome.Errors(pes1))
              }
            case Outcome.Result(a, state) =>
              go(parAp.fab, state).flatMap {
                case Outcome.Errors(pes2)     => Eval.now(Outcome.Errors(pes2))
                case Outcome.Result(f, state) => Eval.now(Outcome.Result(f(a), state))
              }
          }
        case UseVariable(name) =>
          Eval.now(Outcome.Result((), state.copy(usedVariables = state.usedVariables + name)))
        case UsedVariables() =>
          Eval.now(Outcome.Result(state.usedVariables, state))
        case CycleAsk =>
          Eval.now(Outcome.Result(state.cycleSet, state))
        case CycleOver(name, fa) =>
          go(fa, state.copy(cycleSet = state.cycleSet + name))
            .map(_.modifyState(s => s.copy(cycleSet = s.cycleSet - name)))
        case CursorAsk =>
          Eval.now(Outcome.Result(state.cursor, state))
        case CursorOver(cursor, fa) =>
          go(fa, state.copy(cursor = cursor))
            .map(_.modifyState(s => s.copy(cursor = s.cursor)))
        case re: RaiseError[C] =>
          Eval.now(Outcome.Errors(re.pe))
        case alg: Attempt[C, a] =>
          go(alg.fa, state).flatMap {
            case Outcome.Errors(pes)      => Eval.now(Outcome.Result(Left(pes), state))
            case Outcome.Result(a, state) => Eval.now(Outcome.Result(Right(a), state))
          }
      }
    }

    go(fa, State(0, Set.empty, Set.empty, Cursor.empty)).value match {
      case Outcome.Errors(pes)  => Left(pes)
      case Outcome.Result(a, _) => Right(a)
    }
  }

  trait Ops[C] {
    def nextId: Alg[C, Int] = Alg.NextId

    def useVariable(name: String): Alg[C, Unit] =
      Alg.UseVariable(name)

    def usedVariables: Alg[C, Set[String]] =
      Alg.UsedVariables()

    def cycleAsk: Alg[C, Set[String]] = Alg.CycleAsk

    def cycleOver[A](name: String, fa: Alg[C, A]): Alg[C, A] =
      Alg.CycleOver(name, fa)

    def cursorAsk: Alg[C, Cursor] = Alg.CursorAsk

    def cursorOver[A](cursor: Cursor, fa: Alg[C, A]): Alg[C, A] =
      Alg.CursorOver(cursor, fa)

    def raiseError(pe: PositionalError[C]): Alg[C, Nothing] =
      Alg.RaiseError(NonEmptyChain.one(pe))

    def raise[A](message: String, carets: List[C]): Alg[C, A] =
      cursorAsk.flatMap(c => raiseError(PositionalError(c, carets, message)))

    def raiseEither[A](e: Either[String, A], carets: List[C]): Alg[C, A] =
      e match {
        case Left(value)  => raise(value, carets)
        case Right(value) => pure(value)
      }

    def raiseOpt[A](oa: Option[A], message: String, carets: List[C]): Alg[C, A] =
      raiseEither(oa.toRight(message), carets)

    def modifyError[A](f: PositionalError[C] => PositionalError[C])(fa: Alg[C, A]): Alg[C, A] =
      attempt(fa).flatMap {
        case Right(a)  => pure(a)
        case Left(pes) => Alg.RaiseError(pes.map(f))
      }

    def appendMessage[A](message: String)(fa: Alg[C, A]): Alg[C, A] =
      modifyError[A](d => d.copy(message = d.message + "\n" + message))(fa)

    def pure[A](a: A): Alg[Nothing, A] = Alg.Pure(a)

    def flatMap[A, B](fa: Alg[C, A])(f: A => Alg[C, B]): Alg[C, B] =
      Alg.FlatMap(fa, f)

    def parAp[A, B](fa: Alg[C, A])(fab: Alg[C, A => B]): Alg[C, B] =
      Alg.ParAp(fa, fab)

    def attempt[A](fa: Alg[C, A]): Alg[C, EitherNec[PositionalError[C], A]] =
      Alg.Attempt(fa)

    def unit: Alg[C, Unit] = pure(())

    def ambientEdge[A](edge: GraphArc)(fa: Alg[C, A]): Alg[C, A] =
      cursorAsk.flatMap { cursor =>
        cursorOver(cursor.add(edge), fa)
      }

    def ambientField[A](name: String)(fa: Alg[C, A]): Alg[C, A] =
      ambientEdge(GraphArc.Field(name))(fa)

    def ambientIndex[A](index: Int)(fa: Alg[C, A]): Alg[C, A] =
      ambientEdge(GraphArc.Index(index))(fa)
  }
  object Ops {
    def apply[C] = new Ops[C] {}
  }
}
