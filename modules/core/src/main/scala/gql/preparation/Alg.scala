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
import cats.effect.kernel.Unique

sealed trait Alg2[+C, +A]
object Alg2 {
  type Variables = Map[String, Unit]

  case object NextId extends Alg2[Nothing, Unique.Token]

  final case class UseVariables(names: NonEmptyChain[String]) extends Alg2[Nothing, Unit]
  final case class UsedVariables() extends Alg2[Nothing, Set[String]]

  case object CycleAsk extends Alg2[Nothing, Set[String]]
  final case class CycleOver[C, A](name: String, fa: Alg2[C, A]) extends Alg2[C, A]

  case object CursorAsk extends Alg2[Nothing, Cursor]
  final case class CursorOver[C, A](cursor: Cursor, fa: Alg2[C, A]) extends Alg2[C, A]

  final case class RaiseError[C](pe: NonEmptyChain[PositionalError[C]]) extends Alg2[C, Nothing]

  final case class Pure[A](a: A) extends Alg2[Nothing, A]
  final case class FlatMap[C, A, B](
      fa: Alg2[C, A],
      f: A => Alg2[C, B]
  ) extends Alg2[C, B]
  final case class ParAp[C, A, B](
      fa: Alg2[C, A],
      fab: Alg2[C, A => B]
  ) extends Alg2[C, B]

  final case class Attempt[C, A](
      fa: Alg2[C, A]
  ) extends Alg2[C, EitherNec[PositionalError[C], A]]

  final case class NeedVars[C, A](f: Variables => Eval[Alg2[C, A]]) extends Alg2[C, A]

  sealed trait Result[C, +A]
  object Result {
    // Usage written in the current scope, excluding inherited read history.
    final case class Success[C, A](value: A, usedVariables: Chain[String]) extends Result[C, A]
    final case class Failure[C](errors: NonEmptyChain[PositionalError[C]]) extends Result[C, Nothing]
    final case class NeedVars[C, A](f: Variables => Eval[Result[C, A]]) extends Result[C, A]
  }
  implicit def monad[C]: Monad[Alg2[C, *]] = ???

  def useVariables[C](names: NonEmptyChain[String]): Alg2[C, Unit] = Alg2.UseVariables(names)
  def pure[C, A](a: A): Alg2[C, A] = Alg2.Pure(a)
  def raiseErrors[C](errors: NonEmptyChain[PositionalError[C]]): Alg2[C, Nothing] = Alg2.RaiseError(errors)

  final case class State(
      readVariables: Chain[String],
      writtenVariables: Chain[String],
      cycleSet: Set[String],
      cursor: Cursor
  )
  def go0[C] = {
    def liftResult[B](result: Result[C, B], vars: Option[Variables]): Eval[Alg2[C, B]] = result match {
      case Result.Success(value, usedVariables) =>
        Eval.now(NonEmptyChain.fromChain(usedVariables).traverse_(useVariables[C](_)) *> pure(value))
      case Result.Failure(errors) => Eval.now(Alg2.RaiseError(errors))
      case Result.NeedVars(f) =>
        vars match {
          case Some(v) => f(v).flatMap(liftResult(_, vars))
          case None    => Eval.now(Alg2.NeedVars(v => f(v).flatMap(liftResult(_, Some(v)))))
        }
    }

    def go[A](
        fa: Alg2[C, A],
        state: State,
        vars: Option[Variables]
    ): Eval[Result[C, A]] = Eval.defer {
      def needVars(f: Variables => Eval[Alg2[C, A]], scope: State = state): Eval[Result[C, A]] =
        vars match {
          case Some(v) => f(v).flatMap(go(_, scope, vars))
          case None    => Eval.now(Result.NeedVars(v => f(v).flatMap(go(_, scope, Some(v)))))
        }

      lazy val childState = state.copy(
        readVariables = state.readVariables ++ state.writtenVariables,
        writtenVariables = Chain.empty
      )

      fa match {
        case NextId => Eval.now(Result.Success(new Unique.Token, state.writtenVariables))
        case Pure(a) => Eval.now(Result.Success(a, state.writtenVariables))
        case UseVariables(names) =>
          Eval.now(Result.Success((), state.writtenVariables ++ names.toChain))
        case UsedVariables() =>
          Eval.now(Result.Success((state.readVariables ++ state.writtenVariables).iterator.toSet, state.writtenVariables))
        case CycleAsk => Eval.now(Result.Success(state.cycleSet, state.writtenVariables))
        case CycleOver(name, fa) => go(fa, state.copy(cycleSet = state.cycleSet + name), vars)
        case CursorAsk => Eval.now(Result.Success(state.cursor, state.writtenVariables))
        case CursorOver(cursor, fa) => go(fa, state.copy(cursor = cursor), vars)
        case error: RaiseError[C] => Eval.now(Result.Failure(error.pe))
        case attempt: Attempt[C, a] =>
          def attemptResult(result: Result[C, a]): Result[C, EitherNec[PositionalError[C], a]] = result match {
            case Result.Success(value, writtenVariables) => Result.Success(Right(value), writtenVariables)
            case Result.Failure(errors) => Result.Success(Left(errors), state.writtenVariables)
            case Result.NeedVars(f) => Result.NeedVars(v => f(v).map(attemptResult))
          }
          go(attempt.fa, state, vars).map(attemptResult)
        case NeedVars(f) => needVars(f)
        case parAp: ParAp[C, a, A] =>
          go(parAp.fa, childState, vars).flatMap {
            case Result.Failure(errs1) =>
              go(parAp.fab, childState, vars).flatMap {
                case Result.Failure(errs2) => Eval.now(Result.Failure(errs1 ++ errs2))
                case Result.Success(_, _)  => Eval.now(Result.Failure(errs1))
                case Result.NeedVars(f) =>
                  needVars(v =>
                    f(v)
                      .flatMap(liftResult(_, Some(v)))
                      .flatMap(ab => go(ParAp[C, a, A](Alg2.RaiseError(errs1), ab), childState, Some(v)))
                      .flatMap(liftResult(_, Some(v)))
                  )
              }
            case done @ Result.Success(a, _) =>
              val fa: Eval[Result[C, A]] = go(parAp.fab, childState, vars).flatMap {
                case Result.Failure(errs2) => Eval.now(Result.Failure(errs2))
                case Result.Success(f, usedVars) =>
                  Eval.now(Result.Success(f(a), usedVars))
                case Result.NeedVars(f) =>
                  needVars(
                    v =>
                      f(v)
                        .flatMap(liftResult(_, Some(v)))
                        .flatMap(ab => go(ParAp[C, a, A](Alg2.Pure(a), ab), childState, Some(v)))
                        .flatMap(liftResult(_, Some(v))),
                    childState
                  )
              }
              fa
                .flatMap(liftResult(_, vars))
                .flatMap(r => liftResult(done, vars).map(w => w *> r))
                .flatMap(fa => go(fa, state, vars))
            case Result.NeedVars(f) =>
              go(parAp.fab, childState, vars)
                .flatMap(liftResult(_, vars))
                .flatMap(ab => needVars(v => f(v).flatMap(liftResult(_, Some(v))).map(a => ParAp[C, a, A](a, ab))))
          }
        case bind: FlatMap[C, a, A] =>
          def continueBind(result: Result[C, a], currentVars: Option[Variables]): Eval[Result[C, A]] = result match {
            case Result.Failure(errs) => Eval.now(Result.Failure(errs))
            case Result.Success(a, writtenVariables) =>
              go(bind.f(a), state.copy(writtenVariables = writtenVariables), currentVars)
            case Result.NeedVars(f) =>
              currentVars match {
                case Some(v) => f(v).flatMap(continueBind(_, currentVars))
                case None    => Eval.now(Result.NeedVars(v => f(v).flatMap(continueBind(_, Some(v)))))
              }
          }
          go(bind.fa, state, vars).flatMap(continueBind(_, vars))
      }
    }
  }
}

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
            .map(_.modifyState(s => s.copy(cursor = state.cursor)))
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

    def appendMessage[A](message: => String)(fa: Alg[C, A]): Alg[C, A] =
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

    def defer[A](fa: => Alg[C, A]): Alg[C, A] =
      unit.flatMap(_ => fa)
  }
  object Ops {
    def apply[C] = new Ops[C] {}
  }
}
