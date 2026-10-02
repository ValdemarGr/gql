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
import cats.effect.kernel.Unique

sealed trait Alg[C, +A] {
  def run: EitherNec[PositionalError[C], Alg.Variables[C] => EitherNec[PositionalError[C], A]] = Alg.run(this)
}
object Alg {
  type Variables[C] = VariableMap[C]

  final case class NextId[C]() extends Alg[C, Unique.Token]

  final case class UseVariables[C](names: NonEmptyChain[String]) extends Alg[C, Unit]
  final case class UsedVariables[C]() extends Alg[C, Set[String]]

  final case class CycleAsk[C]() extends Alg[C, Set[String]]
  final case class CycleOver[C, A](name: String, fa: Alg[C, A]) extends Alg[C, A]

  final case class CursorAsk[C]() extends Alg[C, Cursor]
  final case class CursorOver[C, A](cursor: Cursor, fa: Alg[C, A]) extends Alg[C, A]

  final case class RaiseError[C](pe: NonEmptyChain[PositionalError[C]]) extends Alg[C, Nothing]

  final case class Pure[C, A](a: A) extends Alg[C, A]
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

  final case class NeedVars[C, A](f: Variables[C] => Eval[Alg[C, A]]) extends Alg[C, A]

  sealed trait Result[C, +A]
  object Result {
    // Usage written in the current scope, excluding inherited read history.
    final case class Success[C, A](value: A, usedVariables: Chain[String]) extends Result[C, A]
    final case class Failure[C](errors: NonEmptyChain[PositionalError[C]]) extends Result[C, Nothing]
    final case class NeedVars[C, A](f: Variables[C] => Eval[Result[C, A]]) extends Result[C, A]
  }
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

  def useVariables[C](names: NonEmptyChain[String]): Alg[C, Unit] = Alg.UseVariables(names)
  def pure[C, A](a: A): Alg[C, A] = Alg.Pure(a)
  def raiseErrors[C](errors: NonEmptyChain[PositionalError[C]]): Alg[C, Nothing] = Alg.RaiseError(errors)

  final case class State(
      readVariables: Chain[String],
      writtenVariables: Chain[String],
      cycleSet: Set[String],
      cursor: Cursor
  )
  def run[C, A0](fa: Alg[C, A0]): EitherNec[PositionalError[C], Variables[C] => EitherNec[PositionalError[C], A0]] = {
    def liftResult[B](result: Result[C, B], vars: Option[Variables[C]]): Eval[Alg[C, B]] = result match {
      case Result.Success(value, usedVariables) =>
        Eval.now(NonEmptyChain.fromChain(usedVariables).traverse_(useVariables[C](_)) *> pure(value))
      case Result.Failure(errors) => Eval.now(Alg.RaiseError(errors))
      case Result.NeedVars(f) =>
        vars match {
          case Some(v) => Eval.defer(f(v)).flatMap(liftResult(_, vars))
          case None    => Eval.now(Alg.NeedVars[C, B](v => Eval.defer(f(v)).flatMap(liftResult(_, Some(v)))))
        }
    }

    def go[A](
        fa: Alg[C, A],
        state: State,
        vars: Option[Variables[C]]
    ): Eval[Result[C, A]] = Eval.defer {
      def needVars(f: Variables[C] => Eval[Alg[C, A]], scope: State = state): Eval[Result[C, A]] =
        vars match {
          case Some(v) => Eval.defer(f(v)).flatMap(go(_, scope, vars))
          case None    => Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(go(_, scope, Some(v)))))
        }

      lazy val childState = state.copy(
        readVariables = state.readVariables ++ state.writtenVariables,
        writtenVariables = Chain.empty
      )

      fa match {
        case NextId() => Eval.now(Result.Success(new Unique.Token, state.writtenVariables))
        case Pure(a)  => Eval.now(Result.Success(a, state.writtenVariables))
        case UseVariables(names) =>
          Eval.now(Result.Success((), state.writtenVariables ++ names.toChain))
        case UsedVariables() =>
          Eval.now(Result.Success((state.readVariables ++ state.writtenVariables).iterator.toSet, state.writtenVariables))
        case CycleAsk()             => Eval.now(Result.Success(state.cycleSet, state.writtenVariables))
        case CycleOver(name, fa)    => go(fa, state.copy(cycleSet = state.cycleSet + name), vars)
        case CursorAsk()            => Eval.now(Result.Success(state.cursor, state.writtenVariables))
        case CursorOver(cursor, fa) => go(fa, state.copy(cursor = cursor), vars)
        case error: RaiseError[C]   => Eval.now(Result.Failure(error.pe))
        case attempt: Attempt[C, a] =>
          def attemptResult(result: Result[C, a]): Result[C, EitherNec[PositionalError[C], a]] = result match {
            case Result.Success(value, writtenVariables) => Result.Success(Right(value), writtenVariables)
            case Result.Failure(errors)                  => Result.Success(Left(errors), state.writtenVariables)
            case Result.NeedVars(f)                      => Result.NeedVars(v => Eval.defer(f(v)).map(attemptResult))
          }
          go(attempt.fa, state, vars).map(attemptResult)
        case request: NeedVars[C, A] => needVars(request.f)
        case parAp: ParAp[C, a, A] =>
          go(parAp.fa, childState, vars).flatMap {
            case Result.Failure(errs1) =>
              go(parAp.fab, childState, vars).flatMap {
                case Result.Failure(errs2) => Eval.now(Result.Failure(errs1 ++ errs2))
                case Result.Success(_, _)  => Eval.now(Result.Failure(errs1))
                case Result.NeedVars(f) =>
                  needVars(v =>
                    Eval
                      .defer(f(v))
                      .flatMap(liftResult(_, Some(v)))
                      .flatMap(ab => go(ParAp[C, a, A](Alg.RaiseError(errs1), ab), childState, Some(v)))
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
                      Eval
                        .defer(f(v))
                        .flatMap(liftResult(_, Some(v)))
                        .flatMap(ab => go(ParAp[C, a, A](Alg.Pure(a), ab), childState, Some(v)))
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
                .flatMap(ab => needVars(v => Eval.defer(f(v)).flatMap(liftResult(_, Some(v))).map(a => ParAp[C, a, A](a, ab))))
          }
        case bind: FlatMap[C, a, A] =>
          def continueBind(result: Result[C, a], currentVars: Option[Variables[C]]): Eval[Result[C, A]] = result match {
            case Result.Failure(errs) => Eval.now(Result.Failure(errs))
            case Result.Success(a, writtenVariables) =>
              go(bind.f(a), state.copy(writtenVariables = writtenVariables), currentVars)
            case Result.NeedVars(f) =>
              currentVars match {
                case Some(v) => Eval.defer(f(v)).flatMap(continueBind(_, currentVars))
                case None    => Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(continueBind(_, Some(v)))))
              }
          }
          go(bind.fa, state, vars).flatMap(continueBind(_, vars))
      }
    }

    def complete(result: Result[C, A0], vars: Variables[C]): Eval[EitherNec[PositionalError[C], A0]] = result match {
      case Result.Success(value, _) => Eval.now(Right(value))
      case Result.Failure(errors)   => Eval.now(Left(errors))
      case Result.NeedVars(f)       => Eval.defer(f(vars)).flatMap(complete(_, vars))
    }

    go(fa, State(Chain.empty, Chain.empty, Set.empty, Cursor.empty), None).value match {
      case Result.Success(value, _) => Right(_ => Right(value))
      case Result.Failure(errors)   => Left(errors)
      case Result.NeedVars(f)       => Right(vars => Eval.defer(f(vars)).flatMap(complete(_, vars)).value)
    }
  }

  trait Ops[C] {
    def nextId: Alg[C, Unique.Token] = Alg.NextId()

    def useVariable(name: String): Alg[C, Unit] =
      Alg.UseVariables(NonEmptyChain.one(name))

    def useVariables(names: NonEmptyChain[String]): Alg[C, Unit] = Alg.UseVariables(names)

    def getVariables: Alg[C, Variables[C]] = Alg.NeedVars[C, Variables[C]](v => Eval.now(Alg.Pure(v)))

    def usedVariables: Alg[C, Set[String]] =
      Alg.UsedVariables()

    def cycleAsk: Alg[C, Set[String]] = Alg.CycleAsk()

    def cycleOver[A](name: String, fa: Alg[C, A]): Alg[C, A] =
      Alg.CycleOver(name, fa)

    def cursorAsk: Alg[C, Cursor] = Alg.CursorAsk()

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

    def pure[A](a: A): Alg[C, A] = Alg.Pure(a)

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
