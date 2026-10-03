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

  final case class Defer[C, A](fa: Alg[C, A]) extends Alg[C, Unit]

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
    final case class Done[C, A](outcome: EitherNec[PositionalError[C], A], usedVariables: Chain[String], deferred: Chain[Alg[C, Unit]])
        extends Result[C, A]
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

  final case class State[C](
      readVariables: Chain[String],
      writtenVariables: Chain[String],
      deferred: Chain[Alg[C, Unit]],
      cycleSet: Set[String],
      cursor: Cursor
  )
  def run[C, A0](fa: Alg[C, A0]): EitherNec[PositionalError[C], Variables[C] => EitherNec[PositionalError[C], A0]] = {
    def combineResults[A, B, D](left: Result[C, A], right: Result[C, B], state: State[C])(f: (A, B) => D): Result[C, D] =
      (left, right) match {
        case (Result.NeedVars(left), Result.NeedVars(right)) =>
          Result.NeedVars(v => (Eval.defer(left(v)), Eval.defer(right(v))).mapN((left, right) => combineResults(left, right, state)(f)))
        case (Result.NeedVars(left), right) =>
          Result.NeedVars(v => Eval.defer(left(v)).map(combineResults(_, right, state)(f)))
        case (left, Result.NeedVars(right)) =>
          Result.NeedVars(v => Eval.defer(right(v)).map(combineResults(left, _, state)(f)))
        case (Result.Done(left, usedVars1, deferred1), Result.Done(right, usedVars2, deferred2)) =>
          Result.Done(
            (left, right).parMapN(f),
            state.writtenVariables ++ usedVars1 ++ usedVars2,
            state.deferred ++ deferred1 ++ deferred2
          )
      }

    def go[A](
        fa: Alg[C, A],
        state: State[C],
        vars: Option[Variables[C]]
    ): Eval[Result[C, A]] = Eval.defer {
      def needVars(f: Variables[C] => Eval[Alg[C, A]]): Eval[Result[C, A]] =
        vars match {
          case Some(v) => Eval.defer(f(v)).flatMap(go(_, state, vars))
          case None    => Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(go(_, state, Some(v)))))
        }

      fa match {
        case NextId() => Eval.now(Result.Done(Right(new Unique.Token), state.writtenVariables, state.deferred))
        case Pure(a)  => Eval.now(Result.Done(Right(a), state.writtenVariables, state.deferred))
        case UseVariables(names) =>
          Eval.now(Result.Done(Right(()), state.writtenVariables ++ names.toChain, state.deferred))
        case UsedVariables() =>
          Eval.now(
            Result.Done(Right((state.readVariables ++ state.writtenVariables).iterator.toSet), state.writtenVariables, state.deferred)
          )
        case CycleAsk()             => Eval.now(Result.Done(Right(state.cycleSet), state.writtenVariables, state.deferred))
        case CycleOver(name, fa)    => go(fa, state.copy(cycleSet = state.cycleSet + name), vars)
        case CursorAsk()            => Eval.now(Result.Done(Right(state.cursor), state.writtenVariables, state.deferred))
        case CursorOver(cursor, fa) => go(fa, state.copy(cursor = cursor), vars)
        case task: Defer[C, a] =>
          val scoped = state.cycleSet.foldLeft[Alg[C, Unit]](CursorOver(state.cursor, task.fa.void)) { (fa, name) =>
            CycleOver(name, fa)
          }
          Eval.now(Result.Done(Right(()), state.writtenVariables, state.deferred.append(scoped)))
        case error: RaiseError[C] => Eval.now(Result.Done(Left(error.pe), state.writtenVariables, state.deferred))
        case attempt: Attempt[C, a] =>
          def attemptResult(result: Result[C, a]): Result[C, EitherNec[PositionalError[C], a]] = result match {
            case Result.Done(outcome @ Right(_), writtenVariables, deferred) => Result.Done(Right(outcome), writtenVariables, deferred)
            case Result.Done(outcome @ Left(_), _, _) => Result.Done(Right(outcome), state.writtenVariables, state.deferred)
            case Result.NeedVars(f)                   => Result.NeedVars(v => Eval.defer(f(v)).map(attemptResult))
          }
          go(attempt.fa, state, vars).map(attemptResult)
        case request: NeedVars[C, A] => needVars(request.f)
        case parAp: ParAp[C, a, A] =>
          val childState: State[C] = state.copy(
            readVariables = state.readVariables ++ state.writtenVariables,
            writtenVariables = Chain.empty,
            deferred = Chain.empty
          )
          (go(parAp.fa, childState, vars), go(parAp.fab, childState, vars)).mapN { (left, right) =>
            combineResults(left, right, state)((value, f) => f(value))
          }
        case bind: FlatMap[C, a, A] =>
          def continueBind(result: Result[C, a], currentVars: Option[Variables[C]]): Eval[Result[C, A]] = result match {
            case Result.Done(Left(errors), writtenVariables, deferred) => Eval.now(Result.Done(Left(errors), writtenVariables, deferred))
            case Result.Done(Right(a), writtenVariables, deferred) =>
              go(bind.f(a), state.copy(writtenVariables = writtenVariables, deferred = deferred), currentVars)
            case Result.NeedVars(f) =>
              // With variables supplied, go resolves every demand.
              Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(continueBind(_, Some(v)))))
          }
          go(bind.fa, state, vars).flatMap(continueBind(_, vars))
      }
    }

    def finish[A](result: Result[C, A], vars: Option[Variables[C]], inheritedUsage: Chain[String]): Eval[Result[C, A]] = Eval.defer {
      result match {
        case Result.Done(_, _, deferred) if deferred.isEmpty => Eval.now(result)
        case Result.Done(outcome, used, deferred) =>
          val readUsage = inheritedUsage ++ used
          val scope = State[C](readUsage, Chain.empty, Chain.empty, Set.empty, Cursor.empty)
          def merge(validation: Result[C, Unit]): Result[C, A] = validation match {
            case Result.Done(validation, extraUsage, _) =>
              Result.Done((validation, outcome).parMapN((_, value) => value), used ++ extraUsage, Chain.empty)
            case Result.NeedVars(f) => Result.NeedVars(v => Eval.defer(f(v)).map(merge))
          }
          val checks = deferred.foldLeft(Eval.now[Result[C, Unit]](Result.Done(Right(()), Chain.empty, Chain.empty))) { (checks, task) =>
            val next = go(task, scope, vars).flatMap(finish(_, vars, readUsage))
            (checks, next).mapN((left, right) => combineResults(left, right, scope)((_, _) => ()))
          }
          checks.map(merge)
        case Result.NeedVars(f) =>
          Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(finish(_, Some(v), inheritedUsage))))
      }
    }

    def complete(result: Result[C, A0], vars: Variables[C]): Eval[EitherNec[PositionalError[C], A0]] = result match {
      case Result.Done(outcome, _, _) => Eval.now(outcome)
      case Result.NeedVars(f)         => Eval.defer(f(vars)).flatMap(complete(_, vars))
    }

    go(fa, State[C](Chain.empty, Chain.empty, Chain.empty, Set.empty, Cursor.empty), None)
      .flatMap(finish(_, None, Chain.empty))
      .value match {
      case Result.Done(outcome, _, _) => outcome.map(value => (_: Variables[C]) => Right(value))
      case Result.NeedVars(f)         => Right(vars => Eval.defer(f(vars)).flatMap(complete(_, vars)).value)
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

    def raiseErrors(errors: NonEmptyChain[PositionalError[C]]): Alg[C, Nothing] =
      Alg.RaiseError(errors)

    def raise[A](message: String, carets: List[C]): Alg[C, A] =
      cursorAsk.flatMap(c => raiseError(PositionalError(c, carets, message)))

    def validate(message: String, carets: List[C]): Alg[C, Unit] =
      defer(raise[Unit](message, carets))

    def validate(errors: NonEmptyChain[PositionalError[C]]): Alg[C, Unit] =
      defer(raiseErrors(errors))

    def raiseEither[A](e: Either[String, A], carets: List[C]): Alg[C, A] =
      e.fold(raise(_, carets), pure)

    def raiseOpt[A](oa: Option[A], message: String, carets: List[C]): Alg[C, A] =
      raiseEither(oa.toRight(message), carets)

    def modifyError[A](f: PositionalError[C] => PositionalError[C])(fa: Alg[C, A]): Alg[C, A] =
      fa.handleErrorWith(errors => raiseErrors(errors.map(f)))

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

    def defer[A](fa: Alg[C, A]): Alg[C, Unit] = Alg.Defer(fa)

    def delay[A](fa: => Alg[C, A]): Alg[C, A] =
      unit.flatMap(_ => fa)
  }
  object Ops {
    def apply[C] = new Ops[C] {}
  }
}
