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
    final case class Success[C, A](value: A, usedVariables: Chain[String], deferred: Chain[Alg[C, Unit]]) extends Result[C, A]
    final case class Failure[C](errors: NonEmptyChain[PositionalError[C]], usedVariables: Chain[String], deferred: Chain[Alg[C, Unit]])
        extends Result[C, Nothing]
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
    val G = Ops[C]
    def liftResult[B](result: Result[C, B], vars: Option[Variables[C]]): Eval[Alg[C, B]] = result match {
      case Result.Success(value, usedVariables, deferred) =>
        Eval.now(
          NonEmptyChain.fromChain(usedVariables).traverse_(G.useVariables) *>
            deferred.traverse_(G.defer(_)) *> G.pure(value)
        )
      case Result.Failure(errors, usedVariables, deferred) =>
        Eval.now(
          NonEmptyChain.fromChain(usedVariables).traverse_(G.useVariables) *>
            deferred.traverse_(G.defer(_)) *> G.raiseErrors(errors)
        )
      case Result.NeedVars(f) =>
        vars match {
          case Some(v) => Eval.defer(f(v)).flatMap(liftResult(_, vars))
          case None    => Eval.now(Alg.NeedVars[C, B](v => Eval.defer(f(v)).flatMap(liftResult(_, Some(v)))))
        }
    }

    def go[A](
        fa: Alg[C, A],
        state: State[C],
        vars: Option[Variables[C]]
    ): Eval[Result[C, A]] = Eval.defer {
      def needVars(f: Variables[C] => Eval[Alg[C, A]], scope: State[C] = state): Eval[Result[C, A]] =
        vars match {
          case Some(v) => Eval.defer(f(v)).flatMap(go(_, scope, vars))
          case None    => Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(go(_, scope, Some(v)))))
        }

      lazy val childState: State[C] = state.copy(
        readVariables = state.readVariables ++ state.writtenVariables,
        writtenVariables = Chain.empty,
        deferred = Chain.empty
      )

      fa match {
        case NextId() => Eval.now(Result.Success(new Unique.Token, state.writtenVariables, state.deferred))
        case Pure(a)  => Eval.now(Result.Success(a, state.writtenVariables, state.deferred))
        case UseVariables(names) =>
          Eval.now(Result.Success((), state.writtenVariables ++ names.toChain, state.deferred))
        case UsedVariables() =>
          Eval.now(
            Result.Success((state.readVariables ++ state.writtenVariables).iterator.toSet, state.writtenVariables, state.deferred)
          )
        case CycleAsk()             => Eval.now(Result.Success(state.cycleSet, state.writtenVariables, state.deferred))
        case CycleOver(name, fa)    => go(fa, state.copy(cycleSet = state.cycleSet + name), vars)
        case CursorAsk()            => Eval.now(Result.Success(state.cursor, state.writtenVariables, state.deferred))
        case CursorOver(cursor, fa) => go(fa, state.copy(cursor = cursor), vars)
        case task: Defer[C, a] =>
          val scoped = state.cycleSet.foldLeft[Alg[C, Unit]](CursorOver(state.cursor, task.fa.void)) { (fa, name) =>
            CycleOver(name, fa)
          }
          Eval.now(Result.Success((), state.writtenVariables, state.deferred.append(scoped)))
        case error: RaiseError[C] => Eval.now(Result.Failure(error.pe, state.writtenVariables, state.deferred))
        case attempt: Attempt[C, a] =>
          def attemptResult(result: Result[C, a]): Result[C, EitherNec[PositionalError[C], a]] = result match {
            case Result.Success(value, writtenVariables, deferred) => Result.Success(Right(value), writtenVariables, deferred)
            case Result.Failure(errors, _, _)                      => Result.Success(Left(errors), state.writtenVariables, state.deferred)
            case Result.NeedVars(f)                                => Result.NeedVars(v => Eval.defer(f(v)).map(attemptResult))
          }
          go(attempt.fa, state, vars).map(attemptResult)
        case request: NeedVars[C, A] => needVars(request.f)
        case parAp: ParAp[C, a, A] =>
          go(parAp.fa, childState, vars).flatMap {
            case failed @ Result.Failure(errs1, usedVars1, deferred1) =>
              go(parAp.fab, childState, vars).flatMap {
                case Result.Failure(errs2, usedVars2, deferred2) =>
                  Eval.now(
                    Result.Failure(
                      errs1 ++ errs2,
                      state.writtenVariables ++ usedVars1 ++ usedVars2,
                      state.deferred ++ deferred1 ++ deferred2
                    )
                  )
                case Result.Success(_, usedVars2, deferred2) =>
                  Eval.now(
                    Result.Failure(errs1, state.writtenVariables ++ usedVars1 ++ usedVars2, state.deferred ++ deferred1 ++ deferred2)
                  )
                case Result.NeedVars(f) =>
                  needVars(v =>
                    Eval
                      .defer(f(v))
                      .flatMap(liftResult(_, Some(v)))
                      .flatMap(ab => liftResult(failed, Some(v)).flatMap(a => go(ParAp[C, a, A](a, ab), childState, Some(v))))
                      .flatMap(liftResult(_, Some(v)))
                  )
              }
            case done @ Result.Success(a, _, _) =>
              val fa: Eval[Result[C, A]] = go(parAp.fab, childState, vars).flatMap {
                case failed: Result.Failure[C] => Eval.now(failed)
                case Result.Success(f, usedVars, deferred) =>
                  Eval.now(Result.Success(f(a), usedVars, deferred))
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
            case failed: Result.Failure[C] => Eval.now(failed)
            case Result.Success(a, writtenVariables, deferred) =>
              go(bind.f(a), state.copy(writtenVariables = writtenVariables, deferred = deferred), currentVars)
            case Result.NeedVars(f) =>
              currentVars match {
                case Some(v) => Eval.defer(f(v)).flatMap(continueBind(_, currentVars))
                case None    => Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(continueBind(_, Some(v)))))
              }
          }
          go(bind.fa, state, vars).flatMap(continueBind(_, vars))
      }
    }

    def finish[A](result: Result[C, A], vars: Option[Variables[C]], inheritedUsage: Chain[String]): Eval[Result[C, A]] = Eval.defer {
      def validate(deferred: Chain[Alg[C, Unit]], readUsage: Chain[String])(
          combine: (Chain[String], EitherNec[PositionalError[C], Unit]) => Result[C, A]
      ): Eval[Result[C, A]] = {
        val scope = State[C](readUsage, Chain.empty, Chain.empty, Set.empty, Cursor.empty)
        def merge(validation: Result[C, Unit]): Result[C, A] = validation match {
          case Result.Success(_, used, _)      => combine(used, Right(()))
          case Result.Failure(errors, used, _) => combine(used, Left(errors))
          case Result.NeedVars(f)              => Result.NeedVars(v => Eval.defer(f(v)).map(merge))
        }
        val checks = deferred.foldLeft(Eval.now(G.unit)) { (checks, task) =>
          val next = go(task, scope, vars).flatMap(finish(_, vars, readUsage)).flatMap(liftResult(_, vars))
          (checks, next).mapN((checks, next) => G.parAp(checks)(next.as((_: Unit) => ())))
        }
        checks.flatMap(go(_, scope, vars)).map(merge)
      }

      result match {
        case Result.Success(value, used, deferred) =>
          if (deferred.isEmpty) Eval.now(result)
          else
            validate(deferred, inheritedUsage ++ used) { (extraUsage, validation) =>
              validation match {
                case Right(_)     => Result.Success(value, used ++ extraUsage, Chain.empty)
                case Left(errors) => Result.Failure(errors, used ++ extraUsage, Chain.empty)
              }
            }
        case Result.Failure(errors, used, deferred) =>
          if (deferred.isEmpty) Eval.now(result)
          else
            validate(deferred, inheritedUsage ++ used) { (extraUsage, validation) =>
              Result.Failure(validation.fold(_ ++ errors, _ => errors), used ++ extraUsage, Chain.empty)
            }
        case Result.NeedVars(f) =>
          Eval.now(Result.NeedVars(v => Eval.defer(f(v)).flatMap(finish(_, Some(v), inheritedUsage))))
      }
    }

    def complete(result: Result[C, A0], vars: Variables[C]): Eval[EitherNec[PositionalError[C], A0]] = result match {
      case Result.Success(value, _, _)  => Eval.now(Right(value))
      case Result.Failure(errors, _, _) => Eval.now(Left(errors))
      case Result.NeedVars(f)           => Eval.defer(f(vars)).flatMap(complete(_, vars))
    }

    go(fa, State[C](Chain.empty, Chain.empty, Chain.empty, Set.empty, Cursor.empty), None)
      .flatMap(finish(_, None, Chain.empty))
      .value match {
      case Result.Success(value, _, _)  => Right(_ => Right(value))
      case Result.Failure(errors, _, _) => Left(errors)
      case Result.NeedVars(f)           => Right(vars => Eval.defer(f(vars)).flatMap(complete(_, vars)).value)
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

    def defer[A](fa: Alg[C, A]): Alg[C, Unit] = Alg.Defer(fa)

    def delay[A](fa: => Alg[C, A]): Alg[C, A] =
      unit.flatMap(_ => fa)
  }
  object Ops {
    def apply[C] = new Ops[C] {}
  }
}
