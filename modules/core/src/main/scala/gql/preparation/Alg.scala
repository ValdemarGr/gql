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
  def runToCompletion[C2 >: C](vm: VariableMap[C2]): EitherNec[PositionalError[C2], A] =
    Alg.runToCompletion(this, vm)
}
object Alg {
  trait UniqueId

  case object NextId extends Alg[Nothing, UniqueId]

  case object CycleAsk extends Alg[Nothing, Set[String]]
  final case class CycleOver[C, A](name: String, fa: Alg[C, A]) extends Alg[C, A]

  case object CursorAsk extends Alg[Nothing, Cursor]
  final case class CursorOver[C, A](cursor: Cursor, fa: Alg[C, A]) extends Alg[C, A]

  final case class RaiseError[C](pe: NonEmptyChain[PositionalError[C]]) extends Alg[C, Nothing]

  final case class GetVars[C, A]() extends Alg[C, VariableMap[C]]

  final case class Resume[C, A](fa: PartialEvalRes[C, A]) extends Alg[C, A]
  final case class Force[C, A](fa: Alg[C, A]) extends Alg[C, PartialEvalRes[C, A]]

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

  sealed trait PartialEvalRes[C, +B]

  sealed trait EvalResult[C, +B] extends PartialEvalRes[C, B]

  object PartialEvalRes {
    final case class Result[C, +B](value: B) extends EvalResult[C, B]
    final case class Errors[C, +B](pes: NonEmptyChain[PositionalError[C]]) extends EvalResult[C, B]

    final case class NeedVars[C, +B](
        cont: VariableMap[C] => Eval[EvalResult[C, B]]
    ) extends PartialEvalRes[C, B]
  }

  final case class LocalState(
      cycleSet: Set[String],
      cursor: Cursor
  )

  def eval[C, A](alg0: Alg[C, A]): Outcome0[C, A] = {
    val P = PartialEvalRes

    val loc0 = LocalState(Set.empty, Cursor.empty)

    def rec[B](
        fa: Alg[C, B],
        loc: LocalState
    ): Eval[PartialEvalRes[C, B]] = Eval.defer {
      fa match {
        case NextId  => Eval.now(P.Result(new UniqueId {}))
        case Pure(a) => Eval.now(P.Result(a))
        case bind: FlatMap[C, a, B] =>
          rec[a](bind.fa, loc).flatMap {
            case P.Errors(pes) => Eval.now(P.Errors(pes))
            case P.Result(a)   => rec[B](bind.f(a), loc)
            // fuse the need vars
            case P.NeedVars(cont) =>
              Eval.now {
                P.NeedVars[C, B] { v =>
                  cont(v).flatMap {
                    case P.Errors(pes) => Eval.now(P.Errors(pes))
                    case P.Result(a) =>
                      rec[B](bind.f(a), loc).flatMap {
                        case P.Errors(pes) => Eval.now(P.Errors(pes))
                        case P.Result(b)   => Eval.now(P.Result(b))
                        // fusion
                        case nv: P.NeedVars[C, B] => nv.cont(v)
                      }
                  }
                }
              }
          }
        case parAp: ParAp[C, a, B] =>
          def combineER(
              l: EvalResult[C, a],
              r: EvalResult[C, a => B]
          ): EvalResult[C, B] =
            (l, r) match {
              case (P.Errors(pes1), P.Errors(pes2)) => P.Errors(pes1 ++ pes2)
              case (P.Errors(pes1), P.Result(_))    => P.Errors(pes1)
              case (P.Result(_), P.Errors(pes2))    => P.Errors(pes2)

              case (P.Result(a), P.Result(f)) => P.Result(f(a))
            }

          def combine(
              l: PartialEvalRes[C, a],
              r: PartialEvalRes[C, a => B]
          ): PartialEvalRes[C, B] =
            (l, r) match {
              case (l: EvalResult[C, a], r: EvalResult[C, a => B]) => combineER(l, r)

              case (P.NeedVars(contL), P.NeedVars(contR)) =>
                P.NeedVars[C, B](v => (contL(v), contR(v)).mapN(combineER))
              case (P.NeedVars(contL), r: EvalResult[C, a => B]) =>
                P.NeedVars[C, B](contL.andThen(_.map(l2 => combineER(l2, r))))
              case (l: EvalResult[C, a], P.NeedVars(contR)) =>
                P.NeedVars[C, B](contR.andThen(_.map(r2 => combineER(l, r2))))
            }

          (rec(parAp.fa, loc), rec(parAp.fab, loc)).mapN(combine)
        case CycleAsk               => Eval.now(P.Result(loc.cycleSet))
        case CycleOver(name, fa)    => rec(fa, loc.copy(cycleSet = loc.cycleSet + name))
        case CursorAsk              => Eval.now(P.Result(loc.cursor))
        case CursorOver(cursor, fa) => rec(fa, loc.copy(cursor = cursor))
        case re: RaiseError[C]      => Eval.now(P.Errors(re.pe))
        case alg: Attempt[C, a] =>
          rec(alg.fa, loc).map { res =>
            def from(er: EvalResult[C, a]): EvalResult[C, B] = er match {
              case P.Errors(pes) => P.Result(Left(pes))
              case P.Result(a)   => P.Result(Right(a))
            }
            res match {
              case er: EvalResult[C, a] => from(er)
              case P.NeedVars(cont)     => P.NeedVars(v => cont(v).map(from))
            }
          }
        case GetVars()            => Eval.now(P.NeedVars[C, B](v => Eval.now(P.Result(v))))
        case resume: Resume[C, B] => Eval.now(resume.fa)
        case force: Force[C, a]   => rec(force.fa, loc).map(x => P.Result(x))
      }
    }

    val res = rec[A](alg0, loc0).value

    val O = Outcome0
    def fromEr(er: EvalResult[C, A]): O.Now[C, A] = er match {
      case P.Errors(pes) => O.Now(Left(pes))
      case P.Result(a)   => O.Now(Right(a))
    }

    res match {
      case er: EvalResult[C, A] => fromEr(er)
      case P.NeedVars(cont)     => O.NonCacheable[C, A](cont(_).map(fromEr).value)
    }
  }

  def runToCompletion[C, A](alg: Alg[C, A], vm: VariableMap[C]): EitherNec[PositionalError[C], A] = {
    eval(alg) match {
      case Outcome0.Now(result)       => result
      case Outcome0.NonCacheable(run) => run(vm).result
    }
  }

  sealed trait Outcome0[C, A]
  object Outcome0 {
    final case class Now[C, A](
        result: EitherNec[PositionalError[C], A]
    ) extends Outcome0[C, A]
    final case class NonCacheable[C, A](
        run: VariableMap[C] => Now[C, A]
    ) extends Outcome0[C, A]
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
          ???
          // val s = state.copy(nextId = state.nextId + 1)
          // Eval.now(Outcome.Result(s.nextId, s))
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
    def nextId: Alg[C, UniqueId] = Alg.NextId

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

    def getVariables: Alg[C, VariableMap[C]] =
      Alg.GetVars()

    def resume[A](fa: PartialEvalRes[C, A]): Alg[C, A] =
      Alg.Resume(fa)

    def force[A](fa: Alg[C, A]): Alg[C, PartialEvalRes[C, A]] =
      Alg.Force(fa)

    def pause[A](fa: Alg[C, A]): Alg[C, Alg[C, A]] =
      force(fa).map(resume)
  }
  object Ops {
    def apply[C] = new Ops[C] {}
  }
}
