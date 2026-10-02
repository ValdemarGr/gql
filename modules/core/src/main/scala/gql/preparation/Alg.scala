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
import scala.collection.immutable.HashSet

sealed trait Alg[+C, +A] {
  def runToCompletion[C2 >: C](vm: VariableMap[C2]): EitherNec[PositionalError[C2], A] =
    Alg.runToCompletion(this, vm)
}
object Alg {
  trait UniqueId

  case object NextId extends Alg[Nothing, UniqueId]

  final case class UseVariable(name: String) extends Alg[Nothing, Unit]
  final case class UsedVariables() extends Alg[Nothing, Set[String]]

  case object CycleAsk extends Alg[Nothing, Set[String]]
  final case class CycleOver[C, A](name: String, fa: Alg[C, A]) extends Alg[C, A]

  case object CursorAsk extends Alg[Nothing, Cursor]
  final case class CursorOver[C, A](cursor: Cursor, fa: Alg[C, A]) extends Alg[C, A]

  final case class RaiseError[C](pe: NonEmptyChain[PositionalError[C]]) extends Alg[C, Nothing]

  final case class GetVars[C, A]() extends Alg[C, VariableMap[C]]

  final case class Resume[C, A](fa: Staged[C, A]) extends Alg[C, A]
  final case class Force[C, A](fa: Alg[C, A]) extends Alg[C, Staged[C, A]]

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

  sealed trait Staged[C, +A] {
    private[Alg] def complete(vm: VariableMap[C], used: Set[String]): Eval[Staged.Done[C, A]] =
      this match {
        case done: Staged.Done[C, A] =>
          Eval.now(
            if (used.isEmpty) done
            else if (done.usedVariables.isEmpty) done.copy(usedVariables = used)
            else done.copy(usedVariables = used ++ done.usedVariables)
          )
        case Staged.Deferred(cont) => Eval.defer(cont(vm, used))
      }

    def runToCompletion(vm: VariableMap[C]): EitherNec[PositionalError[C], A] =
      complete(vm, Set.empty).value.result
  }

  object Staged {
    final case class Done[C, +A](
        result: EitherNec[PositionalError[C], A],
        usedVariables: Set[String] = Set.empty
    ) extends Staged[C, A]

    final case class Deferred[C, +A](
        cont: (VariableMap[C], Set[String]) => Eval[Done[C, A]]
    ) extends Staged[C, A]
  }

  final case class LocalState(
      cycleSet: Set[String],
      cursor: Cursor
  )

  def eval[C, A](alg0: Alg[C, A]): Staged[C, A] = {
    import Staged._

    val loc0 = LocalState(Set.empty, Cursor.empty)

    def prepend[B](staged: Staged[C, B], used: Set[String]): Staged[C, B] =
      if (used.isEmpty) staged
      else
        staged match {
          case Done(result, variables) => Done(result, used ++ variables)
          case _: Deferred[C, B]       => Deferred((v, incoming) => staged.complete(v, if (incoming.isEmpty) used else incoming ++ used))
        }

    def bind[B, D](staged: Staged[C, B], f: B => Eval[Staged[C, D]]): Eval[Staged[C, D]] = Eval.defer {
      staged match {
        case Done(Left(pes), _)       => Eval.now(Done(Left(pes)))
        case Done(Right(value), used) => f(value).map(prepend(_, used))
        case Deferred(cont) =>
          Eval.now(Deferred((v, used) => Eval.defer(cont(v, used)).flatMap(bind(_, f)).flatMap(_.complete(v, Set.empty))))
      }
    }

    def combine[B, D](left: Staged[C, B], right: Staged[C, B => D]): Staged[C, D] = {
      def completed(l: Done[C, B], r: Done[C, B => D], used: Set[String]): Done[C, D] =
        Done((l.result.toValidated, r.result.toValidated).mapN((b, f) => f(b)).toEither, used)

      (left, right) match {
        case (l: Done[C, B], r: Done[C, B => D]) =>
          completed(l, r, if (l.usedVariables.isEmpty) r.usedVariables else l.usedVariables ++ r.usedVariables)
        case _ =>
          Deferred((v, used) =>
            left.complete(v, used).flatMap { l =>
              right.complete(v, l.result.fold(_ => used, _ => l.usedVariables)).map(r => completed(l, r, r.usedVariables))
            }
          )
      }
    }

    def attempt[B](staged: Staged[C, B]): Staged[C, EitherNec[PositionalError[C], B]] = staged match {
      case Done(result, used) => Done(Right(result), result.fold(_ => Set.empty, _ => used))
      case Deferred(cont) =>
        Deferred((v, used) =>
          Eval.defer(cont(v, used)).map(done => Done(Right(done.result), done.result.fold(_ => used, _ => done.usedVariables)))
        )
    }

    def rec[B](
        fa: Alg[C, B],
        loc: LocalState
    ): Eval[Staged[C, B]] = Eval.defer[Staged[C, B]] {
      fa match {
        case NextId            => Eval.now(Done(Right(new UniqueId {})))
        case Pure(a)           => Eval.now(Done(Right(a)))
        case UseVariable(name) => Eval.now(Done(Right(()), HashSet(name)))
        case UsedVariables()   => Eval.now(Deferred((_, used) => Eval.now(Done(Right(used), used))))
        case fm: FlatMap[C, a, B] =>
          rec[a](fm.fa, loc).flatMap(staged => bind(staged, (value: a) => rec[B](fm.f(value), loc)))
        case parAp: ParAp[C, a, B] =>
          (rec(parAp.fa, loc), rec(parAp.fab, loc)).mapN(combine[a, B])
        case CycleAsk               => Eval.now(Done(Right(loc.cycleSet)))
        case CycleOver(name, fa)    => rec(fa, loc.copy(cycleSet = loc.cycleSet + name))
        case CursorAsk              => Eval.now(Done(Right(loc.cursor)))
        case CursorOver(cursor, fa) => rec(fa, loc.copy(cursor = cursor))
        case re: RaiseError[C]      => Eval.now(Done(Left(re.pe)))
        case alg: Attempt[C, a]     => rec(alg.fa, loc).map(attempt)
        case _: GetVars[C, a]       => Eval.now(Deferred[C, B]((v, used) => Eval.now(Done(Right(v), used))))
        case resume: Resume[C, B]   => Eval.now(resume.fa)
        case force: Force[C, a]     => rec(force.fa, loc).map(x => Done(Right(x)))
      }
    }

    rec[A](alg0, loc0).value
  }

  def runToCompletion[C, A](alg: Alg[C, A], vm: VariableMap[C]): EitherNec[PositionalError[C], A] =
    eval(alg).runToCompletion(vm)

  trait Ops[C] {
    def nextId: Alg[C, UniqueId] = Alg.NextId

    def useVariable(name: String): Alg[C, Unit] = Alg.UseVariable(name)

    def usedVariables: Alg[C, Set[String]] = Alg.UsedVariables()

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

    def resume[A](fa: Staged[C, A]): Alg[C, A] =
      Alg.Resume(fa)

    def force[A](fa: Alg[C, A]): Alg[C, Staged[C, A]] =
      Alg.Force(fa)

    def pause[A](fa: Alg[C, A]): Alg[C, Alg[C, A]] =
      force(fa).map(resume)
  }
  object Ops {
    def apply[C] = new Ops[C] {}
  }
}
