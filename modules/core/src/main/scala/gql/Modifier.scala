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
package gql

import gql.ast._
import cats.arrow.ArrowChoice

sealed trait Modifier
object Modifier {
  case object List extends Modifier
  case object NonNull extends Modifier
}

/** A very convinient algebra for transforming GraphQL types with modifiers. It can take a GraphQL type to a Scala type and back.
  */
final case class ModifierStack[+T](modifiers: List[Modifier], inner: T) {
  def map[B](f: T => B): ModifierStack[B] =
    ModifierStack(modifiers, f(inner))

  def set[B](t: B): ModifierStack[B] =
    map(_ => t)

  def push(m: Modifier): ModifierStack[T] =
    ModifierStack(m :: modifiers, inner)

  // Base case is optional
  def invert: InverseModifierStack[T] = {
    def optional: InverseModifierStack[T] => InverseModifierStack[T] =
      _.push(InverseModifier.Optional)

    def go(
        ms: List[Modifier],
        parentOp: InverseModifierStack[T] => InverseModifierStack[T]
    ): InverseModifierStack[T] =
      ms match {
        case Nil                      => parentOp(InverseModifierStack(Nil, inner))
        case Modifier.List :: rest    => parentOp(go(rest, optional).push(InverseModifier.List))
        case Modifier.NonNull :: rest => go(rest, identity)
      }

    go(modifiers, optional)
  }

  def show(showInner: T => String): String = modifiers match {
    case Nil                      => showInner(inner)
    case Modifier.List :: rest    => s"[${ModifierStack(rest, inner).show(showInner)}]"
    case Modifier.NonNull :: rest => s"${ModifierStack(rest, inner).show(showInner)}!"
  }

  import gql.parser.{Type, NonNullType}
  def toType(implicit ev: T <:< String): Type =
    modifiers.foldLeft(Type.Named(ev(inner)): Type) {
      case (accum: NonNullType, Modifier.NonNull) => Type.NonNull(accum)
      case (accum, _)                             => Type.List(accum)
    }
}

object ModifierStack {
  def fromOut[F[_]](t: Out[F, ?]): ModifierStack[OutToplevel[F, ?]] =
    InverseModifierStack.fromOut(t).invert

  def fromIn(t: In[?]): ModifierStack[InToplevel[?]] =
    InverseModifierStack.fromIn(t).invert

  import gql.parser.Type
  def fromType(t: Type): ModifierStack[String] = {
    import Type._
    t match {
      case Named(name) => ModifierStack(Nil, name)
      case List(of)    => fromType(of).push(Modifier.List)
      case NonNull(of) => fromType(of).push(Modifier.NonNull)
    }
  }
}

sealed trait InverseModifier
object InverseModifier {
  case object List extends InverseModifier
  case object Optional extends InverseModifier
}

/** The Scala counterpart to [[ModifierStack]].
  *
  * Note that they are not equivalent, since [[ModifierStack]] explicitly declares [[Modifier.NonNull]], while [[InverseModifierStack]]
  * explicitly declares the opposite; [[InverseModifier.Optional]].
  */
final case class InverseModifierStack[+T](modifiers: List[InverseModifier], inner: T) {
  def map[B](f: T => B): InverseModifierStack[B] =
    InverseModifierStack(modifiers, f(inner))

  def set[B](t: B): InverseModifierStack[B] =
    InverseModifierStack(modifiers, t)

  def push(m: InverseModifier): InverseModifierStack[T] =
    InverseModifierStack(m :: modifiers, inner)

  // Converts from scala view to graphql view
  // Note that nested options will be flattened
  // Base case is non-null in the scala world
  def invert: ModifierStack[T] = {
    def nonNull: ModifierStack[T] => ModifierStack[T] =
      _.push(Modifier.NonNull)

    def go(
        ms: List[InverseModifier],
        parentOp: ModifierStack[T] => ModifierStack[T]
    ): ModifierStack[T] = {
      ms match {
        case Nil                              => parentOp(ModifierStack(Nil, inner))
        case InverseModifier.List :: rest     => parentOp(go(rest, nonNull).push(Modifier.List))
        case InverseModifier.Optional :: rest => go(rest, identity)
      }
    }
    go(modifiers, nonNull)
  }

  def showScala(showInner: T => String): String = modifiers match {
    case Nil => showInner(inner)
    case InverseModifier.List :: rest =>
      s"List[${InverseModifierStack(rest, inner).showScala(showInner)}]"
    case InverseModifier.Optional :: rest =>
      s"Option[${InverseModifierStack(rest, inner).showScala(showInner)}]"
  }
}

object InverseModifierStack {
  def fromOut[F[_]](t: Out[F, ?]): InverseModifierStack[OutToplevel[F, ?]] =
    t match {
      case t: OutToplevel[F, ?] => InverseModifierStack(Nil, t)
      case OutArr(of, _, _)     => fromOut(of).push(InverseModifier.List)
      case o: OutOpt[F, ?, ?]   => fromOut(o.of).push(InverseModifier.Optional)
    }

  def fromIn(t: In[?]): InverseModifierStack[InToplevel[?]] =
    t match {
      case t: InToplevel[?] => InverseModifierStack(Nil, t)
      case InArr(of, _)     => fromIn(of).push(InverseModifier.List)
      case o: InOpt[?]      => fromIn(o.of).push(InverseModifier.Optional)
    }

  def toIn(ms: InverseModifierStack[InToplevel[?]]): In[?] =
    ms.modifiers match {
      case InverseModifier.List :: xs =>
        toIn(InverseModifierStack(xs, ms.inner)) match {
          case e: In[a] => InArr[a, Unit](e, _ => Right(()))
        }
      case InverseModifier.Optional :: xs =>
        toIn(InverseModifierStack(xs, ms.inner)) match {
          case e: In[a] => InOpt[a](e)
        }
      case Nil => ms.inner
    }
}

import cats.implicits._
object language {
  import cats.free._
  import cats.arrow._
  import cats._
  import cats.data._
  type Var[A] = FreeApplicative[FetchVar, A]
  type ArrowM[Arrow0[_, _], A] = Free[ArrowAlg[*, Arrow0], A]
  object ArrowM {
    def compile[Arrow0[_, _]: Arrow, A, B](f: Var[A] => ArrowM[Arrow0, Var[B]]): Arrow0[A, B] = {
      type M = Map[FetchVar[?], Any]
      type S = (Int, Arrow0[M, M])
      type G[C] = State[S, C]

      def nextId = State[S, Int] { case (x, arr) => ((x + 1, arr), x) }

      val initVar = FetchVar[A](0)
      val init: Var[A] = FreeApplicative.lift(initVar)

      val program = f(init)

      def compiler(m: M) = new (FetchVar ~> Id) {
        def apply[A0](fa: FetchVar[A0]): Id[A0] = m(fa).asInstanceOf[A0]
      }
      val ((_, arr), outVar) = program
        .foldMap {
          new (ArrowAlg[*, Arrow0] ~> G) {
            def apply[A1](fa: ArrowAlg[A1, Arrow0]): G[A1] = fa match {
              case alg: ArrowAlg.Declare[a, b, Arrow0] =>
                nextId.flatMap { thisId =>
                  val fetchVar = FetchVar[b](thisId)
                  val thisArr: Arrow0[M, M] = alg.arrow
                    .first[M]
                    .lmap[M](m => (alg.v.foldMap(compiler(m)), m))
                    .rmap { case (b, m) => m + (fetchVar -> b) }

                  State.modify[S] { case (x, arr) => (x, arr >>> thisArr) }.as(FreeApplicative.lift(fetchVar))
                }
            }
          }
        }
        .run((1, Arrow[Arrow0].lift[M, M](identity)))
        .value

      arr.map(m => outVar.foldMap(compiler(m))).lmap[A](a => Map(initVar -> a))
    }

    def declare[Arrow0[_, _], A, B](v: Var[A])(arrow: Arrow0[A, B]): ArrowM[Arrow0, Var[B]] =
      Free.liftF[ArrowAlg[*, Arrow0], Var[B]](ArrowAlg.Declare(v, arrow))
  }
}
import language._

final case class FetchVar[A](id: Int)

sealed trait ArrowAlg[A, Arrow[_, _]]
object ArrowAlg {
  final case class Declare[A, B, Arrow[_, _]](v: Var[A], arrow: Arrow[A, B]) extends ArrowAlg[Var[B], Arrow]
}

object Test {
  final case class MyArrow[A, B](f: A => B)
  implicit lazy val arrowForMyArrow: ArrowChoice[MyArrow] = ???

  implicit class Dsl[A](private val v: Var[A]) {
    def mapv[B](f: A => B): ArrowM[MyArrow, Var[B]] = ArrowM.declare(v)(MyArrow(f))
  }

  final class PartiallyAppliedCompiler[A] {
    def apply[B](f: Var[A] => ArrowM[MyArrow, Var[B]]): MyArrow[A, B] = ArrowM.compile(f)
  }

  def compile[A]: PartiallyAppliedCompiler[A] = new PartiallyAppliedCompiler[A]

  val result: MyArrow[Int, String] = compile[Int] { init =>
    for {
      x <- init.mapv(_ * 2)
      _ <- x.mapv(_ * 2)
      y <- (init, x).tupled.mapv { case (i: Int, x: Int) => i + x }
      combined <- (x, y).tupled.mapv { case (x, y) => x + y }
      out <- combined.mapv(_.toString)
    } yield out
  }
}
