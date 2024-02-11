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
import gql.resolver.Resolver

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
import cats.free._
import cats.arrow._
import cats._
import cats.data._
object Attempt2 {
  final case class FetchVar[A](id: Int)
  type Var[A] = FreeApplicative[FetchVar, A]
  sealed trait ArrowAlg[Arrow0[_, _], A]
  object ArrowAlg {
    final case class Declare[Arrow0[_, _], A, B](
        v: Var[A],
        arrow: Arrow0[A, B]
    ) extends ArrowAlg[Arrow0, Var[B]]
  }

  type FreeArrow[Arrow0[_, _], A] = Free[ArrowAlg[Arrow0, *], A]

  def declare[Arrow0[_, _], A, B](v0: Var[A])(arrow: Arrow0[A, B]): FreeArrow[Arrow0, Var[B]] =
    Free.liftF[ArrowAlg[Arrow0, *], Var[B]](ArrowAlg.Declare(v0, arrow))

  object Compiler {
    def compileFull[Arrow0[_, _]: Arrow, A, B](f: Var[A] => FreeArrow[Arrow0, Var[B]]): Arrow0[A, B] = {
      type U = Array[Any]
      type S = (Int, Arrow0[U, U])
      type G[C] = State[S, C]

      def nextId: G[Int] = State[S, Int] { case (x, u) => ((x + 1, u), x) }

      val initVar = FetchVar[A](0)
      val init: Var[A] = FreeApplicative.lift(initVar)
      val program = f(init)

      def varCompiler(x: U) = new (FetchVar ~> Id) {
        def apply[A0](fa: FetchVar[A0]): Id[A0] = x(fa.id).asInstanceOf[A0]
      }
      val arrowCompiler = new (ArrowAlg[Arrow0, *] ~> G) {
        def apply[A1](fa: ArrowAlg[Arrow0, A1]): G[A1] = fa match {
          case alg: ArrowAlg.Declare[Arrow0, a, b] =>
            nextId.flatMap { thisId =>
              val fetchVar = FetchVar[b](thisId)
              val thisArr: Arrow0[U, U] = alg.arrow
                .first[U]
                .lmap[U](m => (alg.v.foldMap(varCompiler(m)), m))
                .rmap { case (b, u) => u.update(fetchVar.id, b); u }

              State.modify[S] { case (x, arr) => (x, arr >>> thisArr) }.as(FreeApplicative.lift(fetchVar))
            }
        }
      }

      val ((varNum, arrowProgram), lastVar) = program
        .foldMap(arrowCompiler)
        .run((1, Arrow[Arrow0].lift[U, U](identity)))
        .value

      arrowProgram
        .map(u => lastVar.foldMap(varCompiler(u)))
        .lmap[A] { a =>
          val arr = Array.ofDim[Any](varNum)
          arr.update(0, a)
          arr
        }
    }
  }

  trait Dsl[Arrow0[_, _]] {
    implicit class Syntax[A](private val v: Var[A]) {
      def decl[B](f: Arrow0[A, A] => Arrow0[A, B])(implicit A: Arrow[Arrow0]): FreeArrow[Arrow0, Var[B]] =
        declare[Arrow0, A, B](v)(f(A.lift(identity)))
    }

    def decl[A](arrow: Arrow0[Unit, A])(implicit A: Arrow[Arrow0]): FreeArrow[Arrow0, Var[A]] =
      ().pure[Var].decl(_ => arrow)
  }

  final class PartiallyAppliedCompiler[A] {
    def apply[Arrow0[_, _]: Arrow, B](f: Var[A] => FreeArrow[Arrow0, Var[B]]): Arrow0[A, B] =
      Compiler.compileFull(f)
  }

  def compile[A]: PartiallyAppliedCompiler[A] = new PartiallyAppliedCompiler[A]

  object Test {
    case class MyArrow[A, B](f: A => B)
    implicit lazy val arrowForMyArrow: Arrow[MyArrow] = 
      new Arrow[MyArrow] {
        override def compose[A, B, C](f: MyArrow[B,C], g: MyArrow[A,B]): MyArrow[A,C] = 
          MyArrow(f.f.compose(g.f))

        override def first[A, B, C](fa: MyArrow[A,B]): MyArrow[(A, C),(B, C)] = 
          MyArrow { case (a, c) => (fa.f(a), c) }

        override def lift[A, B](f: A => B): MyArrow[A,B] = 
          MyArrow(f)
      }

    object MyArrowDsl extends Dsl[MyArrow]
    import MyArrowDsl._

    val result: MyArrow[Int, String] = compile { init =>
      for {
        x <- init.decl(_.map(_ - 2).map(_ + 2).map(_ - 2))
        y <- init.decl(_.map(_ * 2))
        k <- (x, x, x).tupled.decl(_.map { case (x1, x2, x3) => x1 + x2 + x3 })
        z <- (k, y).tupled.decl(_.map { case (k, y) => k + y }.map(_.toString))
      } yield z
    }
  }

  object Practical {
    import cats.effect._
    object ResolverDsl extends Dsl[Resolver[IO, *, *]]
    import ResolverDsl._

    import gql.dsl.all._

    val result: Resolver[IO, Int, String] = compile { init =>
      for {
        x <- init.decl(_.evalMap(x => IO(x - 2)).evalMap(x => IO(x + 2)).evalMap(x => IO(x - 2)))
        y <- init.decl(_.streamMap(i => fs2.Stream(i)))
        age <- decl(arged(arg[Int]("age")))
        z <- (x, y, age).tupled.decl(_.map { case (x, y, age) => x + y + age }.map(_.toString))
      } yield z
    }
  }
}
