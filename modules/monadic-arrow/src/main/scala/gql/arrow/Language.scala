package gql.arrow

import org.typelevel.scalaccompat.annotation._
import cats.implicits._
import cats.free._
import cats.arrow._
import cats._
import cats.data._
import org.tpolecat.sourcepos._

final case class FetchVar[A](
  id: Int,
  pos: Option[SourcePos],
  compilerPos: SourcePos
)

final case class Var[A](impl: FreeApplicative[FetchVar, A]) extends AnyVal
object Var {
  implicit val applicative: Applicative[Var] = new Applicative[Var] {
    val underlying = FreeApplicative.freeApplicative[FetchVar]
    override def ap[A, B](ff: Var[A => B])(fa: Var[A]): Var[B] = Var(underlying.ap(ff.impl)(fa.impl))
    override def pure[A](x: A): Var[A] = Var(underlying.pure(x))
  }
}

sealed trait DeclAlg[F[_, _], A]
object DeclAlg {
  final case class AskArrow[F[_, _]]() extends DeclAlg[F, Arrow[F]]
  final case class Declare[F[_, _], A, B](v: Var[A], arrow: F[A, B], pos: SourcePos) extends DeclAlg[F, Var[B]]
  final case class Choice[F[_, _], A, B, C](
    v: Var[Either[A, B]],
    bd: Var[A] => Free[DeclAlg[F, *], Var[C]],
    cd: Var[B] => Free[DeclAlg[F, *], Var[C]],
    ev: ArrowChoice[F],
    sp: SourcePos
  ) extends DeclAlg[F, Var[C]]
}

abstract class Language[F[_, _]] { self =>
  final type Declaration[A] = DeclAlg[F, A]
  final type Decl[A] = Free[Declaration, A]

  def declare[A, B](v: Var[A])(f: F[A, B])(implicit sp: SourcePos): Decl[Var[B]] =
    Language.declare(v)(f)

  def choice[A, B, C](v: Var[Either[A, B]])(l: Var[A] => Decl[Var[C]])(
    r: Var[B] => Decl[Var[C]]
  )(implicit sp: SourcePos, c: ArrowChoice[F]): Decl[Var[C]] =
    Language.choice(v)(l)(r)

  def askArrow: Decl[Arrow[F]] = Language.askArrow

  def procFull[A, B](f: Var[A] => Decl[Var[B]])(implicit arrow: Arrow[F], sp: SourcePos): F[A, B] =
    Language.proc[F, A, B](f)(arrow, sp)

  def liftArrow[A](fa: F[Any, A])(implicit sp: SourcePos): Decl[Var[A]] =
    declare(((): Any).pure[Var])(fa)

  def liftUnitArrow[A](fa: F[Unit, A])(implicit sp: SourcePos): Decl[Var[A]] =
    declare(().pure[Var])(fa)

  implicit class VarOps[A](private val v: Var[A]) {
    def rmap[B](f: A => B)(implicit sp: SourcePos): Decl[Var[B]] =
      askArrow.flatMap(arrow => andThen(arrow.lift(f)))

    def andThen[B](f: F[A, B])(implicit sp: SourcePos): Decl[Var[B]] = self.declare(v)(f)

    def apply[B](f: F[A, A] => F[A, B])(implicit sp: SourcePos): Decl[Var[B]] =
      askArrow.flatMap(arrow => andThen(f(arrow.id[A])))

    def liftArrowChoice[B, C, D](bd: F[B, D], cd: F[C, D])(implicit
      sp: SourcePos,
      c: ArrowChoice[F],
      ev: A <:< Either[B, C]
    ): Decl[Var[D]] =
      apply(_.map(ev.apply(_)).andThen(c.choice(bd, cd)))
  }

  implicit class VarEitherOps[A, B](private val v: Var[Either[A, B]]) {
    def choice[C](bd: Var[A] => Decl[Var[C]], cd: Var[B] => Decl[Var[C]])(implicit
      sp: SourcePos,
      c: ArrowChoice[F]
    ): Decl[Var[C]] = self.choice(v)(bd)(cd)
  }

  implicit class ArrowOps[A, B](private val arr: F[A, B]) {
    def proc[C](f: Var[B] => Decl[Var[C]])(implicit sp: SourcePos, arrow: Arrow[F]): F[A, C] =
      arr andThen procFull(f)
  }

  final class PartiallyAppliedLanguageCompiler[A](@unused private val dummy: Boolean = false) {
    def apply[B](f: Var[A] => Decl[Var[B]])(implicit sp: SourcePos, arrow: Arrow[F]): F[A, B] =
      procFull(f)
  }

  def proc[A]: PartiallyAppliedLanguageCompiler[A] = new PartiallyAppliedLanguageCompiler[A]
}

object Language {
  def declare[F[_, _], A, B](v: Var[A])(f: F[A, B])(implicit
    sp: SourcePos
  ): Free[DeclAlg[F, *], Var[B]] =
    Free.liftF[DeclAlg[F, *], Var[B]](DeclAlg.Declare(v, f, sp))

  def choice[F[_, _], A, B, C](v: Var[Either[A, B]])(l: Var[A] => Free[DeclAlg[F, *], Var[C]])(
    r: Var[B] => Free[DeclAlg[F, *], Var[C]]
  )(implicit sp: SourcePos, c: ArrowChoice[F]): Free[DeclAlg[F, *], Var[C]] =
    Free.liftF[DeclAlg[F, *], Var[C]](DeclAlg.Choice(v, l, r, c, sp))

  def askArrow[F[_, _]]: Free[DeclAlg[F, *], Arrow[F]] =
    Free.liftF[DeclAlg[F, *], Arrow[F]](DeclAlg.AskArrow())

  def proc[F[_, _], A, B](f: Var[A] => Free[DeclAlg[F, *], Var[B]])(implicit
    arrow: Arrow[F],
    sp: SourcePos
  ): F[A, B] = {
    type Declaration[X] = DeclAlg[F, X]
    type Decl[X] = Free[Declaration, X]
    val init = Var(FreeApplicative.lift(FetchVar[A](0, None, sp)))
    val program = f(init)
    type U = Vector[Any]
    type S = (Int, F[U, U])
    type G[C] = State[S, C]

    val nextId: G[Int] = State[S, Int] { case (x, u) => ((x + 1, u), x) }
    def alloc[A0](p: SourcePos): G[(Var[A0], Int)] =
      nextId.map(i => Var(FreeApplicative.lift(FetchVar[A0](i, Some(p), sp))) -> i)
    val getU: G[F[U, U]] = State.get[S].map { case (_, u) => u }
    def modifyU(f: F[U, U] => F[U, U]): G[Unit] = State.modify[S] { case (x, arr) => (x, f(arr)) }

    def resolveVariable[V](v: Var[V]): F[U, V] = {
      val x = v.impl
      type M[X] = Chain[String]
      val errors = x.analyze[Chain[String]] {
        new (FetchVar ~> M) {
          def apply[A0](fa: FetchVar[A0]): M[A0] =
            if (fa.compilerPos eq sp) Chain.empty
            else {
              val msg = fa.pos match {
                case None =>
                  s"""|Initial variable introduced at ${fa.compilerPos}.
                      |Variables that were not declared in this scope may not be referenced.
                      |Example:
                      |```
                      |proc[Int]{ i =>
                      |  for {
                      |    y <- i.apply(_.andThen(proc[Int]{ _ =>
                      |      // referencing 'i' here is an error
                      |      i.apply(_.map(_ + 1))
                      |    }))
                      |  } yield y
                      |}
                      |```""".stripMargin
                case Some(p) =>
                  s"""|Variable declared at ${p}.
                      |Compilation initiated at ${fa.compilerPos}.
                      |Variables that were not declared in this scope may not be referenced.
                      |Example:
                      |```
                      |proc[Int]{ i =>
                      |  for {
                      |    x <- i.apply(_.map(_ + 1))
                      |    y <- i.apply(_.andThen(proc[Int]{ _ =>
                      |      // referencing 'x' here is an error
                      |      x.apply(_.map(_ + 1))
                      |    }))
                      |  } yield y
                      |}
                      |```""".stripMargin
              }
              Chain.one(
                s"""|Variable closure error.
                    |$msg""".stripMargin
              )
            }
        }
      }

      if (errors.nonEmpty) {
        throw new RuntimeException(errors.mkString_("\n\n"))
      } else {
        val r = x.foldMap {
          new (FetchVar ~> Reader[U, *]) {
            def apply[A0](fa: FetchVar[A0]): Reader[U, A0] =
              Reader(u => u(fa.id).asInstanceOf[A0])
          }
        }
        arrow.lift(r.run)
      }
    }

    def introduce[C](v: Var[C]): F[U, (C, U)] =
      resolveVariable(v).first[U].lmap[U](m => (m, m))

    def setVar[C, D](arr: F[C, D], id: Int): F[(C, U), U] =
      arr.first[U].map { case (d, u) => u.updated(id, d) }

    val arrowCompiler: Declaration ~> G = new (Declaration ~> G) { self =>
      @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
      def apply[A1](fa: Declaration[A1]): G[A1] = fa match {
        case _: DeclAlg.AskArrow[F] => State.pure(Arrow[F])
        case alg: DeclAlg.Choice[F, a, b, a1] =>
          val reset = modifyU(_ => Arrow[F].id[U])
          def path[X](f: Var[X] => Decl[Var[a1]]): G[F[(U, X), a1]] =
            alloc[X](alg.sp).flatMap { case (x, i) =>
              reset *> (f(x).foldMap(self), getU).tupled.map { case (outVar, programArr) =>
                arrow.lift[(U, X), U] { case (u, x) => u.updated(i, x) } >>> programArr >>> resolveVariable(outVar)
              }
            }

          (
            alloc[a1](alg.sp),
            getU,
            (path(alg.bd), path(alg.cd)).mapN(alg.ev.choice(_, _))
          ).flatMapN { case ((a1, i1), arr, inner) =>
            val exec = introduce(alg.v).map { case (e, u) => e.leftMap((u, _)).map((u, _)) } >>> inner
            val full = setVar(exec, i1).lmap[U](m => (m, m))
            modifyU(_ => arr >>> full).as(a1)
          }
        case alg: DeclAlg.Declare[F, a, b] =>
          alloc[b](alg.pos).flatMap { case (v, i) =>
            modifyU(_ >>> introduce(alg.v) >>> setVar(alg.arrow, i)).as(v)
          }
      }
    }

    val ((varNum, arrowProgram), lastVar) = program
      .foldMap(arrowCompiler)
      .run((1, Arrow[F].lift[U, U](identity)))
      .value

    val b = Vector.fill[Any](varNum)(null)
    val outputArrow: F[U, B] = resolveVariable(lastVar)
    val initArrow: F[A, U] = arrow.lift[A, U](b.updated(0, _))
    initArrow >>> arrowProgram >>> outputArrow
  }
}
