package gql.arrow

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

class Language[Arrow0[_, _]] {
  final type Var[A] = FreeApplicative[FetchVar, A]

  sealed trait Declaration[A]
  object Declaration {
    case class Declare[A, B](v: Var[A], arrow: Arrow0[A, B], pos: SourcePos) extends Declaration[Var[B]]
  }

  final type Decl[A] = Free[Declaration, A]

  def declare[A, B](v: Var[A])(f: Arrow0[A, B])(implicit sp: SourcePos): Decl[Var[B]] =
    Free.liftF[Declaration, Var[B]](Declaration.Declare(v, f, sp))

  def compileFull[A, B](f: Var[A] => Decl[Var[B]])(implicit arrow: Arrow[Arrow0], sp: SourcePos): Arrow0[A, B] = {
    val init = FreeApplicative.lift(FetchVar[A](0, None, sp))
    val program = f(init)
    type U = Vector[Any]
    type S = (Int, Arrow0[U, U])
    type G[C] = State[S, C]

    val nextId: G[Int] = State[S, Int] { case (x, u) => ((x + 1, u), x) }

    def varCompiler(x: U) = new (FetchVar ~> Id) {
      def apply[A0](fa: FetchVar[A0]): Id[A0] =
        if (fa.compilerPos eq sp) x(fa.id).asInstanceOf[A0]
        else {
          val msg = fa.pos match {
            case None =>
              s"""|Initial variable introduced at ${fa.compilerPos}.
                  |Variables that were not declared in this scope may not be referenced.
                  |Example:
                  |```
                  |compile[Int]{ init =>
                  |  for {
                  |    y <- init.apply(_.andThen(compile[Int]{ _ =>
                  |      // referencing 'init' here is an error
                  |      init.apply(_.map(_ + 1))
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
                  |compile[Int]{ init =>
                  |  for {
                  |    x <- init.apply(_.map(_ + 1))
                  |    y <- init.apply(_.andThen(compile[Int]{ _ =>
                  |      // referencing 'x' here is an error
                  |      x.apply(_.map(_ + 1))
                  |    }))
                  |  } yield y
                  |}
                  |```""".stripMargin
          }
          throw new RuntimeException(
            s"""|Variable closure error.
                |$msg""".stripMargin
          )
        }
    }

    val arrowCompiler = new (Declaration ~> G) {
      def apply[A1](fa: Declaration[A1]): G[A1] = fa match {
        case alg: Declaration.Declare[a, b] =>
          nextId.flatMap { thisId =>
            val fetchVar = FetchVar[b](thisId, alg.pos.some, sp)
            val thisArr: Arrow0[U, U] = alg.arrow
              .first[U]
              .lmap[U](m => (alg.v.foldMap(varCompiler(m)), m))
              .rmap { case (b, u) => u.updated(fetchVar.id, b) }

            State.modify[S] { case (x, arr) => (x, arr >>> thisArr) }.as(FreeApplicative.lift(fetchVar))
          }
      }
    }

    val ((varNum, arrowProgram), lastVar) = program
      .foldMap(arrowCompiler)
      .run((1, Arrow[Arrow0].lift[U, U](identity)))
      .value

    val b = Vector.fill[Any](varNum)(null)
    arrowProgram
      .map(u => lastVar.foldMap(varCompiler(u)))
      .lmap[A] { a =>
        b.updated(0, a)
      }
  }
}

abstract class LanguageDsl[Arrow0[_, _]: Arrow] extends Language[Arrow0] { self =>
  def liftArrow[A](f: Arrow0[Unit, Unit] => Arrow0[Unit, A])(implicit sp: SourcePos): Decl[Var[A]] =
    declare[Unit, A](().pure[Var])(f(Arrow[Arrow0].id[Unit]))

  implicit class VarOps[A](v: Var[A]) {
    def declare[B](f: Arrow0[A, B])(implicit sp: SourcePos): Decl[Var[B]] = self.declare(v)(f)

    def apply[B](f: Arrow0[A, A] => Arrow0[A, B])(implicit sp: SourcePos): Decl[Var[B]] =
      declare(f(Arrow[Arrow0].id[A]))
  }

  final class PartiallyAppliedLanguageCompiler[A](private val dummy: Boolean = true) {
    def apply[B](f: Var[A] => Decl[Var[B]])(implicit sp: SourcePos): Arrow0[A, B] = compileFull(f)
  }

  def compile[A]: PartiallyAppliedLanguageCompiler[A] = new PartiallyAppliedLanguageCompiler[A]
}
