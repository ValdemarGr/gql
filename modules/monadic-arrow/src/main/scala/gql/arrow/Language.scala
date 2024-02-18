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

final case class Var[A](impl: FreeApplicative[FetchVar, A]) extends AnyVal
object Var {
  implicit val applicative: Applicative[Var] = new Applicative[Var] {
    val underlying = FreeApplicative.freeApplicative[FetchVar]
    override def ap[A, B](ff: Var[A => B])(fa: Var[A]): Var[B] = Var(underlying.ap(ff.impl)(fa.impl))
    override def pure[A](x: A): Var[A] = Var(underlying.pure(x))
  }
}

sealed trait DeclAlg[Arrow0[_, _], A]
object DeclAlg {
  case class Declare[Arrow0[_, _], A, B](v: Var[A], arrow: Arrow0[A, B], pos: SourcePos) extends DeclAlg[Arrow0, Var[B]]
}

class Language[Arrow0[_, _]] {
  final type Declaration[A] = DeclAlg[Arrow0, A]
  final type Decl[A] = Free[Declaration, A]

  def declare[A, B](v: Var[A])(f: Arrow0[A, B])(implicit sp: SourcePos): Decl[Var[B]] =
    Free.liftF[Declaration, Var[B]](DeclAlg.Declare(v, f, sp))

  def compileFull[A, B](f: Var[A] => Decl[Var[B]])(implicit arrow: Arrow[Arrow0], sp: SourcePos): Arrow0[A, B] = {
    val init = Var(FreeApplicative.lift(FetchVar[A](0, None, sp)))
    val program = f(init)
    type U = Vector[Any]
    type S = (Int, Arrow0[U, U])
    type G[C] = State[S, C]

    val nextId: G[Int] = State[S, Int] { case (x, u) => ((x + 1, u), x) }

    def resolveVariable[V](v: Var[V]): Arrow0[U, V] = {
      val x = v.impl
      type M[X] = Chain[String]
      val errors = x.analyze[Chain[String]]{
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
        val r = x.foldMap{
          new (FetchVar ~> Reader[U, *]) {
            def apply[A0](fa: FetchVar[A0]): Reader[U, A0] = 
              Reader { u => u(fa.id).asInstanceOf[A0] }
          }
        }
        arrow.lift(r.run)
      }
    }

    val arrowCompiler = new (Declaration ~> G) {
      def apply[A1](fa: Declaration[A1]): G[A1] = fa match {
        case alg: DeclAlg.Declare[Arrow0, a, b] =>
          nextId.flatMap { thisId =>
            val fetchVar = FetchVar[b](thisId, alg.pos.some, sp)
            val getVar = resolveVariable(alg.v)
            val thisArr: Arrow0[U, U] =
              getVar
              .first[U]
              .lmap[U](m => (m, m))
              .andThen(alg.arrow.first[U])
              .rmap{ case (b, u) => u.updated(fetchVar.id, b) }

            State.modify[S] { case (x, arr) => (x, arr >>> thisArr) }.as(Var(FreeApplicative.lift(fetchVar)))
          }
      }
    }

    val ((varNum, arrowProgram), lastVar) = program
      .foldMap(arrowCompiler)
      .run((1, Arrow[Arrow0].lift[U, U](identity)))
      .value

    val b = Vector.fill[Any](varNum)(null)
    val outputArrow: Arrow0[U, B] = resolveVariable(lastVar)
    val initArrow: Arrow0[A, U] = arrow.lift[A, U](b.updated(0, _))
    initArrow andThen arrowProgram andThen outputArrow
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
