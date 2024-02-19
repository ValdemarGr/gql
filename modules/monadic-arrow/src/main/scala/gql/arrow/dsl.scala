package gql.arrow

import gql.resolver._
import gql._
import org.tpolecat.sourcepos.SourcePos
import gql.dsl.FieldBuilder

trait dsl[F[_]] extends LanguageDsl[Resolver[F, *, *]] {
  def argument[A](arg: Arg[A])(implicit sp: SourcePos): Decl[Var[A]] =
    liftArrow(_.andThen(Resolver.argument[F, Unit, A](arg)))

  implicit class FieldBuilderOps[A](private val fb: FieldBuilder[F, A]) {
    def compile[B](f: Var[A] => Decl[Var[B]])(implicit sp: SourcePos, tpe: => ast.Out[F, B]): ast.Field[F, A, B] =
      fb.from(compileFull(f))(tpe)
  }
}

object dsl {
  def apply[F[_]]: dsl[F] = new dsl[F] {}
}
