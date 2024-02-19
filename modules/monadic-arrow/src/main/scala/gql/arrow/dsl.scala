package gql.arrow

import gql.resolver._
import gql._
import org.tpolecat.sourcepos.SourcePos
import gql.dsl.FieldBuilder

trait ResolverArrowDsl[F[_]] extends Language[Resolver[F, *, *]] {
  def argument[A](arg: Arg[A])(implicit sp: SourcePos): Decl[Var[A]] =
    liftArrow(_.andThen(Resolver.argument[F, Unit, A](arg)))

  implicit class FieldBuilderOps[A](private val fb: FieldBuilder[F, A]) {
    def proc[B](f: Var[A] => Decl[Var[B]])(implicit sp: SourcePos, tpe: => ast.Out[F, B]): ast.Field[F, A, B] =
      fb.from(procFull(f))(tpe)
  }
}

object dsl {
  def apply[F[_]]: ResolverArrowDsl[F] = new ResolverArrowDsl[F] {}
}
