package gql.arrow

import gql.resolver._
import gql._
import org.tpolecat.sourcepos.SourcePos

trait dsl[F[_]] extends LanguageDsl[Resolver[F, *, *]] {
  def argument[A](arg: Arg[A])(implicit sp: SourcePos): Decl[Var[A]] =
    liftArrow(_.andThen(Resolver.argument[F, Unit, A](arg)))
}

object dsl {
  def apply[F[_]]: dsl[F] = new dsl[F] {}
}
