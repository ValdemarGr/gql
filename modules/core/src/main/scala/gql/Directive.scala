package gql

import gql.parser.{QueryAst => QA}
import gql.preparation.MergedFieldInfo

final case class Directive[A](
    name: String,
    arg: DirectiveArg[A] = DirectiveArg.Empty
)

// Ad-hoc Applicative (pure, empty structure) from Apply
sealed trait DirectiveArg[A]
object DirectiveArg {
  final case class WithArg[A](arg: Arg[A]) extends DirectiveArg[A]
  case object Empty extends DirectiveArg[Unit]
}

// Add as necessary
sealed trait Position[+F[_], A] {
  def directive: Directive[A]
}
object Position {
  trait FieldHandler[F[_], A] {
    def apply[I, C](
        a: A,
        field: ast.Field[F, I, ?],
        mfa: MergedFieldInfo[F, C]
    ): Either[String, List[(ast.Field[F, I, ?], MergedFieldInfo[F, C])]]
  }
  final case class Field[F[_], A](
      directive: Directive[A],
      handler: FieldHandler[F, A]
  ) extends Position[F, A]

  trait QueryHandler[Struct[_], A] {
    def apply[C](a: A, query: Struct[C]): Either[String, List[Struct[C]]]
  }
  final case class FragmentSpread[A](
      directive: Directive[A],
      handler: QueryHandler[QA.FragmentSpread, A]
  ) extends Position[Nothing, A]
  final case class InlineFragmentSpread[A](
      directive: Directive[A],
      handler: QueryHandler[QA.InlineFragment, A]
  ) extends Position[Nothing, A]
}
