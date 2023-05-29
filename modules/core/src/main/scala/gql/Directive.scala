package gql

import gql.resolver._
import gql.parser.{QueryAst => QA}
import cats.parse.Caret

final case class Directive[A](
    name: String,
    arg: Directive.DirectiveArg[A] = Directive.DirectiveArg.Empty
)

object Directive {
    sealed trait DirectiveArg[A]
    object DirectiveArg {
        final case class Argument[A](arg: Arg[A]) extends DirectiveArg[A]
        case object Empty extends DirectiveArg[Unit]
    }

  sealed trait Position[+F[_], A]
  object Position {
    final case class Field[F[_], A](
        enact: (
            FieldMeta,
            A,
            ast.Field[F, ?, ?]
        ) => Either[String, List[ast.Field[F, ?, ?]]]
    ) extends Position[F, A]
    final case class FragmentSpread[A](
        enact: (
            QueryMeta,
            A,
            QA.Selection.FragmentSpreadSelection[Caret]
        ) => Either[String, List[QA.Selection.FragmentSpreadSelection[Caret]]]
    ) extends Position[Nothing, A]
    final case class InlineFragmentSpread[A](
        enact: (
            QueryMeta,
            A,
            QA.Selection.FragmentSpreadSelection[Caret]
        ) => Either[String, List[QA.Selection.FragmentSpreadSelection[Caret]]]
    ) extends Position[Nothing, A]
  }
}
