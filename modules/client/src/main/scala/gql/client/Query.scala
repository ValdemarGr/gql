package gql.client

import cats.data._
import gql.parser.{QueryParser => P}
import io.circe._
import org.typelevel.paiges._
import cats.implicits._
import cats._
import gql.client.Selection.Fragment
import gql.client.Selection.InlineFragment

final case class SimpleQuery[A](
    operationType: P.OperationType,
    selectionSet: SelectionSet[A]
) {
  def compile: Query.Compiled[A] = ???
}

final case class NamedQuery[A](name: String, query: SimpleQuery[A]) {
  def compile: Query.Compiled[A] = ???
}

final case class ParameterizedQuery[A, V](
    name: String,
    query: SimpleQuery[A],
    variables: Var.Impl[V]
) {
  def compile(variables: V): Query.Compiled[A] = ???
}

object Query {
  final case class Compiled[A](
      decoder: A => EitherNec[String, A],
      query: String,
      variables: Option[Json]
  )

  object Render {
    def renderOperationType(op: P.OperationType): Doc =
      op match {
        case P.OperationType.Query        => Doc.text("query")
        case P.OperationType.Mutation     => Doc.text("mutation")
        case P.OperationType.Subscription => Doc.text("subscription")
      }

    def renderValue(v: P.Value): Doc = {
      import P.Value._
      v match {
        case IntValue(v)     => Doc.text(v.toString)
        case StringValue(v)  => Doc.text(s""""$v"""")
        case FloatValue(v)   => Doc.text(v.toString)
        case NullValue       => Doc.text("null")
        case BooleanValue(v) => Doc.text(v.toString)
        case ListValue(v) =>
          Doc.intercalate(Doc.comma + Doc.line, v.map(renderValue)).tightBracketBy(Doc.char('['), Doc.char(']'))
        case ObjectValue(fields) =>
          Doc
            .intercalate(
              Doc.comma + Doc.line,
              fields.map { case (k, v) => Doc.text(k) + Doc.text(": ") + renderValue(v) }
            )
            .bracketBy(Doc.char('{'), Doc.char('}'))
        case EnumValue(v)     => Doc.text(v)
        case VariableValue(v) => Doc.text(s"$$${v}")
      }
    }

    def renderVar(v: Var.One[?]): Doc = {
      val default = v.default match {
        case None          => Doc.empty
        case Some(default) => Doc.space + Doc.char('=') + Doc.space + renderValue(default)
      }
      Doc.text(s"$$${v.name}") + Doc.space + Doc.char(':') + Doc.space + Doc.text(v.tpe) + default
    }

    def renderArg(a: Arg): Doc = 
        Doc.text(a.name) + Doc.char(':') + Doc.space + renderValue(a.value)

    def renderSelectionSet(ss: SelectionSet[?]): Doc = {
      type C[A] = Const[List[Doc], A]
      val compiler = new (Selection ~> C) {
        override def apply[A](fa: Selection[A]): C[A] = Const[List[Doc], A] {
          List {
            fa match {
              // Fragments are floated to the top level and handled separately
              case _: Fragment[?] => Doc.empty
              case InlineFragment(on, _, subSelection) =>
                Doc.text(s"...$on") + Doc.space + renderSelectionSet(subSelection)
              case Selection.Field(name, alias, args, sq) =>
                val aliased = alias match {
                  case None    => Doc.empty
                  case Some(a) => Doc.text(a) + Doc.char(':') + Doc.space
                }

                val lhs = aliased + Doc.text(name)

                val argsDoc = args.toNel match {
                  case None => Doc.empty
                  case Some(args) =>
                    Doc.intercalate(Doc.comma + Doc.line, args.map(renderArg).toList).bracketBy(Doc.char('('), Doc.char(')'))
                }

                val rhs = sq match {
                  case Terminal(_)         => Doc.empty
                  case ss: SelectionSet[?] => Doc.space + renderSelectionSet(ss)
                }

                lhs + argsDoc + rhs
            }
          }
        }
      }
      val docs = ss.impl.underlying.foldMap(compiler)
      Doc.intercalate(Doc.comma + Doc.line, docs.getConst).bracketBy(Doc.char('{'), Doc.char('}'))
    }
  }
}
