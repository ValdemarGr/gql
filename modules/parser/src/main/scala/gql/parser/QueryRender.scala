package gql.parser

import org.typelevel.paiges._
import cats.data._

object QueryRender {
  import QueryAst._
  /*
  def renderOperationDefinition(op: OperationDefinition): Doc = op match {
    case OperationDefinition.Simple(ss) =>
      renderSelectionSet(ss.selections.map(_.value))
    case OperationDefinition.Detailed(d, name, v, ss) =>
      renderOperationType(d) + Doc.space +
        name.map(Doc.text(_) + Doc.space).getOrElse(Doc.empty) +
        v.map(xs => renderVariableDefinitions(xs.nel.map(_.value)) + Doc.space).getOrElse(Doc.empty) +
        Doc.empty
  }

  def renderOperationType(op: OperationType): Doc = op match {
    case OperationType.Query        => Doc.text("query")
    case OperationType.Mutation     => Doc.text("mutation")
    case OperationType.Subscription => Doc.text("subscription")
  }

  def renderVariableDefinitions(vds: NonEmptyList[VariableDefinition]): Doc = {
    Doc
      .intercalate(
        Doc.comma + Doc.line,
        vds.toList.map { vd =>
          val default = vd.defaultValue match {
            case None          => Doc.empty
            case Some(default) => Doc.space + Doc.char('=') + Doc.space + GraphqlRender.renderValue(default)
          }

          Doc.text(s"$$${vd.name}") + Doc.space + Doc.char(':') + Doc.space +
            GraphqlRender.renderType(vd.tpe) + default
        }
      )
      .bracketBy(Doc.char('('), Doc.char(')'))

  }

  def renderArg(a: Argument): Doc =
    Doc.text(a.name) + Doc.char(':') + Doc.space + GraphqlRender.renderValue(a.value)

  def renderSelectionSet(ss: NonEmptyList[Selection]): Doc = {
    Doc
      .intercalate(
        Doc.comma + Doc.line,
        ss.map {
          case Selection.FieldSelection(f)          => renderField(f)
          case Selection.FragmentSpreadSelection(f) => Doc.text(s"...${f.fragmentName}")
          case Selection.InlineFragmentSelection(f) =>
            Doc.text(s"...${f.typeCondition.get}") + Doc.space +
              renderSelectionSet(f.selectionSet.selections.map(_.value))
        }.toList
      )
      .bracketBy(Doc.char('{'), Doc.char('}'))
  }

  def renderFragment(frag: FragmentDefinition): Doc =
    Doc.text("fragment") + Doc.space +
      Doc.text(frag.name) + Doc.space +
      Doc.text("on") + Doc.space + Doc.text(frag.typeCnd) + Doc.space +
      renderSelectionSet(frag.selectionSet.selections.map(_.value))

  def renderField(f: Field): Doc = {
    val aliased = f.alias match {
      case None    => Doc.empty
      case Some(a) => Doc.text(a) + Doc.char(':') + Doc.space
    }

    val lhs = aliased + Doc.text(f.name)

    val argsDoc = f.arguments match {
      case None => Doc.empty
      case Some(args) =>
        val xs = args.nel
        Doc.intercalate(Doc.comma + Doc.line, xs.map(renderArg).toList).bracketBy(Doc.char('('), Doc.char(')'))
    }

    val rhs = f.selectionSet.value match {
      case None => Doc.empty
      case Some(ss) =>
        Doc.space + renderSelectionSet(ss.selections.map(_.value))
    }

    lhs + argsDoc + rhs
  }*/
}
