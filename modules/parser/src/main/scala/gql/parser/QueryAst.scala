package gql.parser

import cats.data._
import gql.parser.{Value => V}

object QueryAst {
  sealed trait ExecutableDefinition
  object ExecutableDefinition {
    final case class Operation(o: Pos[OperationDefinition]) extends ExecutableDefinition
    final case class Fragment(f: Pos[FragmentDefinition]) extends ExecutableDefinition
  }

  sealed trait OperationDefinition
  object OperationDefinition {
    final case class Detailed(
        tpe: OperationType,
        name: Option[String],
        variableDefinitions: Option[VariableDefinitions],
        selectionSet: SelectionSet
    ) extends OperationDefinition

    final case class Simple(selectionSet: SelectionSet) extends OperationDefinition
  }

  sealed trait OperationType
  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
  }

  final case class SelectionSet(selections: NonEmptyList[Pos[Selection]])

  sealed trait Selection
  object Selection {
    final case class FieldSelection(field: Field) extends Selection
    final case class FragmentSpreadSelection(fragmentSpread: FragmentSpread) extends Selection
    final case class InlineFragmentSelection(inlineFragment: InlineFragment) extends Selection
  }

  final case class Field(
      alias: Option[String],
      name: String,
      arguments: Option[Arguments],
      selectionSet: Pos[Option[SelectionSet]]
  )

  final case class Arguments(nel: NonEmptyList[Argument])

  final case class Argument(name: String, value: V)

  final case class FragmentSpread(fragmentName: String)

  final case class InlineFragment(typeCondition: Option[String], selectionSet: SelectionSet)

  final case class FragmentDefinition(
      name: String,
      typeCnd: String,
      selectionSet: SelectionSet
  )

  final case class VariableDefinitions(nel: NonEmptyList[Pos[VariableDefinition]])

  final case class VariableDefinition(name: String, tpe: Type, defaultValue: Option[V])

  sealed trait Type
  object Type {
    final case class Named(name: String) extends Type
    final case class List(of: Type) extends Type
    final case class NonNull(of: Type) extends Type
  }
}
