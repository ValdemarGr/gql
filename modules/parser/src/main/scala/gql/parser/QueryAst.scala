/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

  final case class Argument(name: String, value: V[AnyValue])

  final case class FragmentSpread(fragmentName: String)

  final case class InlineFragment(typeCondition: Option[String], selectionSet: SelectionSet)

  final case class FragmentDefinition(
      name: String,
      typeCnd: String,
      selectionSet: SelectionSet
  )

  final case class VariableDefinitions(nel: NonEmptyList[Pos[VariableDefinition]])

  final case class VariableDefinition(name: String, tpe: Type, defaultValue: Option[V[gql.parser.Const]])
}
