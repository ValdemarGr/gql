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
  sealed trait ExecutableDefinition[P[_]]
  object ExecutableDefinition {
    final case class Operation[P[_]](o: P[OperationDefinition[P]]) extends ExecutableDefinition[P]
    final case class Fragment[P[_]](f: P[FragmentDefinition[P]]) extends ExecutableDefinition[P]
  }

  sealed trait OperationDefinition[P[_]]
  object OperationDefinition {
    final case class Detailed[P[_]](
        tpe: OperationType,
        name: Option[String],
        variableDefinitions: Option[VariableDefinitions[P]],
        selectionSet: SelectionSet[P]
    ) extends OperationDefinition[P]

    final case class Simple[P[_]](selectionSet: SelectionSet[P]) extends OperationDefinition[P]
  }

  sealed trait OperationType
  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
  }

  final case class SelectionSet[P[_]](selections: NonEmptyList[P[Selection[P]]])

  sealed trait Selection[P[_]]
  object Selection {
    final case class FieldSelection[P[_]](field: Field[P]) extends Selection[P]
    final case class FragmentSpreadSelection[P[_]](fragmentSpread: FragmentSpread) extends Selection[P]
    final case class InlineFragmentSelection[P[_]](inlineFragment: InlineFragment[P]) extends Selection[P]
  }

  final case class Field[P[_]](
      alias: Option[String],
      name: String,
      arguments: Option[Arguments],
      selectionSet: P[Option[SelectionSet[P]]]
  )

  final case class Arguments(nel: NonEmptyList[Argument])

  final case class Argument(name: String, value: V[AnyValue])

  final case class FragmentSpread(fragmentName: String)

  final case class InlineFragment[P[_]](typeCondition: Option[String], selectionSet: SelectionSet[P])

  final case class FragmentDefinition[P[_]](
      name: String,
      typeCnd: String,
      selectionSet: SelectionSet[P]
  )

  final case class VariableDefinitions[P[_]](nel: NonEmptyList[P[VariableDefinition]])

  final case class VariableDefinition(name: String, tpe: Type, defaultValue: Option[V[gql.parser.Const]])
}
