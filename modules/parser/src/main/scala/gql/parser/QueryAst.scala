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
  sealed trait ExecutableDefinition[C] {
    def map[A](f: C => A): ExecutableDefinition[A]
  }
  object ExecutableDefinition {
    final case class Operation[C](o: OperationDefinition[C], c: C) extends ExecutableDefinition[C] {
      def map[A](f: C => A): ExecutableDefinition[A] = Operation(o.map(f), f(c))
    }
    final case class Fragment[C](f: FragmentDefinition[C], c: C) extends ExecutableDefinition[C] {
      def map[A](g: C => A): ExecutableDefinition[A] = Fragment(f.map(g), g(c))
    }
  }

  sealed trait OperationDefinition[C] {
    def map[A](f: C => A): OperationDefinition[A]
  }
  object OperationDefinition {
    final case class Detailed[C](
        tpe: OperationType,
        name: Option[String],
        variableDefinitions: Option[VariableDefinitions[C]],
        selectionSet: SelectionSet[C]
    ) extends OperationDefinition[C] {
      def map[A](f: C => A): OperationDefinition[A] =
        Detailed(tpe, name, variableDefinitions.map(_.map(f)), selectionSet.map(f))
    }

    final case class Simple[C](selectionSet: SelectionSet[C]) extends OperationDefinition[C] {
      def map[A](f: C => A): OperationDefinition[A] = Simple(selectionSet.map(f))
    }
  }

  sealed trait OperationType
  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
  }

  final case class SelectionSet[C](selections: NonEmptyList[Selection[C]]) {
    def map[A](f: C => A): SelectionSet[A] = SelectionSet(selections.map(_.map(f)))
  }

  sealed trait Selection[C] {
    def map[A](f: C => A): Selection[A]
  }
  object Selection {
    final case class FieldSelection[C](field: Field[C], c: C) extends Selection[C] {
      def map[A](f: C => A): Selection[A] = FieldSelection(field.map(f), f(c))
    }
    final case class FragmentSpreadSelection[C](fragmentSpread: FragmentSpread, c: C) extends Selection[C] {
      def map[A](f: C => A): Selection[A] = FragmentSpreadSelection(fragmentSpread, f(c))
    }
    final case class InlineFragmentSelection[C](inlineFragment: InlineFragment[C], c: C) extends Selection[C] {
      def map[A](f: C => A): Selection[A] = InlineFragmentSelection(inlineFragment.map(f), f(c))
    }
  }

  final case class Field[C](
      alias: Option[String],
      name: String,
      arguments: Option[Arguments[C]],
      selectionSet: Option[SelectionSet[C]],
      caret: C
  ) {
    def map[A](f: C => A): Field[A] =
      Field(
        alias,
        name,
        arguments.map(_.map(f)),
        selectionSet.map(_.map(f)),
        f(caret)
      )
  }

  final case class Arguments[C](nel: NonEmptyList[Argument[C]]) {
    def map[A](f: C => A): Arguments[A] = Arguments(nel.map(_.map(f)))
  }

  final case class Argument[C](name: String, value: V[AnyValue, C]) {
    def map[A](f: C => A): Argument[A] = Argument(name, value.map(f))
  }

  final case class FragmentSpread(fragmentName: String)

  final case class InlineFragment[C](typeCondition: Option[String], selectionSet: SelectionSet[C]) {
    def map[A](f: C => A): InlineFragment[A] = InlineFragment(typeCondition, selectionSet.map(f))
  }

  final case class FragmentDefinition[C](
      name: String,
      typeCnd: String,
      selectionSet: SelectionSet[C],
      caret: C
  ) {
    def map[A](f: C => A): FragmentDefinition[A] =
      FragmentDefinition(name, typeCnd, selectionSet.map(f), f(caret))
  }

  final case class VariableDefinitions[C](nel: NonEmptyList[VariableDefinition[C]]) {
    def map[A](f: C => A): VariableDefinitions[A] =
      VariableDefinitions(nel.map(_.map(f)))
  }

  final case class VariableDefinition[C](name: String, tpe: Type, defaultValue: Option[V[gql.parser.Const, C]], c: C) {
    def map[A](f: C => A): VariableDefinition[A] =
      VariableDefinition(name, tpe, defaultValue.map(_.map(f)), f(c))
  }
}
