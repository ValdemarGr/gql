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

import cats.data.NonEmptyList
import cats.parse.Caret
import gql.parser.{QueryAst => QA}

object TypeSystemAst {
  sealed trait TypeDefinition { def name: String }
  object TypeDefinition {
    final case class ScalarTypeDefinition(
        description: Option[String],
        name: String,
        directives: Option[QA.Directives[Caret, Const]]
    ) extends TypeDefinition
    final case class ObjectTypeDefinition(
        description: Option[String],
        name: String,
        interfaces: List[String],
        directives: Option[QA.Directives[Caret, Const]],
        fieldDefinitions: NonEmptyList[FieldDefinition]
    ) extends TypeDefinition
    final case class InterfaceTypeDefinition(
        description: Option[String],
        name: String,
        interfaces: List[String],
        directives: Option[QA.Directives[Caret, Const]],
        fieldDefinitions: NonEmptyList[FieldDefinition]
    ) extends TypeDefinition
    final case class UnionTypeDefinition(
        description: Option[String],
        name: String,
        directives: Option[QA.Directives[Caret, Const]],
        types: NonEmptyList[String]
    ) extends TypeDefinition
    final case class EnumTypeDefinition(
        description: Option[String],
        name: String,
        directives: Option[QA.Directives[Caret, Const]],
        values: NonEmptyList[EnumValueDefinition]
    ) extends TypeDefinition
    final case class InputObjectTypeDefinition(
        description: Option[String],
        name: String,
        directives: Option[QA.Directives[Caret, Const]],
        inputFields: NonEmptyList[InputValueDefinition]
    ) extends TypeDefinition
  }

  sealed trait DirectiveLocation
  object DirectiveLocation {
    case object QUERY extends DirectiveLocation
    case object MUTATION extends DirectiveLocation
    case object SUBSCRIPTION extends DirectiveLocation
    case object FIELD extends DirectiveLocation
    case object FRAGMENT_DEFINITION extends DirectiveLocation
    case object FRAGMENT_SPREAD extends DirectiveLocation
    case object INLINE_FRAGMENT extends DirectiveLocation
    case object VARIABLE_DEFINITION extends DirectiveLocation
    case object SCHEMA extends DirectiveLocation
    case object SCALAR extends DirectiveLocation
    case object OBJECT extends DirectiveLocation
    case object FIELD_DEFINITION extends DirectiveLocation
    case object ARGUMENT_DEFINITION extends DirectiveLocation
    case object INTERFACE extends DirectiveLocation
    case object UNION extends DirectiveLocation
    case object ENUM extends DirectiveLocation
    case object ENUM_VALUE extends DirectiveLocation
    case object INPUT_OBJECT extends DirectiveLocation
    case object INPUT_FIELD_DEFINITION extends DirectiveLocation
  }

  final case class DirectiveDefinition(
      description: Option[String],
      name: String,
      argumentsDefinition: Option[NonEmptyList[InputValueDefinition]],
      repeatable: Boolean,
      locations: NonEmptyList[DirectiveLocation]
  )

  sealed trait TypeSystemDefinition
  object TypeSystemDefinition {
    final case class TypeDefinition(definition: TypeSystemAst.TypeDefinition) extends TypeSystemDefinition
    final case class DirectiveDefinition(definition: TypeSystemAst.DirectiveDefinition) extends TypeSystemDefinition
  }

  final case class InputValueDefinition(
      description: Option[String],
      name: String,
      tpe: Type,
      defaultValue: Option[Value[Const, Caret]],
      directives: Option[QA.Directives[Caret, Const]]
  )

  final case class FieldDefinition(
      description: Option[String],
      name: String,
      argumentsDefinition: List[InputValueDefinition],
      tpe: Type,
      directives: Option[QA.Directives[Caret, Const]]
  )

  final case class EnumValueDefinition(
      description: Option[String],
      name: String,
      directives: Option[QA.Directives[Caret, Const]]
  )
}
