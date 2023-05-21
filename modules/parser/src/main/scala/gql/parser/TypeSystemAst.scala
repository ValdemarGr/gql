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

object TypeSystemAst {
  sealed trait TypeDefinition { def name: String }
  object TypeDefinition {
    final case class ScalarTypeDefinition(description: Option[String], name: String) extends TypeDefinition
    final case class ObjectTypeDefinition(
        description: Option[String],
        name: String,
        interfaces: List[String],
        fieldDefinitions: NonEmptyList[FieldDefinition]
    ) extends TypeDefinition
    final case class InterfaceTypeDefinition(
        description: Option[String],
        name: String,
        interfaces: List[String],
        fieldDefinitions: NonEmptyList[FieldDefinition]
    ) extends TypeDefinition
    final case class UnionTypeDefinition(description: Option[String], name: String, types: NonEmptyList[String]) extends TypeDefinition
    final case class EnumTypeDefinition(
        description: Option[String],
        name: String,
        values: NonEmptyList[EnumValueDefinition]
    ) extends TypeDefinition
    final case class InputObjectTypeDefinition(
        description: Option[String],
        name: String,
        inputFields: NonEmptyList[InputValueDefinition]
    ) extends TypeDefinition
  }

  final case class InputValueDefinition(
      description: Option[String],
      name: String,
      tpe: Type,
      defaultValue: Option[Value[Const, Caret]]
  )

  final case class FieldDefinition(
      description: Option[String],
      name: String,
      argumentsDefinition: List[InputValueDefinition],
      tpe: Type
  )

  final case class EnumValueDefinition(description: Option[String], name: String)
}
