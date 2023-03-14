package gql.parser

import cats.data.NonEmptyList

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
    final case class EnumTypeDefinition(description: Option[String], name: String, values: NonEmptyList[EnumValueDefinition])
        extends TypeDefinition
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
      defaultValue: Option[Value[Const]]
  )

  final case class FieldDefinition(
      description: Option[String],
      name: String,
      argumentsDefinition: List[InputValueDefinition],
      tpe: Type
  )

  final case class EnumValueDefinition(description: Option[String], name: String)
}
