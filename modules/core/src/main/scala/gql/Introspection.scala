package gql

import cats.implicits._
import gql.dsl._
import gql.ast._

object Introspection {
  sealed trait __TypeKind
  object TypeKind {
    case object SCALAR extends __TypeKind
    case object OBJECT extends __TypeKind
    case object INTERFACE extends __TypeKind
    case object UNION extends __TypeKind
    case object ENUM extends __TypeKind
    case object INPUT_OBJECT extends __TypeKind
    case object LIST extends __TypeKind
    case object NON_NULL extends __TypeKind
  }

  final case class __Field(
      name: String,
      description: Option[String],
      args: List[__InputValue],
      `type`: __Type,
      isDeprecated: Boolean,
      deprecationReason: Option[String]
  )

  final case class __EnumValue(
      name: String,
      description: Option[String],
      isDeprecated: Boolean,
      deprecationReason: Option[String]
  )

  final case class __InputValue(
      name: String,
      description: Option[String],
      `type`: __Type,
      defaultValue: Option[String]
  )

  final case class __Type(
      kind: __TypeKind,
      name: Option[String],
      description: Option[String],
      fields: Option[List[__Field]],
      interfaces: Option[List[__Type]],
      possibleTypes: Option[List[__Type]],
      enumValues: Option[List[__EnumValue]],
      inputFields: Option[List[__InputValue]],
      ofType: Option[__Type]
  )

  final case class __Directive()

  final case class __Schema(
      types: List[__Type],
      queryType: __Type,
      mutationType: Option[__Type],
      subscriptionType: Option[__Type],
      directives: List[__Directive]
  )

  def fromSchemaShape[G[_]](ss: SchemaShape[G, _, _, _]): __Schema = {
    val d = ss.discover
    val all = d.inputs.fmap(Left(_)) ++ d.outputs.fmap(Right(_))
    ???
  }
}
