package gql.parser

import cats.parse.{Parser => P}
import cats.implicits._
import cats.data.NonEmptyList

// https://spec.graphql.org/June2018/#sec-Source-Text
object TypeSystemParser {
  import TypeSystemAst._
  import GraphqlParser._

  val badNames = Set("true", "false", "null")
  lazy val enumValueDefinition: P[EnumValueDefinition] =
    (stringValue.?.with1 ~ name.filterNot(badNames.contains)).map { case (d, n) => EnumValueDefinition(d, n) }

  lazy val enumTypeDefinition: P[TypeDefinition.EnumTypeDefinition] =
    ((stringValue.?.with1.soft <* s("enum")) ~ name ~ enumValueDefinition.rep.between(t('{'), t('}'))).map { case ((d, n), vs) =>
      TypeDefinition.EnumTypeDefinition(d, n, vs)
    }

  lazy val scalarTypeDefinition: P[TypeDefinition.ScalarTypeDefinition] =
    ((stringValue.?.with1.soft <* s("scalar")) ~ name).map { case (d, n) => TypeDefinition.ScalarTypeDefinition(d, n) }

  lazy val inputValueDefinition: P[InputValueDefinition] =
    (stringValue.?.with1 ~ name ~ (t(':') *> `type`) ~ defaultValue(constValue).?)
      .map { case (((d, n), t), dv) => InputValueDefinition(d, n, t, dv) }

  lazy val fieldDefinition: P[FieldDefinition] =
    (stringValue.?.with1 ~ name ~ inputValueDefinition.rep.between(t('('), t(')')).? ~ (t(':') *> `type`))
      .map { case (((d, n), args), t) => FieldDefinition(d, n, args.map(_.toList).getOrElse(Nil), t) }

  lazy val implementsInterface: P[NonEmptyList[String]] =
    s("implements") *> name.repSep(t('&'))

  lazy val objectTypeDefinition: P[TypeDefinition.ObjectTypeDefinition] =
    ((stringValue.?.with1.soft <* s("type")) ~ name ~ implementsInterface.? ~ fieldDefinition.rep.between(t('{'), t('}')))
      .map { case (((d, n), i), fs) => TypeDefinition.ObjectTypeDefinition(d, n, i.map(_.toList).getOrElse(Nil), fs) }

  lazy val interfaceTypeDefinition: P[TypeDefinition.InterfaceTypeDefinition] =
    ((stringValue.?.with1.soft <* s("interface")) ~ name ~ implementsInterface.? ~ fieldDefinition.rep.between(t('{'), t('}')))
      .map { case (((d, n), i), fs) => TypeDefinition.InterfaceTypeDefinition(d, n, i.map(_.toList).getOrElse(Nil), fs) }

  lazy val unionTypeDefinition: P[TypeDefinition.UnionTypeDefinition] =
    ((stringValue.?.with1.soft <* s("union")) ~ name ~ (s("=") *> name.repSep(t('|'))))
      .map { case ((d, n), m) => TypeDefinition.UnionTypeDefinition(d, n, m) }

  lazy val inputObjectTypeDefinition: P[TypeDefinition.InputObjectTypeDefinition] =
    ((stringValue.?.with1.soft <* s("input")) ~ name ~ inputValueDefinition.rep.between(t('{'), t('}')))
      .map { case ((d, n), fs) => TypeDefinition.InputObjectTypeDefinition(d, n, fs) }

  lazy val typeDefinition: P[TypeDefinition] =
    scalarTypeDefinition |
      objectTypeDefinition |
      interfaceTypeDefinition |
      unionTypeDefinition |
      enumTypeDefinition |
      inputObjectTypeDefinition
}
