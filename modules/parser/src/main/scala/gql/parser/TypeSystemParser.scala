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

import cats.parse.{Parser => P}
import cats.implicits._
import cats.data.NonEmptyList

// https://spec.graphql.org/June2018/#sec-Source-Text
object TypeSystemParser {
  import TypeSystemAst._
  import GraphqlParser._

  val badNames = Set("true", "false", "null")
  lazy val enumValueDefinition: P[EnumValueDefinition] =
    (stringValue.?.with1 ~ name.filterNot(badNames.contains) ~ directivesConst.?).map { case ((d, n), ds) => EnumValueDefinition(d, n, ds) }

  lazy val enumTypeDefinition: P[TypeDefinition.EnumTypeDefinition] =
    ((stringValue.?.with1.soft <* s("enum")) ~ name ~ directivesConst.? ~ enumValueDefinition.rep.between(t('{'), t('}'))).map {
      case (((d, n), ds), vs) =>
        TypeDefinition.EnumTypeDefinition(d, n, ds, vs)
    }

  lazy val scalarTypeDefinition: P[TypeDefinition.ScalarTypeDefinition] =
    ((stringValue.?.with1.soft <* s("scalar")) ~ name ~ directivesConst.?).map { case ((d, n), ds) =>
      TypeDefinition.ScalarTypeDefinition(d, n, ds)
    }

  lazy val inputValueDefinition: P[InputValueDefinition] =
    (stringValue.?.with1 ~ name ~ (t(':') *> `type`) ~ defaultValue(constValue).? ~ directivesConst.?)
      .map { case ((((d, n), t), dv), ds) => InputValueDefinition(d, n, t, dv, ds) }

  lazy val fieldDefinition: P[FieldDefinition] =
    (stringValue.?.with1 ~ name ~ inputValueDefinition.rep.between(t('('), t(')')).? ~ (t(':') *> `type`) ~ directivesConst.?)
      .map { case ((((d, n), args), t), ds) => FieldDefinition(d, n, args.map(_.toList).getOrElse(Nil), t, ds) }

  lazy val implementsInterface: P[NonEmptyList[String]] =
    s("implements") *> name.repSep(t('&'))

  lazy val objectTypeDefinition: P[TypeDefinition.ObjectTypeDefinition] =
    ((stringValue.?.with1.soft <* s("type")) ~ name ~ implementsInterface.? ~ directivesConst.? ~
      fieldDefinition.rep.between(t('{'), t('}')))
      .map { case ((((d, n), i), ds), fs) => TypeDefinition.ObjectTypeDefinition(d, n, i.map(_.toList).getOrElse(Nil), ds, fs) }

  lazy val interfaceTypeDefinition: P[TypeDefinition.InterfaceTypeDefinition] =
    ((stringValue.?.with1.soft <* s("interface")) ~ name ~ implementsInterface.? ~ directivesConst.? ~
      fieldDefinition.rep.between(t('{'), t('}')))
      .map { case ((((d, n), i), ds), fs) => TypeDefinition.InterfaceTypeDefinition(d, n, i.map(_.toList).getOrElse(Nil), ds, fs) }

  lazy val unionTypeDefinition: P[TypeDefinition.UnionTypeDefinition] =
    ((stringValue.?.with1.soft <* s("union")) ~ name ~ directivesConst.? ~ (s("=") *> t('|').? *> name.repSep(t('|'))))
      .map { case (((d, n), ds), m) => TypeDefinition.UnionTypeDefinition(d, n, ds, m) }

  lazy val inputObjectTypeDefinition: P[TypeDefinition.InputObjectTypeDefinition] =
    ((stringValue.?.with1.soft <* s("input")) ~ name ~ directivesConst.? ~ inputValueDefinition.rep.between(t('{'), t('}')))
      .map { case (((d, n), ds), fs) => TypeDefinition.InputObjectTypeDefinition(d, n, ds, fs) }

  lazy val typeDefinition: P[TypeDefinition] =
    scalarTypeDefinition |
      objectTypeDefinition |
      interfaceTypeDefinition |
      unionTypeDefinition |
      enumTypeDefinition |
      inputObjectTypeDefinition

  lazy val directiveLocation = P.oneOf(
    List(
      s("FIELD_DEFINITION").as(DirectiveLocation.FIELD_DEFINITION),
      s("ENUM_VALUE").as(DirectiveLocation.ENUM_VALUE),
      s("QUERY").as(DirectiveLocation.QUERY),
      s("MUTATION").as(DirectiveLocation.MUTATION),
      s("SUBSCRIPTION").as(DirectiveLocation.SUBSCRIPTION),
      s("FIELD").as(DirectiveLocation.FIELD),
      s("FRAGMENT_DEFINITION").as(DirectiveLocation.FRAGMENT_DEFINITION),
      s("FRAGMENT_SPREAD").as(DirectiveLocation.FRAGMENT_SPREAD),
      s("INLINE_FRAGMENT").as(DirectiveLocation.INLINE_FRAGMENT),
      s("VARIABLE_DEFINITION").as(DirectiveLocation.SCHEMA),
      s("SCHEMA").as(DirectiveLocation.SCHEMA),
      s("SCALAR").as(DirectiveLocation.SCALAR),
      s("OBJECT").as(DirectiveLocation.OBJECT),
      s("ARGUMENT_DEFINITION").as(DirectiveLocation.ARGUMENT_DEFINITION),
      s("INTERFACE").as(DirectiveLocation.INTERFACE),
      s("UNION").as(DirectiveLocation.UNION),
      s("ENUM").as(DirectiveLocation.ENUM),
      s("INPUT_OBJECT").as(DirectiveLocation.INPUT_OBJECT),
      s("INPUT_FIELD_DEFINITION").as(DirectiveLocation.INPUT_FIELD_DEFINITION)
    )
  )

  lazy val directiveLocations: P[NonEmptyList[DirectiveLocation]] =
    t('|').?.with1 *> directiveLocation.repSep(t('|'))

  lazy val directiveDefinition = {
    ((stringValue.?.with1.soft <* s("directive")) ~ (t('@') *> name) ~
      inputValueDefinition.rep.between(t('('), t(')')).? ~
      (s("repeatable").?.map(_.isDefined) <* s("on")) ~ directiveLocations)
      .map { case ((((d, n), iv), rep), dl) => DirectiveDefinition(d, n, iv, rep, dl) }
  }

  lazy val typeSystemDefinition: P[TypeSystemDefinition] =
    directiveDefinition.map(TypeSystemDefinition.DirectiveDefinition(_)) |
      typeDefinition.map(TypeSystemDefinition.TypeDefinition(_))
}
