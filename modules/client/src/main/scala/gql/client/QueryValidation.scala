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
package gql.client

import gql.parser.TypeSystemAst._
import cats.implicits._
import cats.data._
import gql._
import gql.preparation.RootPreparation
import io.circe.Json
import gql.parser.ParserUtil
import gql.parser.{Type => PType}
import io.circe.JsonObject
import io.circe.syntax._
import gql.parser.QueryAst
import cats.parse.Caret
import gql.util.SchemaUtil

object QueryValidation {
  def generateStubInput(
      tpe: PType,
      ast: Map[String, TypeDefinition]
  ): ValidatedNec[String, Json] = {
    val ms = ModifierStack.fromType(tpe).invert
    // Any modifer has a monoid
    ms.modifiers.headOption match {
      case Some(InverseModifier.List)     => Json.arr().validNec
      case Some(InverseModifier.Optional) => Json.Null.validNec
      case None =>
        ast.get(ms.inner) match {
          case None =>
            ms.inner match {
              case "Int" | "Float" | "String" | "Boolean" | "ID" => Json.fromString("").validNec
              case _                                             => s"Could not find type ${ms.inner}".invalidNec
            }
          case Some(_: TypeDefinition.ScalarTypeDefinition) => Json.fromString("").validNec
          case Some(e: TypeDefinition.EnumTypeDefinition)   => Json.fromString(e.values.head.name).validNec
          case Some(i: TypeDefinition.InputObjectTypeDefinition) =>
            i.inputFields
              .traverse { x =>
                x.defaultValue match {
                  case Some(_) => JsonObject.empty.validNec
                  case None    => generateStubInput(x.tpe, ast).map(v => JsonObject.singleton(x.name, v))
                }
              }
              .map(_.reduceLeft(_ deepMerge _).asJson)
          case _ => s"Type ${ms.inner} is not an input type".invalidNec
        }
    }
  }

  def generateVariableStub[C](
      vd: QueryAst.VariableDefinition[C],
      ast: Map[String, TypeDefinition]
  ): ValidatedNec[String, Map[String, Json]] = {
    val fa = vd.defaultValue match {
      case Some(_) => JsonObject.empty.validNec
      case None    => generateStubInput(vd.tpe, ast).map(v => JsonObject.singleton(vd.name, v))
    }
    fa.map(_.toMap)
  }

  def validateExecutables(
      originQuery: String,
      executables: NonEmptyList[QueryAst.ExecutableDefinition[Caret]],
      ast: Map[String, TypeDefinition]
  ): List[String] =
    SchemaUtil
      .stubSchema(ast)
      .flatMap { x =>
        val vs = executables
          .collect { case QueryAst.ExecutableDefinition.Operation(op, _) => op }
          .collect { case QueryAst.OperationDefinition.Detailed(_, _, vds, _, _) => vds.toList.flatMap(_.nel.toList) }
          .flatten
          .traverse(generateVariableStub(_, ast))

        vs.toEither.map(_.foldLeft(Map.empty[String, Json])(_ ++ _)).flatMap { variables =>
          RootPreparation
            .prepareRun(executables, x, variables, None)
            .leftMap(_.map { pe =>
              val pos = pe.position.formatted
              val caretErrs = pe.caret.distinct
                .map(c => ParserUtil.showVirtualTextLine(originQuery, c.offset))
                .map { case (msg, _, _) => msg }
              val msg = pe.message
              s"$msg at $pos\n${caretErrs.mkString_("\n")}"
            })
        }
      }
      .left
      .toOption
      .toList
      .flatMap(_.toList)

  def validateQuery(
      q: String,
      ast: Map[String, TypeDefinition]
  ): List[String] =
    gql.parser
      .parseQuery(q)
      .leftMap(err => NonEmptyChain.one(err.prettyError.value))
      .map(validateExecutables(q, _, ast))
      .left
      .toOption
      .toList
      .flatMap(_.toList)
}
