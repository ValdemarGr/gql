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
package gql.resolver

import gql.parser.{QueryAst => P}
import gql.preparation.VariableMap
import gql.parser.AnyValue
import gql._
import gql.preparation.PreparedDataField

/** Meta information about the current query.
  */
final case class QueryMeta(
    cursor: Cursor,
    variables: VariableMap[Unit]
)

/** A more specialized version of [[QueryMeta]] that also carries field specific information.
  */
final case class FieldMeta[+F[_]](
    queryMeta: QueryMeta,
    args: Option[P.Arguments[Unit, AnyValue]],
    astNode: PreparedDataField[F, ?],
    parsedArgs: Map[Arg[?], Any]
) {
  // :(
  def arg[A](a: Arg[A]): Option[A] =
    parsedArgs.get(a).asInstanceOf[Option[A]]

  def alias: Option[String] = astNode.alias
}
