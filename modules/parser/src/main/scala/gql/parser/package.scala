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
package gql

import cats.parse._
import cats._
import cats.data._
import cats.implicits._

package object parser {
  def parseFor[A](str: String, p: cats.parse.Parser[A]): Either[ParseError, A] =
    (GraphqlParser.seps0.with1 *> p)
      .parseAll(str)
      .leftMap { err =>
        val offset = err.failedAtOffset
        val left = str.take(offset)
        val nls = left.split("\n")
        val line = nls.size
        val col = nls.last.size

        ParseError(
          Caret(line, col, offset),
          Eval.always(ParserUtil.errorMessage(str, err))
        )
      }

  def parseQuery(str: String): Either[ParseError, NonEmptyList[QueryAst.ExecutableDefinition[Caret]]] =
    parseFor(str, QueryParser.executableDefinition.rep)

  def parseSchema(str: String): Either[ParseError, NonEmptyList[TypeSystemAst.TypeDefinition]] =
    parseFor(
      str,
      TypeSystemParser.typeSystemDefinition.repSep(GraphqlParser.seps0)
    ).map(_.collect { case TypeSystemAst.TypeSystemDefinition.TypeDefinition(t) => t }.toNel.get)
}
