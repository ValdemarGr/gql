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
import io.circe._

package object parser {
  final case class ParseError(
      caret: Caret,
      prettyError: Eval[String]
  ) {
    lazy val asGraphQL = {
      import io.circe.syntax._
      JsonObject(
        "message" -> "could not parse query".asJson,
        "locations" -> Json.arr(Json.obj("line" -> caret.line.asJson, "column" -> caret.col.asJson)),
        "error" -> prettyError.value.asJson
      )
    }
  }

  def parse(str: String): Either[ParseError, NonEmptyList[QueryParser.ExecutableDefinition]] = {
    QueryParser.executableDefinition.rep
      .parseAll(str)
      .leftMap { err =>
        val offset = err.failedAtOffset
        val left = str.take(offset)
        val nls = left.split("\n")
        val line = nls.size
        val col = nls.last.size

        ParseError(
          Caret(line, col, offset),
          Eval.later(ParserUtil.errorMessage(str, err))
        )
      }
  }
}
