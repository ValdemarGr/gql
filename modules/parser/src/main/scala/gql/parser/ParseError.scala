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

import cats.parse._
import cats._
import io.circe.syntax._
import io.circe._

final case class ParseError(
    caret: Caret,
    prettyError: Eval[String]
)

object ParseError {
  implicit val encoder: Encoder.AsObject[ParseError] = Encoder.AsObject.instance[ParseError] { err =>
    Map(
      "message" -> "could not parse query".asJson,
      "locations" -> List(Map("line" -> err.caret.line.asJson, "column" -> err.caret.col.asJson)).asJson,
      "error" -> err.prettyError.value.asJson
    ).asJsonObject
  }
}
