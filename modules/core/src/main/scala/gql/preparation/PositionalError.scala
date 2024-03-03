/*
 * Copyright 2024 Valdemar Grange
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
package gql.preparation

import gql.Cursor
import cats.parse.Caret
import cats.data._
import cats.implicits._

final case class PositionalError[C](position: Cursor, caret: List[C], message: String)

object PositionalError {
  import io.circe.syntax._
  import io.circe._
  implicit val encoder: Encoder.AsObject[PositionalError[Caret]] = Encoder.AsObject.instance[PositionalError[Caret]] { pe =>
    Map(
      "message" -> Some(pe.message.asJson),
      "locations" -> pe.caret.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
      "path" -> NonEmptyChain.fromChain(pe.position.path.map(_.asString)).map(_.asJson)
    ).collect { case (k, Some(v)) => k -> v }.asJsonObject
  }
}
