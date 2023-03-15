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

import io.circe._
import cats.data._
import cats.implicits._
import io.circe.syntax._

final case class QueryResult(
    data: JsonObject,
    errors: Chain[QueryResult.Error]
)

object QueryResult {
  final case class Error(
      message: String,
      path: Chain[Json]
  )

  object Error {
    implicit val encoder: Encoder.AsObject[Error] = Encoder.AsObject.instance[Error] { err =>
      Map(
        "message" -> Some(err.message.asJson),
        "path" -> NonEmptyChain.fromChain(err.path).map(_.asJson)
      ).collect { case (k, Some(v)) => k -> v }.asJsonObject
    }
  }
  implicit val encoder: Encoder.AsObject[QueryResult] = Encoder.AsObject.instance[QueryResult] { r =>
    Map(
      "data" -> Some(r.data).filter(_.nonEmpty).map(_.asJson),
      "errors" -> r.errors.toList.toNel.map(_.asJson)
    ).collect { case (k, Some(v)) => k -> v }.asJsonObject
  }
}
