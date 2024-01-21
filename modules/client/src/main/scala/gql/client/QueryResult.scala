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

import io.circe._

final case class QueryResult[A](
    data: Decoder.Result[A],
    errors: Option[List[QueryResult.Error]]
)

object QueryResult {
  final case class Error(
      mesage: String,
      path: List[String],
      original: JsonObject
  )

  object Error {
    implicit val decoder: Decoder[Error] = Decoder.instance[Error] { c =>
      for {
        message <- c.downField("message").as[String]
        path <- c.downField("path").as[List[String]]
        original <- c.as[JsonObject]
      } yield Error(message, path, original)
    }
  }

  implicit def decoder[A: Decoder]: Decoder[QueryResult[A]] = Decoder.instance[QueryResult[A]] { c =>
    c
      .downField("errors")
      .as[Option[List[Error]]]
      .map(errs => QueryResult(c.downField("data").as[A], errs))
  }
}
