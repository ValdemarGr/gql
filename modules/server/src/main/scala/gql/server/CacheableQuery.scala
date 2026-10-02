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

import gql.preparation.PreparedRoot
import io.circe.Json

final class CacheableQuery[F[_], Q, M, S] private (
    private[gql] val query: String,
    private[gql] val operationName: Option[String],
    val run: Map[String, Json] => Either[CompilationError, PreparedRoot[F, Q, M, S]]
)

object CacheableQuery {
  private[gql] def apply[F[_], Q, M, S](
      query: String,
      operationName: Option[String],
      run: Map[String, Json] => Either[CompilationError, PreparedRoot[F, Q, M, S]]
  ): CacheableQuery[F, Q, M, S] = new CacheableQuery(query, operationName, run)
}
