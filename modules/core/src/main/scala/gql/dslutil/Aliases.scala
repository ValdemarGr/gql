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
package gql.dslutil

import cats.data._
import gql.ast._

trait Aliases {
  type Fields[F[_], -A] = NonEmptyList[(String, Field[F, A, ?])]

  type AbstractFields[F[_]] = NonEmptyList[(String, AbstractField[F, ?])]

  type AnyFields[F[_], -A] = NonEmptyList[(String, AnyField[F, A, ?])]
}

object Aliases extends Aliases
