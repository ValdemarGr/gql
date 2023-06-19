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
package gql.dsl

trait GqlDslFull
    extends DirectiveDslFull
    with EnumDslFull
    with FieldDslFull
    with InputDslFull
    with InterfaceDslFull
    with TypeDslFull
    with UnionDslFull

trait GqlDsl[F[_]]
    extends DirectiveDsl[F]
    with EnumDslFull
    with FieldDsl[F]
    with InputDslFull
    with InterfaceDsl[F]
    with TypeDsl[F]
    with UnionDsl[F]

object GqlDsl extends GqlDslFull {
  def apply[F[_]]: GqlDsl[F] = new GqlDsl[F] {}
}
