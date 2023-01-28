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
package gql.goi

import gql.resolver.Resolver

trait GlobalID[F[_], T, K] {
  type Key = K

  def typename: String

  def codec: IDCodec[Key]

  def toId: Resolver[F, T, Key]

  def fromId: Key => F[Option[T]]
}

object GlobalID {
  def apply[F[_], T, A](typename: String, toId: Resolver[F, T, A], fromId: A => F[Option[T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] = {
    val typename0 = typename
    val toId0 = toId
    val fromId0 = fromId(_)
    val codec0 = codec
    new GlobalID[F, T, A] {
      override def typename: String = typename0

      override def codec: IDCodec[A] = codec0

      override def toId: Resolver[F, T, A] = toId0

      override def fromId: A => F[Option[T]] = fromId0
    }
  }
}
