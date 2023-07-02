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
import cats.data._
import cats.implicits._
import cats._

trait GlobalID[F[_], T, K] {
  def typename: String

  def codec: IDCodec[K]

  def fromIds: NonEmptyList[K] => F[Map[K, T]]
}

object GlobalID {
  def apply[F[_], T, A](typename: String, fromIds: NonEmptyList[A] => F[Map[A, T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] = {
    val typename0 = typename
    val fromIds0 = fromIds(_)
    val codec0 = codec
    new GlobalID[F, T, A] {
      override def typename: String = typename0

      override def codec: IDCodec[A] = codec0

      override def fromIds: NonEmptyList[A] => F[Map[A, T]] = fromIds0
    }
  }

  def unary[F[_]: Applicative, T, A](typename: String, fromId: A => F[Option[T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] =
    GlobalID(
      typename,
      _.toList.traverse(k => fromId(k) tupleLeft k).map(_.collect { case (k, Some(v)) => k -> v }.toMap)
    )
}
