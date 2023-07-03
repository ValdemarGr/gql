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

import gql.ast._
import cats.effect._
import gql.resolver._
import cats.data._

object dsl {
  def goiFull[F[_]: Sync, A, B](tpe: Type[F, A])(resolver: Resolver[F, A, B])(
      fromIds: NonEmptyList[B] => F[Map[B, A]]
  )(implicit codec: IDCodec[B]): Type[F, A] =
    Goi.addId[F, A, B](tpe, resolver, fromIds)

  def goi[F[_]: Sync, A, B](tpe: Type[F, A])(f: A => B)(
      fromIds: NonEmptyList[B] => F[Map[B, A]]
  )(implicit codec: IDCodec[B]): Type[F, A] =
    goiFull[F, A, B](tpe)(Resolver.lift[F, A](f))(fromIds)

  def goi[F[_], A](interface: Interface[F, A]): Interface[F, A] =
    Goi.addId[F, A](interface)
}
