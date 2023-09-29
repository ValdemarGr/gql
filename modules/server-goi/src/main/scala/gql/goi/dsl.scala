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
  final class TypeGoiOps[F[_], A](private val tpe: Type[F, A]) extends AnyVal {
    def goiFull[B](resolver: Resolver[F, A, B])(fromIds: NonEmptyList[B] => IorT[F, String, Map[B, Ior[String, A]]])(implicit
        codec: IDCodec[B],
        F: Sync[F]
    ): Type[F, A] =
      Goi.addId[F, A, B](tpe, resolver, fromIds)

    def goi[B](f: A => B)(fromIds: NonEmptyList[B] => IorT[F, String, Map[B, Ior[String, A]]])(implicit
        codec: IDCodec[B],
        F: Sync[F]
    ): Type[F, A] =
      goiFull[B](Resolver.lift[F, A](f))(fromIds)
  }

  implicit def typeGoiOps[F[_], A](tpe: Type[F, A]): TypeGoiOps[F, A] =
    new TypeGoiOps[F, A](tpe)

  final class InterfaceGoiOps[F[_], A](private val interface: Interface[F, A]) extends AnyVal {
    def goi: Interface[F, A] = Goi.addId[F, A](interface)
  }
}
