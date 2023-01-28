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

object dsl {
  implicit class InterfaceGOIOps[F[_], A](val i: Interface[F, A]) {
    def goi: Interface[F, A] = Goi.addId[F, A](i)
  }

  implicit class GlobalIDOps[F[_], A, K](val gid: GlobalID[F, A, K]) extends AnyVal {
    def tpe(
        hd: (String, Field[F, A, ?]),
        tl: (String, Field[F, A, ?])*
    )(implicit F: Sync[F]) = {
      implicit val codec = gid.codec
      Goi.addId[F, A, K](gid.toId, gql.dsl.tpe[F, A](gid.typename, hd, tl: _*))
    }
  }

  def gid[F[_], T, A](typename: String, toId: T => A, fromId: A => F[Option[T]])(implicit codec: IDCodec[A]): GlobalID[F, T, A] =
    GlobalID(typename, Resolver.lift(toId), fromId)

  def gidFrom[F[_], T, A](typename: String, toId: Resolver[F, T, A], fromId: A => F[Option[T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] =
    GlobalID(typename, toId, fromId)
}
