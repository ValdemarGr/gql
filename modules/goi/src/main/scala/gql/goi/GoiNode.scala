/*
 * Copyright 2022 Valdemar Grange
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

abstract class GoiNode[F[_]] {
  type Value

  def typename: String

  def get(id: String): F[Option[Value]]
}

object GoiNode {
  def apply[F[_], A](typename: String, get: String => F[Option[A]]): GoiNode[F] = {
    val typename0 = typename
    val get0 = x => get(x)
    new GoiNode[F] {
      type Value = A

      def typename: String = typename0

      def get(id: String): F[Option[A]] = get0(id)
    }
  }

}
