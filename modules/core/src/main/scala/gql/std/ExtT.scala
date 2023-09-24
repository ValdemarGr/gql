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
package gql.std

/** A datatype to circumvent https://github.com/lampepfl/dotty/issues/14790#issuecomment-1079965993
  */
trait ExtT[F[_]] {
  type A
  def fa: F[A]
}

object ExtT {
  def liftF[F[_], A0](fa0: F[A0]): ExtT[F] = ExtTImpl(fa0)
}

final case class ExtTImpl[F[_], A0](fa: F[A0]) extends ExtT[F] {
  type A = A0
}
