/*
 * Copyright 2024 Valdemar Grange
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
package gql.parser

import cats.parse.{Parser => P, Caret, Parser0 => P0}

final case class Pos[+A](caret: Caret, value: A)

object Pos {
  def pos[A](p: P[A]): P[Pos[A]] =
    (P.caret.with1 ~ p).map { case (c, a) => Pos(c, a) }

  def pos0[A](p: P0[A]): P0[Pos[A]] =
    (P.caret ~ p).map { case (c, a) => Pos(c, a) }
}
