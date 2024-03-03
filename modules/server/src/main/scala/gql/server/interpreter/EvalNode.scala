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
package gql.server.interpreter

import gql._

final case class EvalNode[F[_], +A](cursor: Cursor, value: A, scope: Scope[F]) {
  def map[B](f: A => B): EvalNode[F, B] = copy(value = f(value))

  def setValue[B](value: B): EvalNode[F, B] = copy(value = value)

  def modify(f: Cursor => Cursor): EvalNode[F, A] = copy(cursor = f(cursor))

  def succeed[B](value: B, f: Cursor => Cursor): EvalNode[F, B] =
    EvalNode(f(cursor), value, scope)

  def succeed[B](value: B): EvalNode[F, B] = succeed(value, identity)

  def setScope(scope: Scope[F]) = copy(scope = scope)
}

object EvalNode {
  def empty[F[_], A](value: A, scope: Scope[F]) = EvalNode[F, A](Cursor.empty, value, scope)
}
