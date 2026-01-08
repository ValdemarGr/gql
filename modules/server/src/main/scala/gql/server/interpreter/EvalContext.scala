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
package gql.server.interpreter

import gql.Cursor

final case class EvalNode[F[_], +A](
    cursor: Cursor,
    value: A,
    active: Res[F]
) {
  def map[B](f: A => B): EvalNode[F, B] = copy(value = f(value))

  def setValue[B](value: B): EvalNode[F, B] = copy(value = value)

  def modify(f: Cursor => Cursor): EvalNode[F, A] = copy(cursor = f(cursor))

  def succeed[B](value: B, f: Cursor => Cursor): EvalNode[F, B] =
    copy(cursor = f(cursor), value)

  def succeed[B](value: B): EvalNode[F, B] = succeed(value, identity)

  def setActive(active: Res[F]): EvalNode[F, A] =
    copy(active = active)
}

object EvalNode {
  def empty[F[_], A](value: A, active: Res[F]): EvalNode[F, A] =
    EvalNode[F, A](Cursor.empty, value, active)
}

trait StreamingApi[F[_]] {
  def submit[A](cont: Continuation[F, A], node: EvalNode[F, A]): F[Unit]
}

object EvalState {
  final case class Entry[F[_], A](
      // the continuation of the node
      cont: Continuation[F, A],
      a: EvalNode[F, A]
  )
}
