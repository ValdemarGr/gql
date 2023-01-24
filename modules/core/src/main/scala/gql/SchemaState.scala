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
package gql

import cats._

final case class SchemaState[F[_]](
    nextId: Int,
    batchFunctions: Map[Int, SchemaState.BatchFunction[F, ?, ?]]
) {
  def mapK[G[_]](fk: F ~> G): SchemaState[G] =
    SchemaState(
      nextId, 
      batchFunctions.map { case (k, v) => k -> SchemaState.BatchFunction(v.f.andThen(fk.apply)) }
    )
}

object SchemaState {
  final case class BatchFunction[F[_], K, V](f: Set[K] => F[Map[K, V]])
  def empty[F[_]] = SchemaState[F](nextId = 0, batchFunctions = Map.empty)
}
