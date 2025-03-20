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

import gql.preparation._
import cats.data.Ior

sealed trait Continuation[F[_], -I] {
  def contramap[I2](f: I2 => I): Continuation[F, I2] =
    Continuation.Contramap[F, I, I2](f, this)
}
object Continuation {
  final case class Done[F[_], I](prep: Prepared[F, I]) extends Continuation[F, I]
  final case class Continue[F[_], I, C](
      step: PreparedStep[F, I, C],
      next: Continuation[F, C]
  ) extends Continuation[F, I]
  final case class Contramap[F[_], I, I2](
      f: I2 => I,
      next: Continuation[F, I]
  ) extends Continuation[F, I2]
  final case class Rethrow[F[_], I](
      inner: Continuation[F, I]
  ) extends Continuation[F, Ior[EvalFailure, I]]
}
