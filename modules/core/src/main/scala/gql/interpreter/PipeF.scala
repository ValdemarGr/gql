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
package gql.interpreter

import fs2._
import cats.effect._
import scala.concurrent.duration.FiniteDuration

trait PipeF[F[_]] {
  def apply[A]: fs2.Pipe[F, Chunk[A], Chunk[A]]
}

object PipeF {
  type τ[F[_]]

  def apply[F[_]](f: fs2.Pipe[F, Chunk[τ[F]], Chunk[τ[F]]]) = new PipeF[F] {
    def apply[A] = f.asInstanceOf[fs2.Pipe[F, Chunk[A], Chunk[A]]]
  }

  def identity[F[_]] = apply[F](x => x)

  def groupWithin[F[_]: Temporal](n: Int, d: FiniteDuration) = apply[F](_.unchunks.groupWithin(n, d))
}
