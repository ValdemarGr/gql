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
package gql.client.codegen

import cats.implicits._
import cats.effect._
import cats._

trait Logger[F[_]] {
  def log(msg: String): F[Unit]

  def scoped[A](ctx: String)(fa: F[A])(implicit F: Monad[F]): F[A] =
    log(s"${ctx.capitalize}...") *> fa <* log(s"Done ${ctx}")
}

object Logger {
  def make[F[_]](outputChannel: String => F[Unit])(implicit F: Async[F]) =
    F.realTime.flatMap(F.ref(_)).map { state =>
      new Logger[F] {
        def log(msg: String): F[Unit] =
          for {
            before <- state.get
            now <- F.realTime
            _ <- state.set(now)
            diff = now - before
            m = s"${diff.toMillis} ms since last message: $msg"
            _ <- outputChannel(m)
          } yield ()
      }
    }
}
