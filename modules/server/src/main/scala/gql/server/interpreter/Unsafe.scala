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
package fs2

import fs2.internal.Scope
import cats.effect.Resource
import cats._
import cats.implicits._

object UnsafeFs2Access {
  def getScope[F[_]]: Pull[F, Nothing, Scope[F]] = Pull.getScope[F]

  def leaseScope[F[_]: MonadThrow]: Pull[F, Nothing, Resource[F, Unit]] =
    getScope[F].map(scope => Resource.make(scope.lease)(x => x.cancel.rethrow).void)
}
