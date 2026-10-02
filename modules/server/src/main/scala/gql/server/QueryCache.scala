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
package gql

import cats.effect.Async
import cats.effect.std.Mutex
import cats.implicits._
import gql.preparation.PreparedRoot
import java.util.LinkedHashMap

/** A bounded LRU overlay for one preparation function. */
final class QueryCache[F[_], Q, M, S] private (
    maxEntries: Int,
    prepare: (String, Option[String]) => Either[CompilationError, CacheableQuery[F, Q, M, S]],
    entries: LinkedHashMap[(String, Option[String]), CacheableQuery[F, Q, M, S]],
    mutex: Mutex[F]
)(implicit F: Async[F]) {
  def getPrep(query: String, operationName: Option[String] = None): F[Option[CacheableQuery[F, Q, M, S]]] =
    mutex.lock.surround(F.delay(Option(entries.get((query, operationName)))))

  def persist(cq: CacheableQuery[F, Q, M, S]): F[Unit] =
    mutex.lock.surround {
      F.delay {
        entries.put((cq.query, cq.operationName), cq)
        if (entries.size() > maxEntries) {
          val oldest = entries.entrySet().iterator()
          oldest.next()
          oldest.remove()
        }
      }
    }

  def compile(qp: QueryParameters): F[Either[CompilationError, PreparedRoot[F, Q, M, S]]] =
    getPrep(qp.query, qp.operationName)
      .flatMap {
        case Some(cq) => F.pure(cq.asRight[CompilationError])
        case None     => F.delay(prepare(qp.query, qp.operationName)).flatTap(_.traverse_(persist))
      }
      .flatMap(_.traverse(cq => F.delay(cq.run(qp.variables.getOrElse(Map.empty)))).map(_.flatten))
}

object QueryCache {
  def apply[F[_]: Async, Q, M, S](maxEntries: Int)(
      prepare: (String, Option[String]) => Either[CompilationError, CacheableQuery[F, Q, M, S]]
  ): F[QueryCache[F, Q, M, S]] =
    for {
      entries <- Async[F].delay {
        require(maxEntries > 0, "maxEntries must be positive")
        new LinkedHashMap[(String, Option[String]), CacheableQuery[F, Q, M, S]](16, 0.75f, true)
      }
      mutex <- Mutex[F]
    } yield new QueryCache(maxEntries, prepare, entries, mutex)
}
