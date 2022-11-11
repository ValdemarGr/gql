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
package gql.interpreter

import cats.effect.implicits._
import cats.implicits._
import cats.effect._
import gql.Planner
import cats.data._
import gql.SchemaState
import gql.resolver.BatchResolver
import gql.PreparedQuery
import gql.Statistics

trait BatchAccumulator[F[_]] {
  // Emits the whole result of the batch, so the calle must filter
  def submit(id: PreparedQuery.EdgeId, values: Chain[(Cursor, Set[BatchKey])]): F[Option[Map[BatchKey, BatchValue]]]

  def getErrors: F[List[EvalFailure.BatchResolution]]
}

object BatchAccumulator {
  def apply[F[_]](schemaState: SchemaState[F], plan: Planner.NodeTree)(implicit
      F: Async[F],
      stats: Statistics[F]
  ): F[BatchAccumulator[F]] = {
    val batches: Chain[(BatchResolver.ResolverKey, NonEmptyChain[PreparedQuery.EdgeId])] =
      Chain.fromSeq(plan.batches)

    // Now we allocate a deferred for each id in each batch
    type BatchPromise = Option[Map[BatchKey, BatchValue]] => F[Unit]
    type InputType = Chain[(Cursor, Set[BatchKey])]
    F.ref(List.empty[EvalFailure.BatchResolution]).flatMap { errState =>
      batches
        .flatTraverse { case (batcherKey, batch) =>
          F.ref(Map.empty[PreparedQuery.EdgeId, (InputType, BatchPromise)]).map { inputAccum =>
            batch.map(id => id -> (inputAccum, batch, batcherKey)).toChain
          }
        }
        .map(_.toList.toMap)
        .map { accumLookup =>
          new BatchAccumulator[F] {
            def submit(id: PreparedQuery.EdgeId, values: InputType): F[Option[Map[BatchKey, BatchValue]]] = {
              F.deferred[Option[Map[BatchKey, BatchValue]]].flatMap { ret =>
                val (state, batchIds, batcherKey) = accumLookup(id)

                val resolver = schemaState.batchers(batcherKey)

                val modState: F[Option[Chain[(InputType, BatchPromise)]]] = {
                  val complete: BatchPromise = ret.complete(_).void

                  state.modify { m =>
                    // We are not the last submitter
                    if (m.size != (batchIds.size - 1)) {
                      val m2 = m + (id -> ((values, complete)))
                      (m2, None)
                    } else {
                      // Garbage collect
                      val m2 = m -- batchIds.toIterable
                      val allElems: Chain[(InputType, BatchPromise)] =
                        batchIds.collect { case bid if bid != id => m(bid) } append (values, complete)
                      (m2, Some(allElems))
                    }
                  }
                }

                modState.flatMap {
                  // Not last submitter, just await
                  case None => F.unit
                  case Some(xs) =>
                    val allKeys: Chain[BatchKey] = xs.flatMap { case (input, _) =>
                      input.flatMap { case (_, keys) =>
                        Chain.fromIterableOnce(keys)
                      }
                    }

                    val allKeysSet: Set[BatchKey] = Set.from(allKeys.iterator)

                    resolver(allKeysSet).timed.attempt
                      .flatMap[Option[Map[BatchKey, BatchValue]]] {
                        case Left(err) =>
                          val allCursors: Chain[Cursor] =
                            xs.flatMap { case (input, _) => input.map { case (cg, _) => cg } }

                          errState
                            .update(EvalFailure.BatchResolution(allCursors, err, allKeysSet) :: _)
                            .as(None)
                        case Right((dur, res)) =>
                          stats.updateStats(s"batch_${batcherKey.id}", dur, allKeysSet.size) as Some(res)
                      }
                      .flatMap(res => xs.parTraverse_ { case (_, complete) => complete(res) })
                } >> ret.get
              }
            }

            def getErrors: F[List[EvalFailure.BatchResolution]] = errState.get
          }
        }
    }
  }
}
