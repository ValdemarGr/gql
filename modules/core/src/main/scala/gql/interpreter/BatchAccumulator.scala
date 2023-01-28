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

import cats.effect.implicits._
import cats.implicits._
import cats.effect._
import gql.Planner
import cats.data._
import gql.SchemaState
import gql.Statistics

trait BatchAccumulator[F[_]] {
  // Emits the whole result of the batch, so the calle must filter
  def submit[K, V](id: Int, values: Chain[(Cursor, Set[K])]): F[Option[Map[K, V]]]

  def getErrors: F[List[EvalFailure.BatchResolution]]
}

object BatchAccumulator {
  def apply[F[_]](schemaState: SchemaState[F], plan: Planner.NodeTree)(implicit
      F: Async[F],
      stats: Statistics[F]
  ): F[BatchAccumulator[F]] = {
    val batches: Chain[(Int, NonEmptyChain[Int])] =
      Chain.fromSeq(plan.batches)

    // Now we allocate a deferred for each id in each batch
    //type BatchPromise = Option[Map[BatchKey, BatchValue]] => F[Unit]
    final case class Batch[K, V](
        complete: Option[Map[K, V]] => F[Unit],
        keys: Chain[(Cursor, Set[K])]
    )
    F.ref(List.empty[EvalFailure.BatchResolution]).flatMap { errState =>
      batches
        .flatTraverse { case (batcherKey, batch) =>
          F.ref(Map.empty[Int, Batch[?, ?]]).map { inputAccum =>
            batch.map(id => id -> (inputAccum, batch, batcherKey)).toChain
          }
        }
        .map(_.toList.toMap)
        .map { accumLookup =>
          new BatchAccumulator[F] {
            def submit[K, V](id: Int, values: Chain[(Cursor, Set[K])]): F[Option[Map[K, V]]] = {
              F.deferred[Option[Map[K, V]]].flatMap { ret =>
                val (state0, batchIds, batcherKey) = accumLookup(id)
                val state1: Ref[F, Map[Int, Batch[?, ?]]] = state0
                val state = state1.asInstanceOf[Ref[F, Map[Int, Batch[K, V]]]]

                val resolver0: SchemaState.BatchFunction[F, ?, ?] = schemaState.batchFunctions(batcherKey)
                val resolver = resolver0.asInstanceOf[SchemaState.BatchFunction[F, K, V]]

                val modState: F[Option[Chain[Batch[K, V]]]] = {
                  val complete: Option[Map[K, V]] => F[Unit] = ret.complete(_).void

                  state.modify { m =>
                    // We are not the last submitter
                    if (m.size != (batchIds.size - 1)) {
                      val m2 = m + (id -> Batch(complete, values))
                      (m2, None)
                    } else {
                      // Garbage collect
                      val m2 = m -- batchIds.toIterable
                      val allElems: Chain[Batch[K, V]] =
                        batchIds.collect { case bid if bid != id => m(bid) } append Batch(complete, values)
                      (m2, Some(allElems))
                    }
                  }
                }

                modState.flatMap[Unit] {
                  // Not last submitter, just await
                  case None => F.unit
                  case Some(xs) =>
                    val allKeys: Chain[K] = xs.flatMap { case Batch(_, input) =>
                      input.flatMap { case (_, keys) =>
                        Chain.fromIterableOnce(keys)
                      }
                    }

                    val allKeysSet: Set[K] = Set.from(allKeys.iterator)

                    resolver
                      .f(allKeysSet)
                      .timed
                      .attempt
                      .flatMap[Option[Map[K, V]]] {
                        case Left(err) =>
                          val allCursors: Chain[Cursor] =
                            xs.flatMap { case Batch(_, input) => input.map { case (cg, _) => cg } }

                          errState
                            .update(EvalFailure.BatchResolution(allCursors, err) :: _)
                            .as(None)
                        case Right((dur, res)) =>
                          stats.updateStats(s"batch_${id}", dur, allKeysSet.size) as Some(res)
                      }
                      .flatMap(res => xs.parTraverse_ { case Batch(complete, _) => complete(res) })
                } >> ret.get
              }
            }

            def getErrors: F[List[EvalFailure.BatchResolution]] = errState.get
          }
        }
    }
  }
}
