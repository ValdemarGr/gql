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

import cats.effect.implicits._
import cats.implicits._
import cats.effect._
import gql._
import cats.data._
import gql.server.planner._
import gql.resolver.Step
import gql.preparation._
import cats._

trait BatchAccumulator[F[_]] { self =>
  // Emits the whole result of the batch, so the calle must filter
  def submit[K, V](id: UniqueBatchInstance[K, V], values: Chain[(Cursor, Set[K])]): F[Option[Map[K, V]]]

  def getErrors: F[List[EvalFailure.BatchResolution]]

  def alpha(i: Int): BatchAccumulator[F] = new BatchAccumulator[F] {
    def submit[K, V](id: UniqueBatchInstance[K, V], values: Chain[(Cursor, Set[K])]): F[Option[Map[K, V]]] =
      self.submit(id.alpha(i), values)

    def getErrors: F[List[EvalFailure.BatchResolution]] = self.getErrors
  }
}

object BatchAccumulator {
  def groupBatches(
      plan: OptimizedDAG
  ): Chain[(Step.BatchKey[?, ?], NonEmptyChain[UniqueBatchInstance[?, ?]])] = Chain.fromSeq {
    plan.plan.values.toList.map{ case (bs, _) => bs }.distinct.mapFilter { bs =>
      plan.tree.lookup(bs.head).batchId.map(_.batcherId).map { bk =>
        val vs = NonEmptyChain
          .fromChainUnsafe(Chain.fromIterableOnce(bs))
          .map(n => plan.tree.lookup(n).batchId.get.uniqueNodeId)
        bk -> vs
      }
    }
  }

  final case class BatchingState[F[_], K, V](
      complete: Option[Map[K, V]] => F[Unit],
      keys: Chain[(Cursor, Set[K])]
  )
  final case class Batch[F[_], K, V](
      inputs: Ref[F, Map[UniqueBatchInstance[K, V], BatchingState[F, K, V]]],
      submitters: NonEmptyChain[UniqueBatchInstance[K, V]],
      batcherRef: Step.BatchKey[K, V]
  )
  def batchingState[F[_]](
      plan: OptimizedDAG
  )(implicit F: Concurrent[F]): F[Map[UniqueBatchInstance[?, ?], Batch[F, ?, ?]]] =
    groupBatches(plan)
      .flatTraverse[F, (UniqueBatchInstance[?, ?], Batch[F, ?, ?])] { case (batcherKey: Step.BatchKey[k, v], batchIds) =>
        val casted = batchIds.asInstanceOf[NonEmptyChain[UniqueBatchInstance[k, v]]]
        F.ref(Map.empty[UniqueBatchInstance[k, v], BatchingState[F, k, v]]).map { inputAccum =>
          val b = Batch[F, k, v](
            inputAccum,
            casted,
            batcherKey
          )

          (casted tupleRight b).toChain
        }
      }
      .map(_.toList.toMap)

  trait UnsafeGetBatch[F[_]] {
    def get[K, V](k: UniqueBatchInstance[K, V]): Batch[F, K, V]
  }
  def unsafeBatchingState[F[_]](plan: OptimizedDAG)(implicit F: Concurrent[F]): F[UnsafeGetBatch[F]] =
    batchingState[F](plan).map(m =>
      new UnsafeGetBatch[F] {
        def get[K, V](k: UniqueBatchInstance[K, V]): Batch[F, K, V] =
          m(k).asInstanceOf[Batch[F, K, V]]
      }
    )

  def apply[F[_]](
      schemaState: SchemaState[F],
      plan: OptimizedDAG,
      throttle: F ~> F
  )(implicit F: Async[F], stats: Statistics[F]): F[BatchAccumulator[F]] =
    F.ref(List.empty[EvalFailure.BatchResolution]).flatMap { errState =>
      unsafeBatchingState[F](plan).map { ugb =>
        new BatchAccumulator[F] {
          def submit[K, V](id: UniqueBatchInstance[K, V], values: Chain[(Cursor, Set[K])]): F[Option[Map[K, V]]] = {
            F.deferred[Option[Map[K, V]]].flatMap { ret =>
              val bs = ugb.get(id)
              val state = bs.inputs
              val batchIds = bs.submitters
              val batcherKey = bs.batcherRef

              val resolver0: SchemaState.BatchFunction[F, ?, ?] = schemaState.batchFunctions(batcherKey)
              val resolver = resolver0.asInstanceOf[SchemaState.BatchFunction[F, K, V]]

              val modState: F[Option[Chain[BatchingState[F, K, V]]]] = {
                val complete: Option[Map[K, V]] => F[Unit] = ret.complete(_).void

                state.modify { m =>
                  // We are not the last submitter
                  if (m.size != (batchIds.size - 1)) {
                    val m2 = m + (id -> BatchingState(complete, values))
                    (m2, None)
                  } else {
                    // Garbage collect
                    val m2 = m -- batchIds.toIterable
                    val allElems: Chain[BatchingState[F, K, V]] =
                      batchIds.collect { case bid if bid != id => m(bid) } append BatchingState(complete, values)
                    (m2, Some(allElems))
                  }
                }
              }

              modState.flatMap[Unit] {
                // Not last submitter, just await
                case None => F.unit
                case Some(xs) =>
                  val allKeys: Chain[K] = xs.flatMap { case BatchingState(_, input) =>
                    input.flatMap { case (_, keys) =>
                      Chain.fromIterableOnce(keys)
                    }
                  }

                  val allKeysSet: Set[K] = Set.from(allKeys.iterator)

                  throttle(resolver.f(allKeysSet)).timed.attempt
                    .flatMap[Option[Map[K, V]]] {
                      case Left(err) =>
                        val allCursors: Chain[Cursor] =
                          xs.flatMap { case BatchingState(_, input) => input.map { case (cg, _) => cg } }

                        errState
                          .update(EvalFailure.BatchResolution(allCursors, err) :: _)
                          .as(None)
                      case Right((dur, res)) =>
                        stats.updateStats(s"batch_${batcherKey.id}", dur, allKeysSet.size) as Some(res)
                    }
                    .flatMap(res => xs.parTraverse_ { case BatchingState(complete, _) => complete(res) })
              } >> ret.get
            }
          }

          def getErrors: F[List[EvalFailure.BatchResolution]] = errState.get
        }
      }
    }
}
