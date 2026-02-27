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

import cats.effect.implicits.*
import gql.preparation.*
import cats.effect.*
import gql.*
import gql.server.planner.*
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import gql.resolver.Step.BatchKey
import cats.effect.std.MapRef
import cats.implicits.*
import gql.SchemaState.BatchFunction
import cats.data.Chain
import cats.*

trait QueryPlanBatches[F[_]] {
  def submitBatch[K, V](p: PreparedStep.Batch[F, K, V], k: ArraySeq[EvalNode[F, Set[K]]]): F[Option[Map[K, V]]]
}

object QueryPlanBatches {
  def make[F[_]](
      schemaState: SchemaState[F],
      plan: OptimizedDAG,
      stats: Statistics[F],
      errors: Ref[F, Chain[EvalFailure]],
      throttle: F ~> F
  )(implicit F: Async[F]) = {
    // assign arbitary ids to the batches
    case class AnalysisBatchState(
        needs: Set[NodeId],
        batch: BatchKey[?, ?]
    )
    case class BatchStateId(id: Int)
    val batchRefStateLookup = mutable.HashMap.empty[BatchStateId, AnalysisBatchState]
    val batchRefLookup = mutable.HashMap.empty[NodeId, BatchStateId]
    plan.batches.zipWithIndex.foreach { case ((batchParticipants, _), i) =>
      plan.tree.lookup(batchParticipants.head).batchId.foreach { batch =>
        val bid = batch.batcherId
        val bsi = BatchStateId(i)
        batchRefStateLookup += bsi -> AnalysisBatchState(batchParticipants.toSet, bid)
        batchParticipants.foreach { n =>
          batchRefLookup += n -> bsi
        }
      }
    }

    case class BatchState[K, V](
        bk: BatchKey[K, V],
        missing: Set[NodeId],
        submittedKeys: List[ArraySeq[Set[K]]],
        cursors: List[ArraySeq[Cursor]],
        listeners: List[Option[Map[K, V]] => F[Unit]]
    )
    MapRef[F, BatchStateId, BatchState[?, ?]]
      .flatTap { state =>
        batchRefStateLookup.toList.traverse_ { case (k, v) =>
          state.setKeyValue(k, BatchState(v.batch, v.needs, Nil, Nil, Nil))
        }
      }
      .map { state =>
        new QueryPlanBatches[F] {
          def submitBatch[K, V](p: PreparedStep.Batch[F, K, V], k: ArraySeq[EvalNode[F, Set[K]]]): F[Option[Map[K, V]]] = {
            val bsId = batchRefLookup(p.nodeId)
            val bs = state(bsId)
            F.deferred[Option[Map[K, V]]]
              .flatTap { d =>
                bs.modify { opt =>
                  val bs = opt.get.asInstanceOf[BatchState[K, V]]
                  val newMissing = bs.missing - p.nodeId
                  val newSubmittedKeys = k.map(_.value) :: bs.submittedKeys
                  val newListeners = ((res: Option[Map[K, V]]) => d.complete(res).void) :: bs.listeners
                  val newCursors = k.map(_.cursor) :: bs.cursors
                  val newState = BatchState[K, V](bs.bk, newMissing, newSubmittedKeys, newCursors, newListeners)
                  val shouldExec = newMissing.isEmpty
                  (
                    Some(newState).filter(_ => !shouldExec),
                    Some(newState).filter(_ => shouldExec)
                  )
                }.flatMap {
                  case None => F.unit
                  case Some(execMe) =>
                    val batchFunction = schemaState.batchFunctions(execMe.bk).asInstanceOf[BatchFunction[F, K, V]]
                    val keySet = execMe.submittedKeys.iterator.flatMap(_.iterator).flatten.toSet
                    throttle(batchFunction.f(keySet).timed.attempt)
                      .flatMap[Option[Map[K, V]]] {
                        case Left(e) =>
                          val cs = Chain.fromSeq(execMe.cursors.flatten)
                          errors.update(Chain.one(EvalFailure.BatchResolution(cs, e)) ++ _).as(None)
                        case Right((dur, res)) =>
                          stats.updateStats(s"batch_${execMe.bk.id}", dur, keySet.size) *>
                            F.pure(Some(res))
                      }
                      .flatMap { res =>
                        execMe.listeners.traverse_(_(res))
                      }
                }
              }
              .flatMap(_.get)
          }
        }
      }
  }
}
