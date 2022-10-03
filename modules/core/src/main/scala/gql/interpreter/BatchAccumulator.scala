package gql.interpreter

import cats.effect.implicits._
import cats.implicits._
import cats.effect._
import gql.Planner
import cats.data._
import gql.SchemaState
import gql.resolver.BatchResolver

trait BatchAccumulator[F[_]] {
  // Emits the whole result of the batch, so the calle must filter
  def submit(id: Int, values: Chain[(CursorGroup, Set[BatchKey])]): F[Option[Map[BatchKey, BatchValue]]]

  def getErrors: F[List[EvalFailure.BatchResolution]]
}

object BatchAccumulator {
  def apply[F[_]](schemaState: SchemaState[F], plan: Planner.NodeTree)(implicit F: Concurrent[F]): F[BatchAccumulator[F]] = {
    val flat = plan.flattened

    // Group similar ends
    // Then group the end-groups by batcher id
    val batches: Chain[(BatchResolver.ResolverKey, NonEmptyChain[Int])] =
      Chain.fromSeq {
        flat
          .map(n => (n.batcher, n))
          .collect { case (Some(batcherKey), node) => (batcherKey, node) }
          .groupByNec { case (_, node) => node.end }
          .toList
          .flatMap { case (_, endGroup) =>
            endGroup
              .groupBy { case (batcherKey, _) => batcherKey }
              .toSortedMap
              .toList
              .map { case (batcherKey, batch) =>
                batcherKey -> batch.map { case (_, node) => node.id }
              }
          }
      }

    // Now we allocate a deferred for each id in each batch
    type BatchPromise = Option[Map[BatchKey, BatchValue]] => F[Unit]
    type InputType = Chain[(CursorGroup, Set[BatchKey])]
    F.ref(List.empty[EvalFailure.BatchResolution]).flatMap { errState =>
      batches
        .flatTraverse { case (batcherKey, batch) =>
          F.ref(Map.empty[Int, (InputType, BatchPromise)]).map { inputAccum =>
            batch.map(id => id -> (inputAccum, batch, batcherKey)).toChain
          }
        }
        .map(_.toList.toMap)
        .map { accumLookup =>
          new BatchAccumulator[F] {
            def submit(id: Int, values: InputType): F[Option[Map[BatchKey, BatchValue]]] = {
              F.deferred[Option[Map[BatchKey, BatchValue]]].flatMap { ret =>
                val (state, batchIds, batcherKey) = accumLookup(id)

                val resolver = schemaState.batchers(batcherKey)

                val modState: F[Option[Chain[(InputType, BatchPromise)]]] = {
                  val complete: BatchPromise = ret.complete(_).void

                  state.modify { m =>
                    // We are not the last submitter
                    if (m.size != (batchIds.size + 1)) {
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

                    resolver(allKeysSet).attempt
                      .flatMap[Option[Map[BatchKey, BatchValue]]] {
                        case Left(err) =>
                          val allCursors: Chain[CursorGroup] =
                            xs.flatMap { case (input, _) => input.map { case (cg, _) => cg } }

                          errState
                            .update(EvalFailure.BatchResolution(allCursors, err, allKeysSet) :: _)
                            .as(None)
                        case Right(res) => F.pure(Some(res))
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
