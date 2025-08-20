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
import cats.data._
import gql._
import org.typelevel.scalaccompat.annotation._
import cats.effect._
import cats._
import cats.implicits._
import gql.preparation._
import gql.server.planner.OptimizedDAG
import gql.Cursor
import gql.server.planner.BatchRef

trait SubgraphBatches[F[_]] { self =>
  def multiplicityNode(id: NodeId, n: Int): F[Unit]

  def inlineBatch[K, V](
      ilb: PreparedStep.InlineBatch[F, K, V],
      keys: Set[K],
      cursor: Cursor
  ): F[Option[Map[K, V]]]

  def batch[K, V](
      ubi: UniqueBatchInstance[K, V],
      keys: Set[K],
      cursor: Cursor
  ): F[Option[Map[K, V]]]

  def getErrors: F[Chain[EvalFailure.BatchResolution]]
}

object SubgraphBatches {
  final case class MulitplicityNode(id: NodeId)
  final case class BatchNodeId(id: NodeId)

  final case class State(
      childBatches: Set[BatchNodeId],
      accum: Map[MulitplicityNode, Set[BatchNodeId]]
  )
  object State {
    def empty = State(Set.empty, Map.empty)
    implicit val monoid: Monoid[State] = new Monoid[State] {
      def empty = State.empty
      def combine(x: State, y: State) = State(x.childBatches ++ y.childBatches, x.accum ++ y.accum)
    }
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def countStep[F[_]](state: State, step: PreparedStep[F, ?, ?]): Eval[State] = Eval.defer {
    import PreparedStep._
    step match {
      case Lift(_, _) | EmbedError(_) | GetMeta(_, _) | EmbedEffect(_) | EmbedStream(_) => Eval.now(state)
      case Compose(_, l, r) =>
        countStep(state, r).flatMap(countStep(_, l))
      case alg: Choose[F, ?, ?, ?, ?] =>
        for {
          s1 <- countStep(state, alg.fac)
          s2 <- countStep(state, alg.fbd)
          s1Unique = s1.childBatches -- s2.childBatches
          s1Out = s1.copy(accum = s1.accum + (MulitplicityNode(alg.fac.nodeId) -> s1Unique))

          s2Unique = s2.childBatches -- s1.childBatches
          s2Out = s2.copy(accum = s2.accum + (MulitplicityNode(alg.fbd.nodeId) -> s2Unique))
        } yield s1Out |+| s2Out
      case alg: First[F, ?, ?, ?]    => countStep(state, alg.step)
      case alg: Batch[F, ?, ?]       => Eval.now(state.copy(childBatches = state.childBatches + BatchNodeId(alg.nodeId)))
      case alg: InlineBatch[F, ?, ?] => Eval.now(state.copy(childBatches = state.childBatches + BatchNodeId(alg.nodeId)))
    }
  }

  def countCont[F[_]](ps: PreparedStep[F, ?, ?], cont: Prepared[F, ?]): Eval[State] = Eval.defer {
    countPrep(cont).flatMap(countStep(_, ps))
  }

  def countField[F[_]](pf: PreparedField[F, ?]): Eval[State] = Eval.defer {
    pf match {
      case PreparedDataField(_, _, _, cont, _, _) => countCont(cont.edges, cont.cont)
      case PreparedSpecification(nid, _, selection) =>
        selection.foldMapA(countField(_)).map { s =>
          s.copy(accum = s.accum + (MulitplicityNode(nid) -> s.childBatches))
        }
    }
  }

  def countPrep[F[_]](prep: Prepared[F, ?]): Eval[State] = Eval.defer {
    prep match {
      case PreparedLeaf(_, _, _)   => Eval.now(State(Set.empty, Map.empty))
      case Selection(_, fields, _) => fields.foldMapA(countField(_))
      case PreparedList(id, of, _) =>
        countCont(of.edges, of.cont).map(s => s.copy(accum = s.accum + (MulitplicityNode(id) -> s.childBatches)))
      case PreparedOption(id, of) =>
        countCont(of.edges, of.cont).map(s => s.copy(accum = s.accum + (MulitplicityNode(id) -> s.childBatches)))
    }
  }

  def countContinuation[F[_]](state: State, cont: Continuation[F, ?]): Eval[State] = Eval.defer {
    cont match {
      case Continuation.Done(prep)         => countPrep(prep).map(_ |+| state)
      case Continuation.Contramap(_, next) => countContinuation(state, next)
      case Continuation.Continue(step, next) =>
        countContinuation(state, next).flatMap(countStep(_, step))
      case Continuation.Rethrow(_, inner) => countContinuation(state, inner)
    }
  }

  def makeRootCounts(plan: OptimizedDAG): List[(BatchRef[?, ?], Set[BatchNodeId])] = {
    val batches: List[Set[NodeId]] = plan.plan.values.toList.map { case (bs, _) => bs }.distinct
    batches.mapFilter { xs =>
      plan.tree.lookup(xs.head).batchId.map { br =>
        br -> xs.map(BatchNodeId(_))
      }
    }
  }

  final case class InputSubmission[F[_], K, V](
      keys: Set[K],
      run: Set[K] => F[Map[K, V]],
      cursors: NonEmptyChain[Cursor],
      completes: NonEmptyChain[Deferred[F, Option[Map[K, V]]]],
      statId: String
  ) {
    def merge(that: InputSubmission[F, K, V]): InputSubmission[F, K, V] = {
      InputSubmission(
        keys ++ that.keys,
        run,
        cursors ++ that.cursors,
        completes ++ that.completes,
        statId
      )
    }
  }
  object InputSubmission {
    def use[F[_], K, V](
        keys: Set[K],
        run: Set[K] => F[Map[K, V]],
        cursor: Cursor,
        statId: String
    )(f: InputSubmission[F, K, V] => F[Unit])(implicit F: Concurrent[F]): F[Option[Map[K, V]]] =
      F.deferred[Option[Map[K, V]]].flatMap { d =>
        val is = InputSubmission(
          keys,
          run,
          NonEmptyChain.one(cursor),
          NonEmptyChain.one(d),
          statId
        )

        f(is) *> d.get
      }
  }

  final case class BatchFamily[F[_], K, V](
      pendingInputs: Int,
      inputSubmission: Option[InputSubmission[F, K, V]]
  )
  def make[F[_]](
      schemaState: SchemaState[F],
      countState: State,
      plan: OptimizedDAG,
      stats: Statistics[F],
      throttle: F ~> F
  )(implicit F: Async[F]): F[SubgraphBatches[F]] = {
    val groups = makeRootCounts(plan)
    val allBatches: F[Map[BatchNodeId, Ref[F, BatchFamily[F, ?, ?]]]] =
      groups
        .flatTraverse { case (_, vs) =>
          F.ref[BatchFamily[F, ?, ?]](BatchFamily(vs.size, none)).map { ref =>
            vs.toList.tupleRight(ref)
          }
        }
        .map(_.toMap)
    val inlineBatchIds: F[Map[BatchNodeId, Ref[F, BatchFamily[F, ?, ?]]]] =
      (countState.childBatches -- groups.map { case (_, vs) => vs.toList }.flatten).toList
        .traverse { id =>
          F.ref[BatchFamily[F, ?, ?]](BatchFamily(1, none)).tupleLeft(id)
        }
        .map(_.toMap)

    val batchLookup: Map[UniqueBatchInstance[?, ?], (SchemaState.BatchFunction[F, ?, ?], Int)] =
      plan.tree.all.mapFilter { n =>
        n.batchId.map { br =>
          (br.uniqueNodeId, (schemaState.batchFunctions(br.batcherId), br.batcherId.id))
        }
      }.toMap
    def getBatchImpl[K, V](id: UniqueBatchInstance[K, V]) = {
      val (res, id0) = batchLookup(id)
      (res.asInstanceOf[SchemaState.BatchFunction[F, K, V]], id0)
    }

    F.ref(Chain.empty[EvalFailure.BatchResolution]).flatMap { errRef =>
      (allBatches, inlineBatchIds).mapN(_ ++ _).map { batches =>
        def modifyFamily[K, V](
            bf: BatchFamily[F, K, V],
            n: Int
        ): (BatchFamily[F, K, V], F[Unit]) = {
          val newPending = bf.pendingInputs + n

          if (newPending > 0) {
            BatchFamily[F, K, V](
              pendingInputs = newPending,
              inputSubmission = bf.inputSubmission
            ) -> F.unit
          } else {
            val empty = BatchFamily[F, K, V](
              pendingInputs = 0,
              inputSubmission = none
            )

            val effect = bf.inputSubmission.traverse_ { is =>
              val go: F[Option[Map[K, V]]] =
                if (is.keys.nonEmpty) {
                  throttle {
                    is.run(is.keys)
                      .timed
                      .attempt
                      .flatMap[Option[Map[K, V]]] {
                        case Right((dur, result)) => stats.updateStats(is.statId, dur, is.keys.size).as(Some(result))
                        case Left(err) =>
                          errRef.update(_ :+ EvalFailure.BatchResolution(is.cursors.toChain, err)).as(None)
                      }
                  }
                } else {
                  F.pure(Some(Map.empty))
                }

              go.flatTap(res => is.completes.traverse_(_.complete(res).void))
            }

            empty -> effect
          }
        }

        def submit[K, V](
            ref: Ref[F, BatchFamily[F, K, V]],
            is: InputSubmission[F, K, V]
        ) =
          ref.modify { bf =>
            val combined = bf.inputSubmission.map(_.merge(is)).getOrElse(is)

            val (newFam, eff) = modifyFamily(bf.copy(inputSubmission = Some(combined)), -1)

            newFam -> eff
          }.flatten

        new SubgraphBatches[F] {
          override def multiplicityNode(mulId: NodeId, n: Int): F[Unit] = {
            if (n === 1) F.unit
            else {
              val toAdd = n - 1
              countState.accum
                .get(MulitplicityNode(mulId))
                .traverse_(_.toList.traverse_(batches(_).modify(modifyFamily(_, toAdd)).flatten))
            }
          }

          override def inlineBatch[K, V](
              ilb: PreparedStep.InlineBatch[F, K, V],
              keys: Set[K],
              cursor: Cursor
          ): F[Option[Map[K, V]]] = {
            val ref = batches(BatchNodeId(ilb.nodeId))
            InputSubmission.use(keys, ilb.run, cursor, ilb.sei.edgeId.asString) { is =>
              submit[K, V](ref.asInstanceOf[Ref[F, BatchFamily[F, K, V]]], is)
            }
          }

          override def batch[K, V](
              ubi: UniqueBatchInstance[K, V],
              keys: Set[K],
              cursor: Cursor
          ): F[Option[Map[K, V]]] = {
            val ref = batches(BatchNodeId(ubi.id))

            val (impl, implId) = getBatchImpl(ubi)
            InputSubmission.use(keys, impl.f, cursor, s"batch_${implId}") { is =>
              submit[K, V](ref.asInstanceOf[Ref[F, BatchFamily[F, K, V]]], is)
            }
          }

          override def getErrors: F[Chain[EvalFailure.BatchResolution]] = errRef.get
        }
      }
    }
  }
}
