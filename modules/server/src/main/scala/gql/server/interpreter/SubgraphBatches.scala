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
      case Lift(_, _) | EmbedError(_) | GetMeta(_, _) | EmbedEffect(_) | EmbedStream(_, _) => Eval.now(state)
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
      case PreparedSpecification(_, _, selection) => selection.foldMapA(countField(_))
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

  final case class BatchFamily[F[_], K, V](
      pendingInputs: Int,
      keys: Set[K],
      completes: List[Deferred[F, Option[Map[K, V]]]],
      cursors: Chain[Cursor]
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
          F.ref[BatchFamily[F, ?, ?]](BatchFamily(vs.size, Set.empty, Nil, Chain.empty)).map { ref =>
            vs.toList.tupleRight(ref)
          }
        }
        .map(_.toMap)
    val inlineBatchIds: F[Map[BatchNodeId, Ref[F, BatchFamily[F, ?, ?]]]] =
      (countState.childBatches -- groups.map { case (_, vs) => vs.toList }.flatten).toList
        .traverse { id =>
          F.ref[BatchFamily[F, ?, ?]](BatchFamily(1, Set.empty, Nil, Chain.empty)).tupleLeft(id)
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
        def consume[K, V](
            ref: Ref[F, BatchFamily[F, K, V]],
            inputs: Set[K],
            cursor: Cursor,
            run: Set[K] => F[Map[K, V]],
            statsId: String
        ): F[Option[Map[K, V]]] = {
          F.deferred[Option[Map[K, V]]].flatMap { d =>
            ref.modify { bf =>
              val allKeys = bf.keys ++ inputs
              val allCursors = bf.cursors :+ cursor
              if (bf.pendingInputs > 1) {
                BatchFamily[F, K, V](
                  keys = allKeys,
                  pendingInputs = bf.pendingInputs - 1,
                  completes = d :: bf.completes,
                  cursors = allCursors
                ) -> d.get
              } else {
                val empty = BatchFamily[F, K, V](
                  keys = Set.empty,
                  pendingInputs = 0,
                  completes = Nil,
                  cursors = Chain.empty
                )
                val effect = throttle {
                  run(allKeys).timed.attempt
                    .flatMap[Option[Map[K, V]]] {
                      case Right((dur, result)) => stats.updateStats(statsId, dur, allKeys.size).as(Some(result))
                      case Left(err) =>
                        errRef.update(_ :+ EvalFailure.BatchResolution(allCursors, err)).as(None)
                    }
                    .flatTap(res => bf.completes.traverse_(_.complete(res).void))
                }

                empty -> effect
              }
            }
          }
        }.flatten

        new SubgraphBatches[F] {
          override def multiplicityNode(mulId: NodeId, n: Int): F[Unit] = {
            val toAdd = n - 1
            countState.accum
              .get(MulitplicityNode(mulId))
              .traverse_(_.toList.traverse_ { id =>
                val ref = batches(id)
                ref.update { case bf =>
                  bf.copy(pendingInputs = bf.pendingInputs + toAdd)
                }
              })
          }

          override def inlineBatch[K, V](
              ilb: PreparedStep.InlineBatch[F, K, V],
              keys: Set[K],
              cursor: Cursor
          ): F[Option[Map[K, V]]] = {
            val ref = batches(BatchNodeId(ilb.nodeId))
            consume[K, V](ref.asInstanceOf[Ref[F, BatchFamily[F, K, V]]], keys, cursor, ilb.run, ilb.sei.edgeId.asString)
          }

          override def batch[K, V](
              ubi: UniqueBatchInstance[K, V],
              keys: Set[K],
              cursor: Cursor
          ): F[Option[Map[K, V]]] = {
            val ref = batches(BatchNodeId(ubi.id))

            val (impl, implId) = getBatchImpl(ubi)
            consume[K, V](
              ref.asInstanceOf[Ref[F, BatchFamily[F, K, V]]],
              keys,
              cursor,
              impl.f,
              s"batch_${implId}"
            )
          }

          override def getErrors: F[Chain[EvalFailure.BatchResolution]] = errRef.get
        }
      }
    }
  }
}
