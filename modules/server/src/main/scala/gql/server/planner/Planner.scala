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
package gql.server.planner

import cats.implicits._
import cats.data._
import gql.preparation._
import scala.collection.immutable.TreeSet
import cats._
import scala.io.AnsiColor
import cats.mtl.Stateful
import gql.resolver.Step
import gql._
import gql.server.planner.PlanEnumeration
import gql.server.planner.OptimizedDAG

trait Planner[F[_]] { self =>
  def plan(naive: NodeTree): F[OptimizedDAG]

  def mapK[G[_]](fk: F ~> G): Planner[G] =
    new Planner[G] {
      def plan(naive: NodeTree): G[OptimizedDAG] = fk(self.plan(naive))
    }
}

object Planner {
  final case class TraversalState(
      id: Int,
      parents: Set[NodeId],
      nodes: Chain[Node]
  )

  def getId[F[_]: Applicative](implicit S: Stateful[F, TraversalState]): F[NodeId] =
    S.inspect(x => NodeId(x.id)) <* S.modify(s => s.copy(id = s.id + 1))

  def setParents[F[_]](parents: Set[NodeId])(implicit S: Stateful[F, TraversalState]): F[Unit] =
    S.modify(s => s.copy(parents = parents))

  def addNodes[F[_]](nodes: Chain[Node])(implicit S: Stateful[F, TraversalState]): F[Unit] =
    S.modify(s => s.copy(nodes = s.nodes ++ nodes))

  def addNode[F[_]](node: Node)(implicit S: Stateful[F, TraversalState]): F[Unit] =
    S.modify(s => s.copy(nodes = s.nodes :+ node))

  def costForStep[F[_], G[_]](step: PreparedStep[G, ?, ?])(implicit
      stats: Statistics[F],
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[Unit] = {
    def goParallel(l: PreparedStep[G, ?, ?], r: PreparedStep[G, ?, ?]): F[Unit] = {
      // A parallel op is disjunctive so the parent must be the same for both branches
      // This creates a diamond shape in the graph
      // Parent -> Left -> Child
      // Parent -> Right -> Child
      S.inspect(_.parents).flatMap { ps =>
        val setParents = S.modify(s => s.copy(parents = ps))
        val left = setParents *> costForStep(l) *> S.inspect(_.parents)
        val right = setParents *> costForStep(r) *> S.inspect(_.parents)

        (left, right).flatMapN { case (lp, rp) =>
          val parents = lp ++ rp
          S.modify(s => s.copy(parents = parents))
        }
      }
    }

    import PreparedStep._
    step match {
      case Lift(_) | EmbedError() | GetMeta(_) => F.unit
      case Compose(l, r)                       => costForStep[F, G](l) *> costForStep[F, G](r)
      case alg: Choose[G, ?, ?, ?, ?]          => goParallel(alg.fac, alg.fbc)
      case alg: First[G, ?, ?, ?]              => costForStep[F, G](alg.step)
      case Batch(_, _) | EmbedEffect(_) | EmbedStream(_, _) =>
        val name = step match {
          case Batch(id, _)           => s"batch_$id"
          case EmbedEffect(cursor)    => cursor.asString
          case EmbedStream(_, cursor) => cursor.asString
          case _                      => ???
        }

        val costF = stats
          .getStatsOpt(name)
          .map(_.getOrElse(Statistics.Stats(100d, 5d)))

        costF.flatMap { cost =>
          getId[F].flatMap { id =>
            S.inspect(_.parents).flatMap { parentIds =>
              addNode {
                Node(
                  id,
                  name,
                  cost.initialCost,
                  cost.extraElementCost,
                  parentIds,
                  step match {
                    case Batch(batcherId, uniqueNodeId) => Some(BatchRef(batcherId, uniqueNodeId))
                    case _                              => None
                  }
                )
              }
            }
          }
        }
    }
  }

  def costForFields[F[_], G[_]](prepared: NonEmptyList[PreparedField[G, ?]])(implicit
      F: Monad[F],
      stats: Statistics[F],
      S: Stateful[F, TraversalState]
  ): F[Unit] = {
    prepared.toList.traverse_ { pf =>
      pf match {
        case PreparedDataField(_, _, cont)          => costForCont[F, G](cont.edges, cont.cont)
        case PreparedSpecification(_, _, selection) => costForFields[F, G](selection)
      }
    }
  }

  def costForPrepared[F[_]: Statistics, G[_]](p: Prepared[G, ?])(implicit
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[Unit] =
    p match {
      case PreparedLeaf(_, _)          => F.unit
      case Selection(fields)           => costForFields[F, G](fields)
      case l: PreparedList[G, ?, ?, ?] => costForCont[F, G](l.of.edges, l.of.cont)
      case o: PreparedOption[G, ?, ?]  => costForCont[F, G](o.of.edges, o.of.cont)
    }

  def costForCont[F[_]: Statistics: Monad, G[_]](
      edges: PreparedStep[G, ?, ?],
      cont: Prepared[G, ?]
  )(implicit S: Stateful[F, TraversalState]): F[Unit] =
    costForStep[F, G](edges) *> costForPrepared[F, G](cont)

  type H[F[_], A] = StateT[F, TraversalState, A]
  def liftStatistics[F[_]: Applicative](stats: Statistics[F]): Statistics[H[F, *]] =
    stats.mapK(StateT.liftK[F, TraversalState])

  def runCostAnalysisFor[F[_]: Monad, A](f: Statistics[H[F, *]] => H[F, A])(implicit stats: Statistics[F]): F[A] =
    f(liftStatistics[F](stats)).runA(TraversalState(1, Set.empty, Chain.empty))

  def runCostAnalysis[F[_]: Monad: Statistics, A](f: Statistics[H[F, *]] => H[F, A]): F[NodeTree] =
    runCostAnalysisFor[F, List[Node]](s => f(s).get.map(_.nodes.toList)).map(NodeTree(_))

  def enumerateAllPlanner[F[_]](tree: NodeTree) = {
    val all = tree.all

    val trivials = all.filter(_.batchId.isEmpty)

    val batches = all.mapFilter(x => x.batchId tupleRight x)
    val grp = batches.groupBy { case (b, _) => b.batcherId }

    val trivialFamilies = trivials.map { n =>
      PlanEnumeration.Family(n.cost, Set(PlanEnumeration.NodeId(n.id.id)))
    }

    val batchFamilies = grp.toList.map { case (_, gs) =>
      val (_, hd) = gs.head
      PlanEnumeration.Family(hd.cost, gs.map { case (_, x) => PlanEnumeration.NodeId(x.id.id) }.toSet)
    }

    val prob = PlanEnumeration.Problem(
      (trivialFamilies ++ batchFamilies).toArray,
      tree.reverseLookup.map { case (k, vs) =>
        PlanEnumeration.NodeId(k.id) -> vs.toList.map(v => PlanEnumeration.NodeId(v.id)).toSet
      }
    )

    PlanEnumeration.enumerateAll(prob)
  }

  def apply[F[_]](implicit F: Applicative[F]) = new Planner[F] {
    def plan(tree: NodeTree): F[OptimizedDAG] = {
      // The best solution has lease amount of nodes in the contracted DAG
      val best = enumerateAllPlanner[F](tree)
        .take(tree.all.size)
        .minBy(m => m.values.toSet.size)
      F.pure {
        OptimizedDAG(
          tree.asInstanceOf[gql.server.planner.NodeTree],
          null
        )
      }
      ???
    }
  }
}
