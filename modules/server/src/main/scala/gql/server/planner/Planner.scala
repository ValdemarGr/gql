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

import fs2.{Pure, Stream}
import cats.implicits._
import cats._
import gql.preparation._

trait Planner[F[_]] { self =>
  def plan(naive: NodeTree): F[OptimizedDAG]

  def mapK[G[_]](fk: F ~> G): Planner[G] =
    new Planner[G] {
      def plan(naive: NodeTree): G[OptimizedDAG] = fk(self.plan(naive))
    }
}

object Planner {
  def apply[F[_]](implicit F: Applicative[F]) = new Planner[F] {
    def plan(tree: NodeTree): F[OptimizedDAG] = F.pure {
      // The best solution has lease amount of nodes in the contracted DAG
      val plan = enumerateAllPlanner[F](tree)
        .map(plan => (plan, plan.values.toSet.size))
        .take(3)
        .compile
        .fold(Option.empty[(Map[NodeId, PlanEnumeration.Batch], Int)]) {
          case (None, (nextPlan, s))                                                => Some((nextPlan, s))
          case (Some((_, bestSize)), (nextPlan, nextSize)) if (nextSize < bestSize) => Some((nextPlan, nextSize))
          case (best, _)                                                            => best
        }
        .map { case (bestPlan, _) => bestPlan }
        .map(_.map { case (k, v) => (NodeId(k.id), (v.nodes.map(n => NodeId(n.id)), v.end)) })
        .getOrElse(Map.empty)

      OptimizedDAG(tree, plan)
    }
  }

  def enumerateAllPlanner[F[_]](tree: NodeTree): Stream[Pure, Map[NodeId, PlanEnumeration.Batch]] = {
    val all = tree.all

    val trivials = all.filter(_.batchId.isEmpty)

    val batches = all.mapFilter(x => x.batchId tupleRight x)
    val grp = batches.groupBy { case (b, _) => b.batcherId }

    val trivialFamilies = trivials.map { n =>
      PlanEnumeration.Family(n.cost, Set(n.id))
    }

    val batchFamilies = grp.toList.map { case (_, gs) =>
      val (_, hd) = gs.head
      PlanEnumeration.Family(hd.cost, gs.map { case (_, x) => x.id }.toSet)
    }

    val rl = tree.reverseLookup.map { case (k, vs) =>
      NodeId(k.id) -> vs.toList.toSet
    }

    val nodes = (trivialFamilies ++ batchFamilies).toArray

    val prob = PlanEnumeration.Problem(nodes, rl)

    PlanEnumeration.enumerateAll(prob)
  }
}
