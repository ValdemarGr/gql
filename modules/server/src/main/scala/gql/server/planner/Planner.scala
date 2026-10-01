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
import gql.resolver.Step.BatchKey
import scala.collection.mutable

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
      val all = tree.all.toArray
      val nodeIds = mutable.HashMap.from(all.iterator.zipWithIndex.map { case (node, i) => node.id -> i })
      val families = mutable.HashMap.empty[Either[NodeId, BatchKey[?, ?]], Int]
      val costs = mutable.ArrayBuffer.empty[Double]
      val nodes = Array.tabulate(all.length) { i =>
        val node = all(i)
        val key = node.batchId.fold[Either[NodeId, BatchKey[?, ?]]](Left(node.id))(batch => Right(batch.batcherId))
        val family = families.getOrElseUpdate(
          key, {
            costs.addOne(node.cost)
            costs.size - 1
          }
        )
        BatchPlanner.Node(i, family, mutable.BitSet.empty)
      }
      all.indices.foreach { i =>
        all(i).parents.foreach(parent => nodes(nodeIds(parent)).children.addOne(i))
      }

      val endTimes = mutable.HashMap.empty[NodeId, Double]
      val batches = BatchPlanner
        .solve(BatchPlanner.Problem(nodes))
        .iterator
        .map { case (family, batch) =>
          val participants = batch.iterator.map(i => all(i).id).toSet
          val parentEnd =
            batch.iterator.flatMap(i => all(i).parents.iterator).map(endTimes(_)).maxOption.getOrElse(0d)
          val end = PlanEnumeration.EndTime(parentEnd + costs(family))
          participants.foreach(nodeId => endTimes.update(nodeId, end.time))
          (participants, end)
        }
        .toSet
      OptimizedDAG(tree, batches)
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
