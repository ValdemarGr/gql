package gql.server.planner

import gql.preparation._
import cats._
import cats.data._
import cats.implicits._
import gql.resolver.Step

final case class BatchRef[K, V](
    batcherId: gql.resolver.Step.BatchKey[K, V],
    uniqueNodeId: UniqueBatchInstance[K, V]
)

final case class NodeId(id: Int) extends AnyVal

final case class Node(
    id: NodeId,
    name: String,
    cost: Double,
    elemCost: Double,
    parents: Set[NodeId],
    batchId: Option[BatchRef[?, ?]]
)

final case class NodeTree(all: List[Node])

final case class Plan(lookup: Map[NodeId, Double]) {
  def apply(id: NodeId): Double = lookup(id)

  def get(id: NodeId): Option[Double] = lookup.get(id)

  def +(kv: (NodeId, Double)): Plan = Plan(lookup + kv)
}

final case class PlannedNodeTree(
    tree: NodeTree,
    plan: Plan
) {
  lazy val batches: List[(Step.BatchKey[?, ?], NonEmptyChain[BatchRef[?, ?]])] = {
    tree.all
      .map(n => (n.batchId, n))
      .collect { case (Some(batcherKey), node) => (batcherKey, node) }
      .groupByNec { case (_, node) => plan(node.id) }
      .toList
      .flatMap { case (_, endGroup) =>
        endGroup.toList
          .groupBy { case (batcherKey, _) => batcherKey.batcherId }
          .map { case (batcherKey, batch) => batcherKey -> NonEmptyChain.fromSeq(batch.map { case (br, _) => br }) }
          .collect { case (k, Some(vs)) => k -> vs }
      }
  }

  lazy val totalCost: Double = {
    val thisFlat = tree.all
    val thisBatches = batches.filter { case (_, edges) => edges.size > 1 }
    val batchCostMap: Map[Step.BatchKey[?, ?], Double] =
      thisFlat.mapFilter(n => n.batchId.map(br => br.batcherId -> n.cost)).toMap

    val naiveCost = thisFlat.map(_.cost).sum

    val batchSubtraction = thisBatches.map { case (_, xs) =>
      // cost * (size - 1 )
      batchCostMap(xs.head.batcherId) * (xs.size - 1)
    }.sum

    naiveCost - batchSubtraction
  }
}

object PlannedNodeTree {
  /*implicit lazy val showForPlannedNodeTree: Show[PlannedNodeTree] = Show.show[PlannedNodeTree] { x =>
    x.tree.show(plan = Some(x.plan))
  }*/
}
