package gql.server.planner

import cats.implicits._

object PlanEnumeration {
    final case class NodeId(id: Int) extends AnyVal
    final case class FamilyId(id: Int) extends AnyVal

    final case class Family(
      cost: Int,
      nodes: Set[NodeId]
    )

  final case class Problem(
      families: Array[Family],
      arcs: Map[NodeId, Set[NodeId]]
  ) {
    val all: Array[NodeId] = families.flatMap(_.nodes.toArray)

    val reverseArcs: Map[NodeId, Set[NodeId]] =
      arcs.toList
        .flatMap { case (parent, children) => children.map(_ -> parent) }
        .groupMap { case (child, _) => child } { case (_, parent) => parent }
        .fmap(_.toSet)

    val familyMap: Map[NodeId, FamilyId] =
      families.zipWithIndex.flatMap { case (fam, id) => fam.nodes.map(_ -> FamilyId(id)) }.toMap
  }

  final case class BatchId(id: Int) extends AnyVal
  final case class EndTime(time: Int) extends AnyVal

  final case class State(
      batch: Map[BatchId, EndTime],
      visited: Map[NodeId, BatchId],
      forbidden: Set[Set[NodeId]]
  )
}