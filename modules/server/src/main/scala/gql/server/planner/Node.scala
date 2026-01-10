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

import cats._
import gql.preparation._
import cats.data._
import cats.implicits._
import scala.io.AnsiColor

final case class BatchRef[K, V](
    batcherId: gql.resolver.Step.BatchKey[K, V],
    uniqueNodeId: UniqueBatchInstance[K, V]
) {
  def alpha(i: Int) = copy(uniqueNodeId = uniqueNodeId.alpha(i))
}

final case class Node(
    id: NodeId,
    name: String,
    cost: Double,
    elemCost: Double,
    parents: Set[NodeId],
    batchId: Option[BatchRef[?, ?]]
) {
  def alpha(i: Int) = copy(
    id = id.alpha(i),
    parents = parents.map(_.alpha(i)),
    batchId = batchId.map(_.alpha(i))
  )
}

final case class NodeTree(all: List[Node]) {
  def alpha(i: Int) = NodeTree(all.map(_.alpha(i)))

  lazy val lookup = all.map(n => n.id -> n).toMap

  lazy val roots = all.filter(_.parents.isEmpty)

  lazy val reverseLookup: Map[NodeId, List[NodeId]] = all
    .flatMap(n => n.parents.toList tupleRight n.id)
    .groupMap { case (k, _) => k } { case (_, v) => v }

  lazy val endTimes: Map[NodeId, Double] = {
    val l = lookup

    def go(id: NodeId): State[Map[NodeId, Double], Double] =
      State.inspect { (cache: Map[NodeId, Double]) => cache.get(id) }.flatMap {
        case Some(e) => State.pure(e)
        case None =>
          val n = l(id)
          n.parents.toList
            .traverse(go)
            .map(_.maxOption.getOrElse(0d) + n.cost)
            .flatTap(e => State.modify(_ + (id -> e)))
      }

    all.traverse_(x => go(x.id)).runS(Map.empty).value
  }
}

object NodeTree {
  implicit lazy val monoidForNodeTree: Monoid[NodeTree] = new Monoid[NodeTree] {
    def empty = NodeTree(Nil)
    def combine(x: NodeTree, y: NodeTree) = NodeTree(x.all ++ y.all)
  }
}

final case class OptimizedDAG(
    tree: NodeTree,
    plan: OptimizedDAG.Plan
) {
  lazy val batches: Set[(Set[NodeId], PlanEnumeration.EndTime)] = plan.values.toSet

  lazy val batchesWithCosts: List[(Double, Set[NodeId])] =
    batches.toList.map { case (b, _) => (tree.lookup(b.head).cost, b) }

  lazy val totalCost: Double = batchesWithCosts.foldMap { case (cost, b) => cost * b.size }

  lazy val optimizedCost: Double = batchesWithCosts.foldMap { case (cost, _) => cost }

  def show(ansiColors: Boolean = false) = {
    val lookup = tree.lookup
    val endTimes = tree.endTimes
    val children = tree.reverseLookup

    val maxEnd = math.max(
      batches.map { case (_, e) => e.time }.maxOption.getOrElse(0d),
      tree.endTimes.values.maxOption.getOrElse(0d)
    )

    val (red, green, blue, reset) =
      if (ansiColors) (AnsiColor.RED_B, AnsiColor.GREEN_B, AnsiColor.BLUE_B, AnsiColor.RESET)
      else ("", "", "", "")

    val prefix =
      if (ansiColors)
        s"""|
          |${red}old field schedule$reset
          |${green}new field offset (deferral of execution)$reset
          |""".stripMargin
      else ""

    val per = math.max((maxEnd / 40d), 1)

    val batchIndexes = batches.toList.zipWithIndex.flatMap { case ((s, _), i) =>
      s.map(n => (n, i)).toList
    }.toMap

    val planLookup =
      plan.toList.map { case (k, (v1, v2)) => (k, (v1, v2)) }.toMap

    def go(nodes: List[NodeId]): String = {
      nodes
        .map { n0 =>
          val n = lookup(n0)
          val nEnd = endTimes(n.id)
          val nStart = nEnd - n.cost
          val basePrefix = " " * (nStart / per).toInt
          val (_, ebEnd) = planLookup(n.id)
          val i = batchIndexes(n.id)
          def fmtDec(d: Double) = BigDecimal(d).setScale(2, BigDecimal.RoundingMode.CEILING).toString()
          val costFmt = fmtDec(n.cost)
          val showDisp = if (ebEnd.time != nEnd) Some {
            val dStart = ebEnd.time - n.cost
            val prefixLength = (BigDecimal(dStart - nStart) / per).setScale(0, BigDecimal.RoundingMode.HALF_UP).toInt
            val pushedPrefix = blue + ">" * prefixLength + green
            s"$basePrefix${pushedPrefix}name: ${n.name}, cost: ${costFmt}, end: ${fmtDec(ebEnd.time)}, batch: $i$reset"
          }
          else None

          val showHere =
            s"$basePrefix${if (showDisp.isDefined) red else ""}name: ${n.name}, cost: ${costFmt}, end: ${fmtDec(nEnd)}, batch: $i$reset"

          val all = showHere + showDisp.map("\n" + _).mkString
          val cs = go(children.get(n.id).map(_.toList).getOrElse(Nil))
          all + "\n" + cs
        }
        .mkString("")
    }

    prefix + go(tree.roots.map(_.id))
  }
}

object OptimizedDAG {
  type Plan = Map[NodeId, (Set[NodeId], PlanEnumeration.EndTime)]

  implicit lazy val showForPlannedNodeTree: Show[OptimizedDAG] = Show.show[OptimizedDAG](_.show())
}
