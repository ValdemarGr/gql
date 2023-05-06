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

final case class NodeTree(all: List[Node]) {
  lazy val lookup = all.map(n => n.id -> n).toMap

  lazy val roots = all.filter(_.parents.isEmpty)

  lazy val reverseLookup: Map[NodeId, List[NodeId]] = all
    .flatMap(n => n.parents.toList tupleRight n.id)
    .groupMap { case (k, _) => k } { case (_, v) => v }

  lazy val endTimes: Map[NodeId, Double] = {
    val l = lookup

    def go(id: NodeId): State[Map[NodeId, Double], Double] =
      State.inspect { cache: Map[NodeId, Double] => cache.get(id) }.flatMap {
        case Some(e) => State.pure(e)
        case None =>
          val n = l(id)
          n.parents.toList
            .traverse(go)
            .map(_.maxOption.getOrElse(0d) + n.cost)
            .flatTap(e => State.modify(_ + (id -> e)))
      }

    roots.traverse_(x => go(x.id)).runS(Map.empty).value
  }
}

final case class OptimizedDAG(
    tree: NodeTree,
    plan: Map[NodeId, (Set[NodeId], PlanEnumeration.EndTime)]
) {
  lazy val batches: Set[(Set[NodeId], PlanEnumeration.EndTime)] = plan.values.toSet

  lazy val totalCost: Double =
    batches.map { case (b, _) => b }.unorderedFoldMap(b => tree.lookup(b.head).cost * b.size)

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
        plan.as {
          s"""|
          |${red}old field schedule$reset
          |${green}new field offset (deferral of execution)$reset
          |""".stripMargin
        }.mkString
      else ""

    val per = math.max((maxEnd / 40d), 1)

    val enumeratedBatches = plan.toList.zipWithIndex.map { case ((k, (v1, v2)), i) => (k, (v1, v2, i)) }.toMap

    def go(nodes: List[NodeId]): String = {
      nodes
        .sortBy(_.id)
        .map { n0 =>
          val n = lookup(n0)
          val nEnd = endTimes(n.id)
          val nStart = nEnd - n.cost
          val basePrefix = " " * (nStart / per).toInt
          val showDisp = enumeratedBatches
            .get(n.id)
            .filter { case (_, d, _) => d.time != nEnd }
            .map { case (_, dEnd, i) =>
              val dStart = dEnd.time - n.cost
              val pushedPrefix = blue + ">" * ((dStart - nStart) / per).toInt + green
              s"$basePrefix${pushedPrefix}name: ${n.name}, cost: ${n.cost}, end: ${dEnd}$reset, batch: $i"
            }
          val showHere =
            s"$basePrefix${if (showDisp.isDefined) red else ""}name: ${n.name}, cost: ${n.cost}, end: ${nEnd}$reset"

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
  implicit lazy val showForPlannedNodeTree: Show[OptimizedDAG] = Show.show[OptimizedDAG](_.show())
}
