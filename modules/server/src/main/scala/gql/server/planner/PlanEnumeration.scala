/*
 * Copyright 2024 Valdemar Grange
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
import fs2.{Pure, Stream}
import gql.preparation.NodeId

object PlanEnumeration {
  final case class FamilyId(id: Int) extends AnyVal

  final case class Family(
      cost: Double,
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
  final case class EndTime(time: Double) extends AnyVal
  object EndTime {
    implicit val ordering: Ordering[EndTime] = Ordering.by(_.time)
  }

  final case class Batch(
      nodes: Set[NodeId],
      end: EndTime
  )

  final case class EnumerationState(
      scheduled: Map[NodeId, Batch],
      forbidden: Set[Set[NodeId]]
  )

  def enumerateAll(problem: Problem): Stream[Pure, Map[NodeId, Batch]] = {
    def go(current: EnumerationState): Stream[Pure, EnumerationState] =
      if (current.scheduled.size === problem.all.size) Stream(current)
      else enumerateNextRound(problem, current).flatMap(go)

    go(EnumerationState(Map.empty, Set.empty)).map(_.scheduled)
  }

  // Actual performance of this function can be significantly improved by efficiently recomputing various metrics
  def enumerateNextRound(problem: Problem, state: EnumerationState): Stream[Pure, EnumerationState] = {
    def scanFamilies(x: NodeId): Set[FamilyId] =
      problem.arcs.getOrElse(x, Nil).flatMap(scanFamilies).toSet ++ problem.familyMap.get(x).toList

    val interesting: Set[NodeId] = problem.all.filter { id =>
      (problem.reverseArcs.getOrElse(id, Set.empty) subsetOf state.scheduled.keySet) &&
      !state.scheduled.contains(id)
    }.toSet

    val grouped: Map[FamilyId, Set[NodeId]] = interesting.groupBy(problem.familyMap(_))

    val pruneable: List[NodeId] = grouped.values.toList
      .mapFilter(s => s.headOption.filter(_ => s.size == 1))
      .mapFilter { id =>
        val rest = interesting - id
        val otherFamilies = rest.flatMap(scanFamilies)
        if (otherFamilies.contains(problem.familyMap(id))) None
        else Some(id)
      }

    pruneable.toNel match {
      case Some(p) =>
        // pruneable nodes will never have any other solutions, so we can just schedule them
        // if any of the pruneable nodes are in the forbidden set, that means that we have already checked a plan
        // that includes scheduling that one pruneable node, so we can skip this plan entirely
        // since the other plan that scheduled that one node must have checked the same schedule
        val isDuplicate = p.exists(n => state.forbidden.contains(Set(n)))

        if (isDuplicate) Stream.empty
        else {
          val news = p.toList.map { n =>
            val parents = problem.reverseArcs.getOrElse(n, Set.empty)
            val maxParentEnd: EndTime = parents
              .map(p => state.scheduled(p).end)
              .maxOption
              .getOrElse(EndTime(0))
            val thisEnd = EndTime(maxParentEnd.time + problem.families(problem.familyMap(n).id).cost)
            n -> Batch(Set(n), thisEnd)
          }

          Stream(
            EnumerationState(
              scheduled = state.scheduled ++ news,
              forbidden = state.forbidden
            )
          )
        }
      case None =>
        val m = grouped.toList.map(_._2.size).maxOption.getOrElse(0)
        val o: Stream[Pure, Set[NodeId]] = Stream
          .emits((1 to m).reverse)
          .flatMap { size =>
            val relevantHere = grouped.values.filter(_.size >= size).toList
            Stream.emits(relevantHere).flatMap(c => fs2.Stream.iterable(Iterable.from(c.toSeq.combinations(size))))
          }
          .map(_.toSet)
          .filter(s => !state.forbidden.contains(s))

        o.mapAccumulate(state.forbidden) { case (forbid, batch) =>
          val batchEndTime = batch
            .flatMap { x =>
              val parents = problem.reverseArcs.getOrElse(x, Set.empty)
              parents.map(y => state.scheduled(y).end)
            }
            .maxOption
            .getOrElse(EndTime(0))
          val thisEnd = EndTime(batchEndTime.time + problem.families(problem.familyMap(batch.head).id).cost)
          val forbid2 = forbid + batch
          forbid2 -> EnumerationState(
            state.scheduled ++ batch.map(b => b -> Batch(batch, thisEnd)),
            forbid + batch
          )
        }.map { case (_, x) => x }
    }
  }
}
