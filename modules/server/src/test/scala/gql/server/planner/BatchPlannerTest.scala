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

import cats.Id
import cats.effect.kernel.Unique
import gql.preparation.{NodeId, UniqueBatchInstance}
import gql.resolver.Step.BatchKey
import munit.FunSuite
import scala.collection.mutable
import scala.util.Random

class BatchPlannerTest extends FunSuite {
  private def problem(families: List[Int], children: List[List[Int]]): BatchPlanner.Problem =
    BatchPlanner.Problem(
      families
        .zip(children)
        .zipWithIndex
        .map { case ((family, cs), id) =>
          BatchPlanner.Node(id, family, mutable.BitSet.fromSpecific(cs))
        }
        .toArray
    )

  private def batches(solution: mutable.ArrayBuffer[(Int, mutable.BitSet)]): Vector[(Int, Set[Int])] =
    solution.iterator.map { case (family, participants) => family -> participants.toSet }.toVector

  test("empty graph has no batches") {
    val input = BatchPlanner.Problem(Array.empty[BatchPlanner.Node])
    assertEquals(batches(BatchPlanner.solve_(input)), Vector.empty)
    assertEquals(batches(BatchPlanner.solve(input)), Vector.empty)
    assert(Planner[Id].plan(NodeTree(Nil)).plan.isEmpty)
  }

  test("equal scores choose lowest family ID and include every ready member") {
    val input = problem(List(1, 0, 1), List(Nil, Nil, Nil))
    assertEquals(batches(BatchPlanner.solve_(input)), Vector(0 -> Set(1), 1 -> Set(0, 2)))
  }

  test("untouched family sum precedes previously touched zero") {
    val input = problem(List(2, 0, 1), List(Nil, List(2), Nil))
    assertEquals(batches(BatchPlanner.solve_(input)), Vector(0 -> Set(1), 2 -> Set(0), 1 -> Set(2)))
  }

  test("family selection and counters cross bitmap words") {
    val families = (0 until 130).toList ++ List(0, 0, 129, 128)
    val children = List.tabulate(134) {
      case 0   => List(130)
      case 130 => List(131)
      case 131 => List(132, 133)
      case _   => Nil
    }
    assertEquals(
      batches(BatchPlanner.solve_(problem(families, children))),
      (1 until 128).map(family => family -> Set(family)).toVector ++
        Vector(0 -> Set(0), 0 -> Set(130), 0 -> Set(131), 128 -> Set(128, 133), 129 -> Set(129, 132))
    )
  }

  test("repeated contraction combines batches missed by first pass") {
    val input = problem(List(2, 1, 1, 2, 0, 0), List(List(2), List(3), List(4), Nil, Nil, Nil))
    assertEquals(
      batches(BatchPlanner.solve_(input)),
      Vector(0 -> Set(5), 1 -> Set(1), 2 -> Set(0, 3), 1 -> Set(2), 0 -> Set(4))
    )
    assertEquals(
      batches(BatchPlanner.solve(input)),
      Vector(1 -> Set(1), 2 -> Set(0, 3), 1 -> Set(2), 0 -> Set(4, 5))
    )
  }

  test("diamond dependencies wait for both parents") {
    val input = problem(List(0, 1, 1, 2), List(List(1, 2), List(3), List(3), Nil))
    assertEquals(batches(BatchPlanner.solve(input)), Vector(0 -> Set(0), 1 -> Set(1, 2), 2 -> Set(3)))
  }

  test("seeded DAGs preserve coverage, family identity, dependency order and input") {
    val random = new Random(548545144077652L)
    (0 until 100).foreach { sample =>
      val size = 1 + random.nextInt(35)
      val familyCount = 1 + random.nextInt(math.min(size, 6))
      val families = List.tabulate(size)(i => if (i < familyCount) i else random.nextInt(familyCount))
      val children = List.tabulate(size)(i => ((i + 1) until size).filter(_ => random.nextInt(5) == 0).toList)
      val input = problem(families, children)
      val result = batches(BatchPlanner.solve(input))
      val assignments = result.zipWithIndex.flatMap { case ((_, participants), batchId) =>
        participants.map(_ -> batchId)
      }
      val nodeToBatch = assignments.toMap
      assertEquals(assignments.size, size, s"sample $sample")
      assertEquals(nodeToBatch.keySet, (0 until size).toSet, s"sample $sample")
      result.foreach { case (family, participants) =>
        assert(participants.nonEmpty, s"sample $sample")
        assertEquals(participants.map(families(_)), Set(family), s"sample $sample")
      }
      children.zipWithIndex.foreach { case (cs, parent) =>
        cs.foreach { child =>
          assert(nodeToBatch(parent) < nodeToBatch(child), s"sample $sample: $parent -> $child")
        }
      }
      assertEquals(input.nodes.iterator.map(_.children.toList).toList, children, s"sample $sample")
      assertEquals(batches(BatchPlanner.solve(input)), result, s"sample $sample")
    }
  }

  private def node(ids: Map[Int, NodeId])(id: Int, cost: Double, parents: Set[Int], batcher: Option[Int] = None): Node =
    Node(
      ids(id),
      s"node_$id",
      cost,
      0d,
      parents.map(ids),
      batcher.map(key => BatchRef(BatchKey[Int, Int](key), UniqueBatchInstance[Int, Int](ids(id))))
    )

  test("adapter groups batcher IDs across distinct instances and keeps nonbatch nodes separate") {
    val ids = List(10, 40, 90, 120, 160).map(i => i -> NodeId(new Unique.Token)).toMap
    val tree = NodeTree(
      List(
        node(ids)(10, 5d, Set.empty, Some(7)),
        node(ids)(40, 5d, Set.empty, Some(7)),
        node(ids)(90, 5d, Set.empty, Some(8)),
        node(ids)(120, 5d, Set.empty),
        node(ids)(160, 5d, Set.empty)
      )
    )
    val result = Planner[Id].plan(tree)
    assertEquals(
      result.batches.map { case (participants, _) => participants },
      Set(Set(ids(10), ids(40)), Set(ids(90)), Set(ids(120)), Set(ids(160)))
    )
    assertEquals(result.plan.keySet, tree.lookup.keySet)
  }

  test("adapter keeps parallel end times and uses latest parent of every batch member") {
    val ids = List(100, 300, 900, 1100, 1500).map(i => i -> NodeId(new Unique.Token)).toMap
    val tree = NodeTree(
      List(
        node(ids)(100, 3d, Set.empty),
        node(ids)(300, 7d, Set.empty),
        node(ids)(900, 5d, Set(100), Some(7)),
        node(ids)(1100, 5d, Set(300), Some(7)),
        node(ids)(1500, 2d, Set(900))
      )
    )
    val result = Planner[Id].plan(tree)
    assertEquals(result.plan(ids(100)), Set(ids(100)) -> PlanEnumeration.EndTime(3d))
    assertEquals(result.plan(ids(300)), Set(ids(300)) -> PlanEnumeration.EndTime(7d))
    assertEquals(result.plan(ids(900)), Set(ids(900), ids(1100)) -> PlanEnumeration.EndTime(12d))
    assertEquals(result.plan(ids(1100)), result.plan(ids(900)))
    assertEquals(result.plan(ids(1500)), Set(ids(1500)) -> PlanEnumeration.EndTime(14d))
  }
}
