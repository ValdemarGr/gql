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

import gql.preparation.NodeId
import cats.effect.kernel.Unique
import munit.FunSuite

class OptimizedDAGTest extends FunSuite {
  test("empty plan has no batches or costs") {
    val dag = OptimizedDAG(NodeTree(Nil), Set.empty)
    assert(dag.batches.isEmpty)
    assert(dag.plan.isEmpty)
    assertEquals(dag.totalCost, 0d)
    assertEquals(dag.optimizedCost, 0d)
  }

  test("node lookup expands batches with their shared participants and end times") {
    val ids = Vector.fill(28)(NodeId(new Unique.Token))
    val members = (0 until 8).map(ids).toSet
    val shared = members -> PlanEnumeration.EndTime(10d)
    val singletons = (20 until 28).map(i => Set(ids(i)) -> PlanEnumeration.EndTime(i.toDouble))
    val batches = singletons.toSet + shared
    val dag = OptimizedDAG(NodeTree(Nil), batches)
    assertEquals(dag.plan.keySet, members ++ singletons.flatMap(_._1))
    members.foreach(id => assertEquals(dag.plan(id), shared))
    singletons.foreach { batch =>
      assertEquals(dag.plan(batch._1.head), batch)
    }
    assertEquals(dag.copy(batches = Set(shared)).plan.keySet, members)
  }

  test("cost metrics use the supplied batch order") {
    val ids = Vector.fill(9)(NodeId(new Unique.Token))
    val first = Set(ids(0), ids(1), ids(2)) -> PlanEnumeration.EndTime(1d)
    val second = Set(ids(3), ids(4), ids(5)) -> PlanEnumeration.EndTime(2d)
    val third = Set(ids(6), ids(7), ids(8)) -> PlanEnumeration.EndTime(3d)
    val batches = Set(first, second, third)
    val nodes = List.tabulate(9) { i =>
      val cost = if (i < 3) 1e16 else if (i < 6) 1d else -1e16
      Node(ids(i), s"node_$i", cost, 0d, Set.empty, None)
    }
    val dag = OptimizedDAG(NodeTree(nodes), batches)
    assertEquals(dag.batches.toList, List(first, second, third))
    assertEquals(dag.totalCost, 4d)
    assertEquals(dag.optimizedCost, 0d)
  }
}
