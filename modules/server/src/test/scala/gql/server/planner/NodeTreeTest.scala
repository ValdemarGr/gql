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

class NodeTreeTest extends FunSuite {
  private def node(ids: Map[Int, NodeId])(id: Int, cost: Double, parents: Set[Int]): Node =
    Node(ids(id), s"node_$id", cost, 0d, parents.map(ids), None)

  test("empty graph has no end times") {
    assert(NodeTree(Nil).endTimes.isEmpty)
  }

  test("unordered sparse DAG resolves shared parents and parallel paths") {
    val ids = List(7, 101, 405, 909, 10005).map(i => i -> NodeId(new Unique.Token)).toMap
    val tree = NodeTree(
      List(
        node(ids)(909, 4d, Set(101, 405)),
        node(ids)(10005, 1d, Set(405)),
        node(ids)(405, 2d, Set(7)),
        node(ids)(101, 5d, Set(7)),
        node(ids)(7, 3d, Set.empty)
      )
    )
    assertEquals(
      tree.endTimes,
      Map(ids(7) -> 3d, ids(101) -> 8d, ids(405) -> 5d, ids(909) -> 12d, ids(10005) -> 6d)
    )
  }

  test("negative parent end times are not clamped to zero") {
    val ids = List(1, 100, 200, 300, 400).map(i => i -> NodeId(new Unique.Token)).toMap
    val tree = NodeTree(
      List(
        node(ids)(300, 3d, Set(200, 100)),
        node(ids)(400, -3d, Set(300)),
        node(ids)(200, 1d, Set(1)),
        node(ids)(100, -2d, Set.empty),
        node(ids)(1, -5d, Set.empty)
      )
    )
    assertEquals(
      tree.endTimes,
      Map(ids(1) -> -5d, ids(100) -> -2d, ids(200) -> -4d, ids(300) -> 1d, ids(400) -> -2d)
    )
  }

  test("deep reversed chain resolves without recursive traversal") {
    val size = 10000
    val ids = (0 until size).map(i => i * 17 -> NodeId(new Unique.Token)).toMap
    val tree = NodeTree(
      List.tabulate(size)(i => node(ids)(i * 17, 1d, if (i == 0) Set.empty else Set((i - 1) * 17))).reverse
    )
    assertEquals(tree.endTimes.size, size)
    assertEquals(tree.endTimes(ids(0)), 1d)
    assertEquals(tree.endTimes(ids((size - 1) * 17)), size.toDouble)
  }
}
