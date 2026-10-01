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
import munit.FunSuite

class NodeTreeTest extends FunSuite {
  private def node(id: Int, cost: Double, parents: Set[Int]): Node =
    Node(NodeId(id), s"node_$id", cost, 0d, parents.map(NodeId(_)), None)

  test("empty graph has no end times") {
    assert(NodeTree(Nil).endTimes.isEmpty)
  }

  test("unordered sparse DAG resolves shared parents and parallel paths") {
    val tree = NodeTree(
      List(
        node(909, 4d, Set(101, 405)),
        node(10005, 1d, Set(405)),
        node(405, 2d, Set(7)),
        node(101, 5d, Set(7)),
        node(7, 3d, Set.empty)
      )
    )
    assertEquals(
      tree.endTimes,
      Map(NodeId(7) -> 3d, NodeId(101) -> 8d, NodeId(405) -> 5d, NodeId(909) -> 12d, NodeId(10005) -> 6d)
    )
  }

  test("negative parent end times are not clamped to zero") {
    val tree = NodeTree(
      List(
        node(300, 3d, Set(200, 100)),
        node(400, -3d, Set(300)),
        node(200, 1d, Set(1)),
        node(100, -2d, Set.empty),
        node(1, -5d, Set.empty)
      )
    )
    assertEquals(
      tree.endTimes,
      Map(NodeId(1) -> -5d, NodeId(100) -> -2d, NodeId(200) -> -4d, NodeId(300) -> 1d, NodeId(400) -> -2d)
    )
  }

  test("deep reversed chain resolves without recursive traversal") {
    val size = 10000
    val tree = NodeTree(
      List.tabulate(size)(i => node(i * 17, 1d, if (i == 0) Set.empty else Set((i - 1) * 17))).reverse
    )
    assertEquals(tree.endTimes.size, size)
    assertEquals(tree.endTimes(NodeId(0)), 1d)
    assertEquals(tree.endTimes(NodeId((size - 1) * 17)), size.toDouble)
  }
}
