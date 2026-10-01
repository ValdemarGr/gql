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

import scala.collection.mutable
import scala.collection.mutable.BitSet

private[planner] object BatchPlanner {
  final case class Node(id: Int, family: Int, children: BitSet)

  final case class Problem(nodes: Array[Node]) {
    def size: Int = nodes.length

    val parents: Array[BitSet] = Array.fill(size)(BitSet.empty)
    nodes.foreach { node =>
      node.children.foreach { child =>
        parents(child).addOne(node.id)
      }
    }
  }

  def solve_(problem: Problem): mutable.ArrayBuffer[(Int, BitSet)] = {
    val batches = mutable.ArrayBuffer.empty[(Int, BitSet)]
    val subtreeFamily = new Array[BitSet](problem.size)

    locally {
      val visited = BitSet.empty
      val leaves = BitSet.fromSpecific(problem.nodes.iterator.filter(_.children.isEmpty).map(_.id))
      val toVisit = mutable.Stack.from(leaves)
      while (toVisit.nonEmpty) {
        val nodeId = toVisit.pop()
        if (!visited.contains(nodeId)) {
          val node = problem.nodes(nodeId)
          if (node.children.subsetOf(visited)) {
            val res = BitSet.empty
            node.children.foreach { child =>
              res |= subtreeFamily(child)
              res.addOne(problem.nodes(child).family)
            }
            subtreeFamily(nodeId) = res
            visited.addOne(nodeId)
            problem.parents(nodeId).foreach { parent =>
              if (problem.nodes(parent).children.subsetOf(visited) && !visited.contains(parent)) toVisit.push(parent)
            }
          }
        }
      }
    }

    val familyCount = BitSet.fromSpecific(problem.nodes.iterator.map(_.family)).size
    val nonEmptyFamilies = BitSet.empty
    // The source planner distinguishes untouched (-1) counters from counters that have returned to zero.
    val familySums = Array.fill(familyCount)(-1)
    val familyCounts = new Array[Array[Int]](familyCount)
    val familyMembers = new Array[BitSet](familyCount)
    val visited = BitSet.empty
    val resolved = BitSet.empty
    var resolvedCount = 0

    def checkNodeFree(nodeId: Int): Unit = {
      val node = problem.nodes(nodeId)
      if (problem.parents(nodeId).subsetOf(resolved) && !visited.contains(nodeId)) {
        visited.addOne(nodeId)
        if (familyMembers(node.family) == null) familyMembers(node.family) = BitSet.empty
        familyMembers(node.family).addOne(nodeId)
        nonEmptyFamilies.addOne(node.family)
        if (subtreeFamily(nodeId).nonEmpty && familyCounts(node.family) == null) {
          familyCounts(node.family) = Array.fill(familyCount)(-1)
        }
        val counts = familyCounts(node.family)
        subtreeFamily(nodeId).foreach { family =>
          familySums(family) = (if (familySums(family) == -1) 0 else familySums(family)) + 1
          counts(family) = (if (counts(family) == -1) 0 else counts(family)) + 1
        }
      }
    }

    def scheduleBatch(family: Int, batch: BitSet): Unit = {
      batches.addOne((family, batch))
      val counts = familyCounts(family)
      resolved |= batch
      if (counts != null) {
        var otherFamily = 0
        while (otherFamily < counts.length) {
          val count = counts(otherFamily)
          if (count != -1) {
            val sum = if (familySums(otherFamily) == -1) 0 else familySums(otherFamily)
            familySums(otherFamily) = sum - count
          }
          otherFamily += 1
        }
      }
      familyCounts(family) = null
      nonEmptyFamilies.subtractOne(family)
      familyMembers(family) = null
      batch.foreach { nodeId =>
        resolvedCount += 1
        problem.nodes(nodeId).children.foreach(checkNodeFree)
      }
    }

    problem.parents.indices.foreach { nodeId =>
      if (problem.parents(nodeId).isEmpty) checkNodeFree(nodeId)
    }

    while (resolvedCount != problem.size) {
      var bestScore = Int.MaxValue
      var bestFamily = -1
      nonEmptyFamilies.foreach { family =>
        val counts = familyCounts(family)
        val selfCount = if (counts == null) -1 else counts(family)
        val score = familySums(family) - (if (selfCount == -1) 0 else selfCount)
        if (score < bestScore) {
          bestScore = score
          bestFamily = family
        }
      }
      assert(bestFamily != -1, "There should always be a best family when there are unresolved nodes")
      scheduleBatch(bestFamily, familyMembers(bestFamily))
    }
    assert(nonEmptyFamilies.isEmpty, "All families should be empty at the end of scheduling")
    batches
  }

  def solve(problem: Problem): mutable.ArrayBuffer[(Int, BitSet)] = {
    var currentProblem = problem
    var reverseMap: (Int, BitSet) => Unit = null
    var previousProblemSize = -1
    while (currentProblem.size != previousProblemSize) {
      previousProblemSize = currentProblem.size
      val solution = solve_(currentProblem).toArray

      val nodeToBatch = new Array[Int](currentProblem.size)
      solution.indices.foreach { batchId =>
        solution(batchId)._2.foreach { nodeId =>
          nodeToBatch(nodeId) = batchId
        }
      }

      val newProblem = Array.tabulate(solution.length) { batchId =>
        val (family, batch) = solution(batchId)
        val children = BitSet.empty
        batch.foreach { nodeId =>
          currentProblem.nodes(nodeId).children.foreach { child =>
            children.addOne(nodeToBatch(child))
          }
        }
        Node(batchId, family, children)
      }

      val reverseMapping = solution.map(_._2)
      val oldReverseMap = reverseMap
      reverseMap = { (batchId: Int, bitset: BitSet) =>
        if (oldReverseMap == null) bitset |= reverseMapping(batchId)
        else reverseMapping(batchId).foreach(nodeId => oldReverseMap(nodeId, bitset))
      }
      currentProblem = Problem(newProblem)
    }

    val results = mutable.ArrayBuffer.empty[(Int, BitSet)]
    currentProblem.nodes.foreach { node =>
      val participants = BitSet.empty
      reverseMap(node.id, participants)
      results.addOne((node.family, participants))
    }
    results
  }
}
