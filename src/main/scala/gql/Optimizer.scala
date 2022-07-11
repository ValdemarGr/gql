package gql

import cats.implicits._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import cats.data._

/*
 * The executable GraphQL query is a tree of potentially effectful functions to run.
 * The optimizers job is to pick the most efficient way to evaluate this tree.
 * Queries are batched by running all nodes on the same level of the tree (potentially) together.
 * Let A be a node with an arbitary number of children: |children(A)| \geq 0
 * Let Defer be a special node that lets the interpreter defer evalutaion until the next level.
 * Let [A] be a contracted set of nodes that can be effeciently evaluated together.
 * As an example consider rewriting the following query:
 *            A____
 *           / \   \
 *          B  D   D
 *        / \
 *       D  D
 * To:
 *            A
 *           / \
 *          B Defer
 *        / \  / \
 *       D  D D   D
 *
 * A naive implementation that just merges parents could increase the running time of the query.
 * Let cost(A) be the time it takes to evaluate A.
 * Let (A, B) be the node that consists of A and B where the children of (A, B) are children(A) \cup children(B).
 * Evaluation of (A, B) may (and will) occur in parallel, thus the cost((A, B)) = max(cost(A), cost(B)).
 * Observe the following rewrite and costs:
 *
 * cost(B) = 5
 * cost(C) = 5
 * cost(E) = 10
 * cost(D) = 10
 *
 *            A
 *           / \
 *          B   E
 *          |   | \
 *          C   D  D
 *         / \
 *        D  D
 *
 *            A
 *            |
 *          (B, E)
 *            |
 *        (C, Defer)
 *            |
 *           [D]
 *
 * Calculating the maximum time of execution for the first query max(B(5) + C(5) + D(10), E(10) + D(10)) = 20
 * And the second = max(B(5), E(10)) + C(5) + D(10) = 25
 * Clearly the running time has been traded for more batching.
 * In fact, a tree with better running time cannot be constructed,
 * since every node is already the shortest distance from root.
 * There are a couple of choices to be made here which essentially boil down to how one chooses to calculate cost.
 * How does one measure 'batchability', the reduction in cost of batching?
 * Maybe for a contraction [V] where the naive execution cost is |[V]| * cost(V),
 * the 'batchability' could be measured by cost(V) - (|[V]| - 1) * b(V) where b(V) is the cost savings of batching?.
 * By using such a method we can adjust the aggressiveness of the query rewriter.
 *
 * The question that begs now, is how?
 * We could start bottom up maybe?
 * optimize(N) =
 *  if N is a leaf node return (N, 1, 1)
 *  else
 *    let o = children.map(optimize)
 *    return merge(o).map{ (node, count, level) => (node, count, (level + 1))}
 *
 * merge(o) =
 *  nodes = o.map{ (node, _, _) => node}.toSet
 *  nodes.map { node =>
 *    // find nodes that are at this level
 *  }
 *
 * 1) assign start og sluttidspunkt på alle nodes i forhold til eksekvering uden batching
 *
 * 2) gennemløb hver knude prioriteret efter seneste sluttidspunkt. Hvis der er batchingmuligheder,
 *    tag den senest mulige, ellers assign bare senest mulige sluttidspunkt
 */
object Optimizer {
  type Level = Int
  final case class NodeInfo(name: String, count: Int, level: Int)
  type S = SortedMap[Level, List[NodeInfo]]
  sealed trait OptimizedNode
  final case class OptimizedLeafValue(ni: NodeInfo) extends OptimizedNode
  final case class OptimizedNodeValue(ni: NodeInfo, children: List[OptimizedNode]) extends OptimizedNode

  sealed trait BaseNode
  final case class BaseLeafValue(name: String) extends BaseNode
  final case class BaseNodeValue(name: String, children: List[BaseNode]) extends BaseNode

  sealed trait OptimizedNodeType
  final case class OptimizedParallelNode(nodes: List[OptimizedNodeType]) extends OptimizedNodeType
  final case class OptimizedDeferNode(child: OptimizedNodeType) extends OptimizedNodeType
  final case class OptimizedLeafNode(name: String) extends OptimizedNodeType
  final case class OptimizedBatchNode(name: String, count: Int, children: List[OptimizedNodeType]) extends OptimizedNodeType

  def level(node: OptimizedNodeType, n: Int = 1): Int = node match {
    case OptimizedParallelNode(nodes)       => nodes.map(level(_, n + 1)).maxOption.getOrElse(n)
    case OptimizedDeferNode(child)          => level(child, n + 1)
    case OptimizedLeafNode(_)               => n
    case OptimizedBatchNode(_, _, children) => children.map(level(_, n + 1)).maxOption.getOrElse(n)
  }

  type NodeMap = Map[String, SortedMap[Int, List[OptimizedNodeType]]]

  def merge3(left: SortedMap[Int, List[OptimizedNodeType]], right: SortedMap[Int, List[OptimizedNodeType]]) = {
    left.foldLeft((null: NodeMap)) { case (acc, (level, nodes)) =>
      ???
    }
  }

  def merge2(nms: NonEmptyList[NodeMap]) = {
    nms.foldLeft((null: NodeMap)) { case (accum, next) =>
      next.map{ case (k, v) =>
        accum.get(k) match {
          case None => SortedMap((k -> v))
          case Some(accumV) =>
            merge3(accumV, v)
            ???
        }
      }

      ???
    }
  }

  def merge(nodes: NonEmptyList[OptimizedNodeType]) = {
    val maxLevel = nodes.map(level(_, 1)).maximum
  }

  def optimize(node: BaseNode, level: Int): OptimizedNode = node match {
    case BaseLeafValue(name) => OptimizedLeafValue(NodeInfo(name, 1, level))
    case BaseNodeValue(name, children) =>
      children.map(optimize(_, level + 1))
      OptimizedNodeValue(NodeInfo(name, 1, level), children.map(optimize(_, level + 1)))
  }

  // def optimize[F[_]]()
}
