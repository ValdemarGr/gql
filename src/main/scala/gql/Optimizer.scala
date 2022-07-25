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
 * cost(A) = 1
 *
 *        ____A
 *       /   / \
 *      D   B   E
 *     / \  |   |  
 *    A   D C   A   
 *  / |     |   | \
 * E  C     A   D  D
 * |  |    / \
 * D  D   D   D
 *        
 *        
 *      A           
 *     / \
 *    B   E
 *    |   | \
 *    C   D  D
 *   / \            
 *  D  D            
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
  def schedule[F[_]](implicit stats: Statistics[F]) = {
  }
}
