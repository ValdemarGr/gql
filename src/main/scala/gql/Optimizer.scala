package gql

import cats.effect._
import cats.implicits._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import cats.data._
import gql.PreparedQuery.PreparedDataField
import gql.PreparedQuery.PreparedFragField
import gql.PreparedQuery.PreparedLeaf
import gql.PreparedQuery.PreparedList
import gql.PreparedQuery.Selection
import cats.Monad
import cats.mtl.Stateful

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
  final case class Node(id: Int, name: String, cost: Double, end: Double, children: List[Node], elemCost: Double) {
    lazy val start = end - cost
  }

  def constructCostTree[F[_]](currentCost: Double, prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]])(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[NonEmptyList[Node]] = {
    prepared.flatTraverse {
      case PreparedDataField(id, name, resolve, selection, bn) =>
        // // TODO, use the typename, not the field name
        // val bn = meta.batchName.getOrElse(name)
        stats
          .getStatsOpt(bn)
          .map {
            case None    => Statistics.Stats(100d, 5d)
            case Some(x) => x
          }
          .flatMap { s =>
            val end = currentCost + s.initialCost

            def handleSelection(p: PreparedQuery.Prepared[F, Any]): F[Node] =
              p match {
                case PreparedLeaf(_, _) =>
                  F.pure(Node(id, bn, s.initialCost, end, Nil, s.extraElementCost))
                case Selection(fields) =>
                  constructCostTree[F](end, fields)
                    .map(nel => Node(id, bn, s.initialCost, end, nel.toList, s.extraElementCost))
                case PreparedList(of) => handleSelection(of)
              }

            handleSelection(selection).map(NonEmptyList.one(_))
          }
      case PreparedFragField(_, _, selection) =>
        constructCostTree[F](currentCost, selection.fields) //.map(c =>
      // NonEmptyList.one(Node(id, s"frag-${id.toString()}", 1, 1, c.toList, 1))
      // )
    }
  }

  def costTree[F[_]: Monad](
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit stats: Statistics[F]): F[NonEmptyList[Node]] =
    constructCostTree[F](0d, prepared)

  def flattenNodeTree(xs: NonEmptyList[Node]): NonEmptyList[Node] =
    xs.flatMap {
      case n @ Node(_, _, _, _, Nil, _)     => NonEmptyList.one(n)
      case n @ Node(_, _, _, _, x :: xs, _) => NonEmptyList.one(n) ++ flattenNodeTree(NonEmptyList(x, xs)).toList
    }

  def plan(nodes: NonEmptyList[Node]): NonEmptyList[Node] = {
    val flatNodes = flattenNodeTree(nodes)

    val orderedFlatNodes = flatNodes.sortBy(_.end).reverse

    val m = orderedFlatNodes.head.end

    def go(remaining: List[Node], handled: List[Node]): List[Node] =
      remaining match {
        case Nil => handled
        case r :: rs =>
          val maxEnd: Double = r.children match {
            case Nil     => m
            case x :: xs => NonEmptyList(x, xs).map(_.start).minimum
          }

          val compatible =
            handled
              .filter(h => h.name == r.name && r.end <= h.end && h.end <= maxEnd)
              .maximumByOption(_.end)

          val newEnd = compatible.map(_.end).getOrElse(maxEnd)

          // println(s"minimum end for ${r.id} is $maxEnd and compatible is ${compatible.map(_.id)} such that end is $newEnd")

          go(rs, r.copy(end = newEnd) :: handled)
      }

    val planned = go(orderedFlatNodes.toList, Nil)

    val plannedMap = planned.map(n => n.id -> n).toMap

    def reConstruct(ns: NonEmptyList[Int]): NonEmptyList[Node] =
      ns.map { n =>
        val newN = plannedMap(n)
        newN.children match {
          case Nil     => newN
          case x :: xs => newN.copy(children = reConstruct(NonEmptyList(x, xs).map(_.id)).toList)
        }
      }

    reConstruct(nodes.map(_.id))
  }
}
