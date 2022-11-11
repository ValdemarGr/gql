/*
 * Copyright 2022 Valdemar Grange
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
package gql

import gql.resolver._
import cats.implicits._
import cats.data._
import gql.PreparedQuery.PreparedDataField
import gql.PreparedQuery.PreparedFragField
import gql.PreparedQuery.PreparedLeaf
import gql.PreparedQuery.PreparedList
import gql.PreparedQuery.PreparedOption
import gql.PreparedQuery.Selection
import scala.collection.immutable.TreeSet
import cats._
import scala.io.AnsiColor

trait Planner[F[_]] { self =>
  def plan(naive: Planner.NodeTree): F[Planner.NodeTree]

  def mapK[G[_]](fk: F ~> G): Planner[G] =
    new Planner[G] {
      def plan(naive: Planner.NodeTree): G[Planner.NodeTree] = fk(self.plan(naive))
    }
}

object Planner {
  final case class Node(
      name: String,
      end: Double,
      cost: Double,
      elemCost: Double,
      children: List[Node],
      batcher: Option[BatchResolver.ResolverKey],
      edgeId: PreparedQuery.EdgeId
  ) {
    lazy val start = end - cost
  }

  def costForPrepared[F[_]: Statistics](p: PreparedQuery.Prepared[F, Any], currentCost: Double)(implicit F: Monad[F]): F[List[Node]] =
    p match {
      case PreparedLeaf(_, _)    => F.pure(Nil)
      case Selection(fields)     => costForFields[F](currentCost, fields).map(_.toList)
      case PreparedList(cont, _) => costForCont[F](cont.edges.toList, cont.cont, currentCost)
      case PreparedOption(cont)  => costForCont[F](cont.edges.toList, cont.cont, currentCost)
    }

  def costForEdges[F[_]](pes: List[PreparedQuery.PreparedEdge.Edge[F]], cont: PreparedQuery.Prepared[F, Any], currentCost: Double)(implicit
      stats: Statistics[F],
      F: Monad[F]
  ): F[List[Node]] =
    pes match {
      case Nil => costForPrepared[F](cont, currentCost)
      case x :: xs =>
        val resolverKey = x.resolver match {
          case PreparedQuery.PreparedResolver.Batch(BatchResolver(id, _)) => Some(id)
          case _                                                          => None
        }

        stats
          .getStatsOpt(x.statisticsName)
          .map {
            case None    => Statistics.Stats(100d, 5d)
            case Some(x) => x
          }
          .flatMap { s =>
            val end = currentCost + s.initialCost

            val childrenF = costForEdges[F](xs, cont, end).map(_.toList)

            childrenF.map { children =>
              List(
                Node(
                  x.statisticsName,
                  end,
                  s.initialCost,
                  s.extraElementCost,
                  children.toList,
                  resolverKey,
                  x.id
                )
              )
            }
          }
    }

  def costForCont[F[_]: Statistics: Monad](
      edges: List[PreparedQuery.PreparedEdge[F]],
      cont: PreparedQuery.Prepared[F, Any],
      currentCost: Double
  ): F[List[Node]] =
    costForEdges[F](
      edges.toList.collect { case e: PreparedQuery.PreparedEdge.Edge[F] => e },
      cont,
      currentCost
    )

  def costForFields[F[_]](
      currentCost: Double,
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[List[Node]] = {
    prepared.toList.flatTraverse {
      case PreparedDataField(_, _, _, cont)      => costForCont[F](cont.edges.toList, cont.cont, currentCost)
      case PreparedFragField(_, _, _, selection) => costForFields[F](currentCost, selection.fields)
    }
  }

  def costTree[F[_]: Monad](
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit stats: Statistics[F]): F[NodeTree] =
    costForFields[F](0d, prepared).map(xs => NodeTree(xs.toList))

  def apply[F[_]](implicit F: Applicative[F]) = new Planner[F] {
    def plan(tree: NodeTree): F[NodeTree] = {
      val flatNodes = tree.flattened

      val orderedFlatNodes = flatNodes.sortBy(_.end).reverse

      val m = orderedFlatNodes.head.end

      // go through every node sorted by end decending
      // if the node has a batching name, move node to lastest possible batch (frees up most space for parents to move)
      def go(
          remaining: List[Node],
          handled: Map[PreparedQuery.EdgeId, Node],
          batchMap: Map[BatchResolver.ResolverKey, Eval[TreeSet[Double]]]
      ): Map[PreparedQuery.EdgeId, Node] =
        remaining match {
          case Nil     => handled
          case r :: rs =>
            // the maximum amount we can move down is the child with smallest start
            val maxEnd: Double = r.children match {
              case Nil     => m
              case x :: xs =>
                // use the already resolved if possible
                val children = NonEmptyList(x, xs)

                children.map(c => handled.get(c.edgeId).getOrElse(c).start).minimum
            }

            val (newEnd, newMap) =
              r.batcher match {
                // No batching, free up as much space as possible for parents to move
                case None     => (maxEnd, batchMap)
                case Some(bn) =>
                  // Find nodes that we may move to:
                  // All nodes that end no later than the earliest of our children but end later than us
                  val compat =
                    batchMap
                      .get(bn)
                      .flatMap { m =>
                        val o = if (m.value.contains(maxEnd)) Some(maxEnd) else m.value.maxBefore(maxEnd)
                        o.filter(_ >= r.end)
                      }
                      .getOrElse(maxEnd)

                  val newSet =
                    batchMap.get(bn) match {
                      case None    => Eval.later(TreeSet(r.end))
                      case Some(s) => s.map(_ + r.end)
                    }
                  val newMap = batchMap + (bn -> newSet)

                  (compat, newMap)
              }

            go(rs, handled + (r.edgeId -> r.copy(end = newEnd)), newMap)
        }

      val plannedMap = go(orderedFlatNodes.toList, Map.empty, Map.empty)

      def reConstruct(ns: List[PreparedQuery.EdgeId]): Eval[List[Node]] = Eval.defer {
        ns.traverse { n =>
          val newN = plannedMap(n)
          val newChildrenF = reConstruct(newN.children.map(_.edgeId)).map(_.toList)
          newChildrenF.map(x => newN.copy(children = x))
        }
      }

      F.pure(tree.set(reConstruct(tree.root.map(_.edgeId)).value))
    }
  }

  final case class NodeTree(
      root: List[Node],
      source: Option[NodeTree] = None
  ) {
    def set(newRoot: List[Node]): NodeTree =
      NodeTree(newRoot, Some(this))

    lazy val flattened: List[Node] = {
      def go(xs: List[Node]): Eval[List[Node]] = Eval.defer {
        xs.flatTraverse {
          case n @ Node(_, _, _, _, Nil, _, _) => Eval.now(List(n))
          case n @ Node(_, _, _, _, xs, _, _)  => go(xs).map(n :: _)
        }
      }

      go(root).value
    }

    lazy val batches: List[(BatchResolver.ResolverKey, NonEmptyChain[PreparedQuery.EdgeId])] =
      flattened
        .map(n => (n.batcher, n))
        .collect { case (Some(batcherKey), node) => (batcherKey, node) }
        .groupByNec { case (_, node) => node.end }
        .toList
        .flatMap { case (_, endGroup) =>
          endGroup
            .groupBy { case (batcherKey, _) => batcherKey }
            .toSortedMap
            .toList
            .map { case (batcherKey, batch) =>
              batcherKey -> batch.map { case (_, node) => node.edgeId }
            }
        }

    def totalCost: Double = {
      val thisFlat = flattened
      val thisFlattenedMap = thisFlat.map(n => n.edgeId -> n).toMap
      val thisBatches = batches.filter { case (_, edges) => edges.size > 1 }

      val naiveCost = thisFlat.map(_.cost).sum

      val batchSubtraction = thisBatches.map { case (_, xs) =>
        // cost * (size - 1 )
        thisFlattenedMap(xs.head).cost * (xs.size - 1)
      }.sum

      naiveCost - batchSubtraction
    }

    def show(showImprovement: Boolean = false, ansiColors: Boolean = false) = {
      val (default, displaced) =
        if (showImprovement)
          source match {
            case Some(x) => (x, Some(this))
            case None    => (this, None)
          }
        else (this, None)

      val maxEnd = displaced.getOrElse(default).flattened.maxByOption(_.end).map(_.end).getOrElse(0d)

      val (red, green, blue, reset) =
        if (ansiColors) (AnsiColor.RED_B, AnsiColor.GREEN_B, AnsiColor.BLUE_B, AnsiColor.RESET)
        else ("", "", "", "")

      val prefix =
        if (ansiColors)
          displaced.as {
            s"""|
          |${red}old field schedule$reset
          |${green}new field offset (deferral of execution)$reset
          |""".stripMargin
          }.mkString
        else ""

      val per = math.max((maxEnd / 40d), 1)

      def go(default: List[Node], displacement: Map[PreparedQuery.EdgeId, Node]): String = {
        default
          .sortBy(_.edgeId.id)
          .map { n =>
            val disp = displacement.get(n.edgeId)
            val basePrefix = " " * (n.start / per).toInt
            val showDisp = disp
              .filter(_.end.toInt != n.end.toInt)
              .map { d =>
                val pushedPrefix = blue + ">" * ((d.start - n.start) / per).toInt + green
                s"$basePrefix${pushedPrefix}name: ${d.name}, cost: ${d.cost}, end: ${d.end}$reset"
              }
            val showHere =
              s"$basePrefix${if (showDisp.isDefined) red else ""}name: ${n.name}, cost: ${n.cost}, end: ${n.end}$reset"

            val all = showHere + showDisp.map("\n" + _).mkString
            val children = go(n.children, disp.map(_.children.map(x => x.edgeId -> x).toMap).getOrElse(Map.empty))
            all + "\n" + children
          }
          .mkString("")
      }

      prefix +
        go(default.root, displaced.map(_.root.map(x => x.edgeId -> x).toMap).getOrElse(Map.empty))
    }
  }
  object NodeTree {
    implicit lazy val showForNodeTree: Show[NodeTree] = Show.show[NodeTree](_.show(showImprovement = true))
  }
}
