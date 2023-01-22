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
import gql.PreparedQuery._
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
/*
  def costForPrepared2[F[_]: Statistics](p: PreparedQuery.Prepared2[F, ?], currentCost: Double)(implicit F: Monad[F]): F[List[Node]] =
    p match {
      case PreparedLeaf2(_, _)    => F.pure(Nil)
      case Selection2(fields)     => costForFields[F](currentCost, fields).map(_.toList)
      case PreparedList2(cont, _) => costForCont[F](cont.edges.toChain, cont.cont, currentCost)
      case PreparedOption2(cont)  => costForCont[F](cont.edges.toChain, cont.cont, currentCost)
    }*/
/*
  def costForStep[F[_]](swi: StepWithInfo[F, ?, ?], cont: Prepared2[F, ?], currentCost: Double)(implicit
      stats: Statistics[F],
      F: Monad[F]
  ): F[NonEmptyList[Node]] =
    swi.step match {
      case PreparedStep.
    }*/
    /*
  def costForFields2[F[_]](
      currentCost: Double,
      prepared: NonEmptyList[PreparedQuery.PreparedField2[F]]
  )(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[List[Node]] = {
    prepared.toList.flatTraverse {
      case PreparedDataField2(_, _, _, cont)          => costForCont2[F](cont.edges.toChain, cont.cont, currentCost)
      case PreparedSpecification2(_, _, _, selection) => costForFields2[F](currentCost, selection)
    }
  }*/

  def costForPrepared[F[_]: Statistics](p: PreparedQuery.Prepared[F], currentCost: Double)(implicit F: Monad[F]): F[List[Node]] =
    p match {
      case PreparedLeaf(_, _)    => F.pure(Nil)
      case Selection(fields)     => costForFields[F](currentCost, fields).map(_.toList)
      case PreparedList(cont, _) => costForCont[F](cont.edges.toChain, cont.cont, currentCost)
      case PreparedOption(cont)  => costForCont[F](cont.edges.toChain, cont.cont, currentCost)
    }

  def costForEdges[F[_]](pes: Chain[PreparedQuery.PreparedEdge.Edge[F]], cont: PreparedQuery.Prepared[F], currentCost: Double)(implicit
      stats: Statistics[F],
      F: Monad[F]
  ): F[List[Node]] =
    pes.uncons match {
      case None => costForPrepared[F](cont, currentCost)
      case Some((x, xs)) =>
        val resolverKey = x.resolver match {
          case PreparedQuery.PreparedResolver.Batch(BatchResolver(id, _)) => Some(id)
          case _                                                          => None
        }

        val uniqueEdgeStr = x.uniqueEdgeName.asString

        stats
          .getStatsOpt(uniqueEdgeStr)
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
                  uniqueEdgeStr,
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
      edges: Chain[PreparedQuery.PreparedEdge[F]],
      cont: PreparedQuery.Prepared[F],
      currentCost: Double
  ): F[List[Node]] =
    costForEdges[F](
      edges.collect { case e: PreparedQuery.PreparedEdge.Edge[F] => e },
      cont,
      currentCost
    )

  def costForFields[F[_]](
      currentCost: Double,
      prepared: NonEmptyList[PreparedQuery.PreparedField[F]]
  )(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[List[Node]] = {
    prepared.toList.flatTraverse {
      case PreparedDataField(_, _, _, cont)          => costForCont[F](cont.edges.toChain, cont.cont, currentCost)
      case PreparedSpecification(_, _, _, selection) => costForFields[F](currentCost, selection)
    }
  }

  def costTree[F[_]: Monad](
      prepared: NonEmptyList[PreparedQuery.PreparedField[F]]
  )(implicit stats: Statistics[F]): F[NodeTree] =
    costForFields[F](0d, prepared).map(xs => NodeTree(xs.toList))

  def apply[F[_]](implicit F: Applicative[F]) = new Planner[F] {
    def plan(tree: NodeTree): F[NodeTree] = {
      tree.root.toNel match {
        case None => F.pure(tree.set(tree.root))
        case Some(_) =>
          def findMax(ns: List[Node], current: Double): Eval[Double] = Eval.defer {
            ns.foldLeftM(current) { case (cur, n) =>
              findMax(n.children, cur max n.end)
            }
          }
          val maxEnd = findMax(tree.root, 0d).value

          // Move as far down as we can
          def moveDown(n: Node): Eval[Node] = Eval.defer {
            n.children
              .traverse(moveDown)
              .map { movedChildren =>
                // This end is the minimum end of all children
                val thisEnd = movedChildren.foldLeft(maxEnd)((z, c) => z min c.end)
                n.copy(end = thisEnd, children = movedChildren)
              }
          }

          val movedDown = tree.root.traverse(moveDown).value

          // Run though orderd by end time (smallest first)
          // Then move up to furthest batchable neighbour
          // If no such neighbour, move up as far as possible
          def moveUp(ns: List[Node]): Map[PreparedQuery.EdgeId, Double] =
            ns.mapAccumulate(
              (
                // When a parent has been moved, it adds a reference for every children to their parent's end time
                Map.empty[PreparedQuery.EdgeId, Double],
                // When a node has been visited and is batchable, it is added here
                Map.empty[BatchResolver.ResolverKey, TreeSet[Double]]
              )
            ) { case ((parentEnds, batchMap), n) =>
              val minEnd = parentEnds.getOrElse(n.edgeId, 0d) + n.cost
              val (newEnd, newMap) = n.batcher match {
                // No batching here, move up as far as possible
                case None => (minEnd, batchMap)
                case Some(bn) =>
                  batchMap.get(bn) match {
                    case None    => (minEnd, batchMap + (bn -> TreeSet(minEnd)))
                    case Some(s) =>
                      // This is a possible batch possibility
                      val o = if (s.contains(minEnd)) Some(minEnd) else s.minAfter(minEnd)
                      // If a batch is found, then we can move up to the batch and don't need to modify the set
                      // If no batch is found, then we move up to the minimum end and add this to the set
                      o match {
                        case None    => (minEnd, batchMap + (bn -> (s + minEnd)))
                        case Some(x) => (x, batchMap)
                      }
                  }
              }

              val newParentEnds = parentEnds ++ n.children.map(c => c.edgeId -> newEnd)
              ((newParentEnds, newMap), (n.edgeId, newEnd))
            }._2.toMap

          val ordered = NodeTree(movedDown).flattened.sortBy(_.end)

          val movedUp = moveUp(ordered)

          def reConstruct(ns: List[Node]): Eval[List[Node]] = Eval.defer {
            ns.traverse { n =>
              reConstruct(n.children).map(cs => n.copy(end = movedUp(n.edgeId), children = cs))
            }
          }

          F.pure(tree.set(reConstruct(tree.root).value))
      }
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
