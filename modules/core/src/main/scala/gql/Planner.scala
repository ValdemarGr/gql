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
package gql

import cats.implicits._
import cats.data._
import gql.PreparedQuery._
import scala.collection.immutable.TreeSet
import cats._
import scala.io.AnsiColor
import cats.mtl.Stateful
import gql.resolver.Step

trait Planner[F[_]] { self =>
  def plan(naive: Planner.NodeTree): F[Planner.NodeTree]

  def mapK[G[_]](fk: F ~> G): Planner[G] =
    new Planner[G] {
      def plan(naive: Planner.NodeTree): G[Planner.NodeTree] = fk(self.plan(naive))
    }
}

object Planner {
  final case class BatchRef[K, V](
      batcherId: gql.resolver.Step.BatchKey[K, V],
      uniqueNodeId: PreparedQuery.UniqueBatchInstance[K, V]
  )

  final case class Node(
      id: Int,
      name: String,
      end: Double,
      cost: Double,
      elemCost: Double,
      children: List[Node],
      batchId: Option[BatchRef[?, ?]]
  ) {
    lazy val start = end - cost
  }

  final case class TraversalState(
      id: Int,
      currentCost: Double
  )

  def scopeCost[F[_]: Monad, A](fa: F[A])(implicit S: Stateful[F, TraversalState]): F[A] =
    S.inspect(_.currentCost).flatMap { initial =>
      fa <* S.modify(_.copy(currentCost = initial))
    }

  def getId[F[_]: Applicative](implicit S: Stateful[F, TraversalState]): F[Int] =
    S.inspect(_.id) <* S.modify(s => s.copy(id = s.id + 1))

  def nextId[F[_]: Applicative](implicit S: Stateful[F, Int]): F[Int] =
    S.get <* S.modify(_ + 1)

  def costForStep[F[_], G[_]](step: PreparedStep[G, ?, ?], right: F[List[Node]])(implicit
      stats: Statistics[F],
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[List[Node]] = {
    import PreparedStep._
    step match {
      case Lift(_) | EmbedError() | GetMeta(_) => right
      case Compose(l, r)                       => costForStep[F, G](l, costForStep[F, G](r, right))
      case alg: Skip[G, ?, ?]                  => costForStep[F, G](alg.compute, right)
      case alg: First[G, ?, ?, ?]              => costForStep[F, G](alg.step, right)
      case Batch(_, _) | EmbedEffect(_) | EmbedStream(_) =>
        val name = step match {
          case Batch(id, _)        => s"batch_$id"
          case EmbedEffect(cursor) => cursor.asString
          case EmbedStream(cursor) => cursor.asString
          case _                   => ???
        }

        val costF = stats
          .getStatsOpt(name)
          .map(_.getOrElse(Statistics.Stats(100d, 5d)))

        costF.flatMap { cost =>
          S.inspect(_.currentCost).flatMap { currentCost =>
            val end = currentCost + cost.initialCost
            S.modify(_.copy(currentCost = end)) >> {
              right.flatMap { children =>
                getId[F].map { id =>
                  List(
                    Node(
                      id,
                      name,
                      end,
                      cost.initialCost,
                      cost.extraElementCost,
                      children,
                      step match {
                        case Batch(batcherId, uniqueNodeId) => Some(BatchRef(batcherId, uniqueNodeId))
                        case _                              => None
                      }
                    )
                  )
                }
              }
            }
          }
        }
    }
  }

  def costForFields[F[_], G[_]](prepared: NonEmptyList[PreparedQuery.PreparedField[G, ?]])(implicit
      F: Monad[F],
      stats: Statistics[F],
      S: Stateful[F, TraversalState]
  ): F[List[Node]] = {
    prepared.toList.flatTraverse { pf =>
      scopeCost {
        pf match {
          case PreparedDataField(_, _, cont)          => costForCont[F, G](cont.edges, cont.cont)
          case PreparedSpecification(_, _, selection) => costForFields[F, G](selection)
        }
      }
    }
  }

  def costForPrepared[F[_]: Statistics, G[_]](p: Prepared[G, ?])(implicit
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[List[Node]] =
    p match {
      case PreparedLeaf(_, _)          => F.pure(Nil)
      case Selection(fields)           => costForFields[F, G](fields).map(_.toList)
      case l: PreparedList[G, ?, ?, ?] => costForCont[F, G](l.of.edges, l.of.cont)
      case o: PreparedOption[G, ?, ?]  => costForCont[F, G](o.of.edges, o.of.cont)
    }

  def costForCont[F[_]: Statistics: Monad, G[_]](
      edges: PreparedStep[G, ?, ?],
      cont: Prepared[G, ?]
  )(implicit S: Stateful[F, TraversalState]): F[List[Node]] =
    costForStep[F, G](edges, costForPrepared[F, G](cont))

  type H[F[_], A] = StateT[F, TraversalState, A]
  def liftStatistics[F[_]: Applicative](stats: Statistics[F]): Statistics[H[F, *]] =
    stats.mapK(StateT.liftK[F, TraversalState])

  def runCostAnalysisFor[F[_]: Monad, A](f: Statistics[H[F, *]] => H[F, A])(implicit stats: Statistics[F]): F[A] =
    f(liftStatistics[F](stats)).runA(TraversalState(1, 0d))

  def runCostAnalysis[F[_]: Monad: Statistics](f: Statistics[H[F, *]] => H[F, List[Node]]): F[NodeTree] =
    runCostAnalysisFor[F, List[Node]](f).map(NodeTree(_))

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
          def moveUp(ns: List[Node]): Map[Int, Double] =
            ns.mapAccumulate(
              (
                // When a parent has been moved, it adds a reference for every children to their parent's end time
                Map.empty[Int, Double],
                // When a node has been visited and is batchable, it is added here
                Map.empty[Step.BatchKey[?, ?], TreeSet[Double]]
              )
            ) { case ((parentEnds, batchMap), n) =>
              val minEnd = parentEnds.getOrElse(n.id, 0d) + n.cost
              val (newEnd, newMap) = n.batchId match {
                // No batching here, move up as far as possible
                case None => (minEnd, batchMap)
                case Some(bn) =>
                  batchMap.get(bn.batcherId) match {
                    case None    => (minEnd, batchMap + (bn.batcherId -> TreeSet(minEnd)))
                    case Some(s) =>
                      // This is a possible batch possibility
                      val o = if (s.contains(minEnd)) Some(minEnd) else s.minAfter(minEnd)
                      // If a batch is found, then we can move up to the batch and don't need to modify the set
                      // If no batch is found, then we move up to the minimum end and add this to the set
                      o match {
                        case None    => (minEnd, batchMap + (bn.batcherId -> (s + minEnd)))
                        case Some(x) => (x, batchMap)
                      }
                  }
              }

              val newParentEnds = parentEnds ++ n.children.map(c => c.id -> newEnd)
              ((newParentEnds, newMap), (n.id, newEnd))
            }._2
              .toMap

          val ordered = NodeTree(movedDown).flattened.sortBy(_.end)

          val movedUp = moveUp(ordered)

          def reConstruct(ns: List[Node]): Eval[List[Node]] = Eval.defer {
            ns.traverse { n =>
              reConstruct(n.children).map(cs => n.copy(end = movedUp(n.id), children = cs))
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
          case n @ Node(_, _, _, _, _, Nil, _) => Eval.now(List(n))
          case n @ Node(_, _, _, _, _, xs, _)  => go(xs).map(n :: _)
        }
      }

      go(root).value
    }

    lazy val batches: List[(Step.BatchKey[?, ?], NonEmptyChain[BatchRef[?, ?]])] =
      flattened
        .map(n => (n.batchId, n))
        .collect { case (Some(batcherKey), node) => (batcherKey, node) }
        .groupByNec { case (_, node) => node.end }
        .toList
        .flatMap { case (_, endGroup) =>
          endGroup
            .groupBy { case (batcherKey, _) => batcherKey.batcherId.id }
            .toSortedMap
            .toList
            .map { case (batcherKey, batch) =>
              Step.BatchKey(batcherKey) -> batch.map { case (br, _) => br }
            }
        }

    def totalCost: Double = {
      val thisFlat = flattened
      val thisBatches = batches.filter { case (_, edges) => edges.size > 1 }
      val batchCostMap: Map[Step.BatchKey[?, ?], Double] =
        thisFlat.mapFilter(n => n.batchId.map(br => br.batcherId -> n.cost)).toMap

      val naiveCost = thisFlat.map(_.cost).sum

      val batchSubtraction = thisBatches.map { case (_, xs) =>
        // cost * (size - 1 )
        batchCostMap(xs.head.batcherId) * (xs.size - 1)
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

      def go(default: List[Node], displacement: Map[Int, Node]): String = {
        default
          .sortBy(_.id)
          .map { n =>
            val disp = displacement.get(n.id)
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
            val children = go(n.children, disp.map(_.children.map(x => x.id -> x).toMap).getOrElse(Map.empty))
            all + "\n" + children
          }
          .mkString("")
      }

      prefix +
        go(default.root, displaced.map(_.root.map(x => x.id -> x).toMap).getOrElse(Map.empty))
    }
  }
  object NodeTree {
    implicit lazy val showForNodeTree: Show[NodeTree] = Show.show[NodeTree](_.show(showImprovement = true))
  }
}
