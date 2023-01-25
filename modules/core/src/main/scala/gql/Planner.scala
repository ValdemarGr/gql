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

import cats.implicits._
import cats.data._
import gql.PreparedQuery._
import scala.collection.immutable.TreeSet
import cats._
import scala.io.AnsiColor
import cats.mtl.Stateful

trait Planner[F[_]] { self =>
  def plan(naive: Planner.NodeTree2): F[Planner.NodeTree2]

  def mapK[G[_]](fk: F ~> G): Planner[G] =
    new Planner[G] {
      def plan(naive: Planner.NodeTree2): G[Planner.NodeTree2] = fk(self.plan(naive))
    }
}

object Planner {
  final case class Node2(
      id: Int,
      name: String,
      end: Double,
      cost: Double,
      elemCost: Double,
      children: List[Node2],
      batchId: Option[Int]
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

  def costForStep[F[_], G[_]](step: PreparedStep[G, ?, ?], right: F[List[Node2]])(implicit
      stats: Statistics[F],
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[List[Node2]] = {
    import PreparedStep._
    step match {
      case Pure(_) | Raise(_) | GetMeta(_) => right
      case Compose(l, r)                   => costForStep[F, G](l, costForStep[F, G](r, right))
      case alg: Skip[G, ?, ?]              => costForStep[F, G](alg.compute, right)
      case alg: First[G, ?, ?, ?]          => costForStep[F, G](alg.step, right)
      case Batch(_) | Effect(_, _) | Stream(_, _) =>
        val name = step match {
          case Batch(id)         => s"batch_$id"
          case Effect(_, cursor) => cursor.asString
          case Stream(_, cursor) => cursor.asString
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
                    Node2(
                      id,
                      name,
                      end,
                      cost.initialCost,
                      cost.extraElementCost,
                      children,
                      step match {
                        case Batch(id) => Some(id)
                        case _         => None
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

  def costForFields2[F[_], G[_]](prepared: NonEmptyList[PreparedQuery.PreparedField2[G, ?]])(implicit
      F: Monad[F],
      stats: Statistics[F],
      S: Stateful[F, TraversalState]
  ): F[List[Node2]] = {
    prepared.toList.flatTraverse { pf =>
      scopeCost {
        pf match {
          case PreparedDataField2(_, _, cont)          => costForCont2[F, G](cont.edges, cont.cont)
          case PreparedSpecification2(_, _, selection) => costForFields2[F, G](selection)
        }
      }
    }
  }

  def costForPrepared2[F[_]: Statistics, G[_]](p: Prepared2[G, ?])(implicit
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[List[Node2]] =
    p match {
      case PreparedLeaf2(_, _)          => F.pure(Nil)
      case Selection2(fields)           => costForFields2[F, G](fields).map(_.toList)
      case l: PreparedList2[G, ?, ?, ?] => costForCont2[F, G](l.of.edges, l.of.cont)
      case o: PreparedOption2[G, ?, ?]  => costForCont2[F, G](o.of.edges, o.of.cont)
    }

  def costForCont2[F[_]: Statistics: Monad, G[_]](
      edges: PreparedStep[G, ?, ?],
      cont: Prepared2[G, ?]
  )(implicit S: Stateful[F, TraversalState]): F[List[Node2]] =
    costForStep[F, G](edges, costForPrepared2[F, G](cont))

  type H[F[_], A] = StateT[F, TraversalState, A]
  def liftStatistics[F[_]: Applicative](stats: Statistics[F]): Statistics[H[F, *]] =
    stats.mapK(StateT.liftK[F, TraversalState])

  def runCostAnalysisFor[F[_]: Monad, A](f: Statistics[H[F, *]] => H[F, A])(implicit stats: Statistics[F]): F[A] =
    f(liftStatistics[F](stats)).runA(TraversalState(1, 0d))

  def runCostAnalysis[F[_]: Monad: Statistics](f: Statistics[H[F, *]] => H[F, List[Node2]]): F[NodeTree2] =
    runCostAnalysisFor[F, List[Node2]](f).map(NodeTree2(_))

  def apply[F[_]](implicit F: Applicative[F]) = new Planner[F] {
    def plan(tree: NodeTree2): F[NodeTree2] = {
      tree.root.toNel match {
        case None => F.pure(tree.set(tree.root))
        case Some(_) =>
          def findMax(ns: List[Node2], current: Double): Eval[Double] = Eval.defer {
            ns.foldLeftM(current) { case (cur, n) =>
              findMax(n.children, cur max n.end)
            }
          }
          val maxEnd = findMax(tree.root, 0d).value

          // Move as far down as we can
          def moveDown(n: Node2): Eval[Node2] = Eval.defer {
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
          def moveUp(ns: List[Node2]): Map[Int, Double] =
            ns.mapAccumulate(
              (
                // When a parent has been moved, it adds a reference for every children to their parent's end time
                Map.empty[Int, Double],
                // When a node has been visited and is batchable, it is added here
                Map.empty[Int, TreeSet[Double]]
              )
            ) { case ((parentEnds, batchMap), n) =>
              val minEnd = parentEnds.getOrElse(n.id, 0d) + n.cost
              val (newEnd, newMap) = n.batchId match {
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

              val newParentEnds = parentEnds ++ n.children.map(c => c.id -> newEnd)
              ((newParentEnds, newMap), (n.id, newEnd))
            }._2
              .toMap

          val ordered = NodeTree2(movedDown).flattened.sortBy(_.end)

          val movedUp = moveUp(ordered)

          def reConstruct(ns: List[Node2]): Eval[List[Node2]] = Eval.defer {
            ns.traverse { n =>
              reConstruct(n.children).map(cs => n.copy(end = movedUp(n.id), children = cs))
            }
          }

          F.pure(tree.set(reConstruct(tree.root).value))
      }
    }
  }
  final case class NodeTree2(
      root: List[Node2],
      source: Option[NodeTree2] = None
  ) {
    def set(newRoot: List[Node2]): NodeTree2 =
      NodeTree2(newRoot, Some(this))

    lazy val flattened: List[Node2] = {
      def go(xs: List[Node2]): Eval[List[Node2]] = Eval.defer {
        xs.flatTraverse {
          case n @ Node2(_, _, _, _, _, Nil, _) => Eval.now(List(n))
          case n @ Node2(_, _, _, _, _, xs, _)  => go(xs).map(n :: _)
        }
      }

      go(root).value
    }

    lazy val batches: List[(Int, NonEmptyChain[Int])] =
      flattened
        .map(n => (n.batchId, n))
        .collect { case (Some(batcherKey), node) => (batcherKey, node) }
        .groupByNec { case (_, node) => node.end }
        .toList
        .flatMap { case (_, endGroup) =>
          endGroup
            .groupBy { case (batcherKey, _) => batcherKey }
            .toSortedMap
            .toList
            .map { case (batcherKey, batch) =>
              batcherKey -> batch.map { case (_, node) => node.id }
            }
        }

    def totalCost: Double = {
      val thisFlat = flattened
      val thisFlattenedMap = thisFlat.map(n => n.id -> n).toMap
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

      def go(default: List[Node2], displacement: Map[Int, Node2]): String = {
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
  object NodeTree2 {
    implicit lazy val showForNodeTree: Show[NodeTree2] = Show.show[NodeTree2](_.show(showImprovement = true))
  }
}
