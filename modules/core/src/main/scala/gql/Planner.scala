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
import scala.collection.immutable.SortedMap

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

  final case class NodeId(id: Int) extends AnyVal

  object NodeId {
    implicit val ord: Order[NodeId] = Order.by[NodeId, Int](_.id)
  }

  final case class Node(
      id: NodeId,
      name: String,
      //end: Double,
      cost: Double,
      elemCost: Double,
      //children: List[Node],
      parents: Set[NodeId],
      batchId: Option[BatchRef[?, ?]]
  ) //{
  //lazy val start = end - cost
  //}

  final case class TraversalState(
      id: Int,
      parents: Set[NodeId],
      nodes: Chain[Node]
  )

  def getId[F[_]: Applicative](implicit S: Stateful[F, TraversalState]): F[NodeId] =
    S.inspect(x => NodeId(x.id)) <* S.modify(s => s.copy(id = s.id + 1))

  def setParents[F[_]](parents: Set[NodeId])(implicit S: Stateful[F, TraversalState]): F[Unit] =
    S.modify(s => s.copy(parents = parents))

  def addNodes[F[_]](nodes: Chain[Node])(implicit S: Stateful[F, TraversalState]): F[Unit] =
    S.modify(s => s.copy(nodes = s.nodes ++ nodes))

  def addNode[F[_]](node: Node)(implicit S: Stateful[F, TraversalState]): F[Unit] =
    S.modify(s => s.copy(nodes = s.nodes :+ node))

  def costForStep[F[_], G[_]](step: PreparedStep[G, ?, ?])(implicit
      stats: Statistics[F],
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[Unit] = {
    def goParallel(l: PreparedStep[G, ?, ?], r: PreparedStep[G, ?, ?]): F[Unit] = {
      // A parallel op is disjunctive so the parent must be the same for both branches
      // This creates a diamond shape in the graph
      // Parent -> Left -> Child
      // Parent -> Right -> Child
      S.inspect(_.parents).flatMap { ps =>
        val setParents = S.modify(s => s.copy(parents = ps))
        val left = setParents *> costForStep(l) *> S.inspect(_.parents)
        val right = setParents *> costForStep(r) *> S.inspect(_.parents)

        (left, right).flatMapN { case (lp, rp) =>
          val parents = lp ++ rp
          S.modify(s => s.copy(parents = parents))
        }
      }
    }

    import PreparedStep._
    step match {
      case Lift(_) | EmbedError() | GetMeta(_) => F.unit
      case Compose(l, r)                       => costForStep[F, G](l) *> costForStep[F, G](r)
      case alg: Choose[G, ?, ?, ?, ?]          => goParallel(alg.fac, alg.fbc)
      case alg: First[G, ?, ?, ?]              => costForStep[F, G](alg.step)
      case Batch(_, _) | EmbedEffect(_) | EmbedStream(_, _) =>
        val name = step match {
          case Batch(id, _)           => s"batch_$id"
          case EmbedEffect(cursor)    => cursor.asString
          case EmbedStream(_, cursor) => cursor.asString
          case _                      => ???
        }

        val costF = stats
          .getStatsOpt(name)
          .map(_.getOrElse(Statistics.Stats(100d, 5d)))

        costF.flatMap { cost =>
          getId[F].flatMap { id =>
            S.inspect(_.parents).flatMap { parentIds =>
              addNode {
                Node(
                  id,
                  name,
                  cost.initialCost,
                  cost.extraElementCost,
                  parentIds,
                  step match {
                    case Batch(batcherId, uniqueNodeId) => Some(BatchRef(batcherId, uniqueNodeId))
                    case _                              => None
                  }
                )
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
  ): F[Unit] = {
    prepared.toList.traverse_ { pf =>
      pf match {
        case PreparedDataField(_, _, cont)          => costForCont[F, G](cont.edges, cont.cont)
        case PreparedSpecification(_, _, selection) => costForFields[F, G](selection)
      }
    }
  }

  def costForPrepared[F[_]: Statistics, G[_]](p: Prepared[G, ?])(implicit
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ): F[Unit] =
    p match {
      case PreparedLeaf(_, _)          => F.unit
      case Selection(fields)           => costForFields[F, G](fields)
      case l: PreparedList[G, ?, ?, ?] => costForCont[F, G](l.of.edges, l.of.cont)
      case o: PreparedOption[G, ?, ?]  => costForCont[F, G](o.of.edges, o.of.cont)
    }

  def costForCont[F[_]: Statistics: Monad, G[_]](
      edges: PreparedStep[G, ?, ?],
      cont: Prepared[G, ?]
  )(implicit S: Stateful[F, TraversalState]): F[Unit] =
    costForStep[F, G](edges) *> costForPrepared[F, G](cont)

  type H[F[_], A] = StateT[F, TraversalState, A]
  def liftStatistics[F[_]: Applicative](stats: Statistics[F]): Statistics[H[F, *]] =
    stats.mapK(StateT.liftK[F, TraversalState])

  def runCostAnalysisFor[F[_]: Monad, A](f: Statistics[H[F, *]] => H[F, A])(implicit stats: Statistics[F]): F[A] =
    f(liftStatistics[F](stats)).runA(TraversalState(1, Set.empty, Chain.empty))

  def runCostAnalysis[F[_]: Monad: Statistics, A](f: Statistics[H[F, *]] => H[F, A]): F[NodeTree] =
    runCostAnalysisFor[F, List[Node]](s => f(s).get.map(_.nodes.toList)).map(NodeTree.simple(_))

  def apply[F[_]](implicit F: Applicative[F]) = new Planner[F] {
    def plan(tree: NodeTree): F[NodeTree] = {
      tree.roots.toNel match {
        case None => F.pure(tree.copy(source = Some(tree)))
        case Some(_) =>
          val baseEnds = tree.endTimes
          val childrenV = tree.childrenLookup
          val lookupV = tree.lookup

          val maxEnd = baseEnds.values.maxOption.get

          // Move as far down as we can
          def moveDown(id: NodeId): State[EndTimes, Double] =
            State.inspect((a: EndTimes) => a.get(id)).flatMap {
              case Some(d) => State.pure(d)
              case None =>
                val children = childrenV.get(id).toList.flatMap(_.toList)
                // Find the minimum start time of all children
                // That is as far down as we can go
                children
                  .traverse(n => moveDown(n).map(newEnd => newEnd - lookupV(n).cost))
                  .map(_.minOption.getOrElse(maxEnd))
                  .flatMap { newEnd =>
                    State.modify[EndTimes](_ + (id -> newEnd)) as newEnd
                  }
            }

          val movedDown = tree.roots.map(_.id).traverse_(moveDown).runS(Map.empty).value

          // Run though orderd by end time (smallest first)
          // Then move up to furthest batchable neighbour
          // If no such neighbour, move up as far as possible
          def moveUp(ns: List[NodeId]): Map[NodeId, Double] =
            ns.mapAccumulate(
              (
                // Map of node id to NEW start time
                Map.empty[NodeId, Double],
                // When a node has been visited and is batchable, it is added here
                Map.empty[Step.BatchKey[?, ?], TreeSet[Double]]
              )
            ) { case ((ends, batchMap), id) =>
              val n = lookupV(id)
              // We may move up to the maximum parent end time
              val minEnd = n.parents.map(ends(_)).maxOption.getOrElse(0d)
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

              val newEnds = ends + (id -> newEnd)
              ((newEnds, newMap), (n.id, newEnd))
            }._2
              .toMap

          val ordered = movedDown.toList.sortBy { case (_, d) => d }.map { case (id, _) => id }

          val movedUp = moveUp(ordered)

          F.pure(tree.set(movedUp))
      }
    }
  }

  type EndTimes = Map[NodeId, Double]

  final case class NodeTree(
      all: List[Node],
      endTimes: EndTimes,
      source: Option[NodeTree] = None
  ) {
    def set(endTimes: EndTimes): NodeTree =
      NodeTree(all, endTimes, Some(this))

    lazy val lookup: Map[NodeId, Node] =
      all.map(n => n.id -> n).toMap

    lazy val childrenLookup: SortedMap[NodeId, NonEmptyChain[NodeId]] =
      all
        .flatMap(n => n.parents.map(p => p -> n.id))
        .groupByNec { case (p, _) => p }
        .fmap(_.map { case (_, c) => c })

    lazy val roots = all.filter(_.parents.isEmpty)

    // If a node has children, it is not a leaf
    lazy val leaves = {
      val childrenV = childrenLookup
      all.filter(n => !childrenV.contains(n.id))
    }

    lazy val batches: List[(Step.BatchKey[?, ?], NonEmptyChain[BatchRef[?, ?]])] = {
      val endTimesV = endTimes
      all
        .map(n => (n.batchId, n))
        .collect { case (Some(batcherKey), node) => (batcherKey, node) }
        .groupByNec { case (_, node) => endTimesV(node.id) }
        .toList
        .flatMap { case (_, endGroup) =>
          endGroup.toList
            .groupBy { case (batcherKey, _) => batcherKey.batcherId }
            .map { case (batcherKey, batch) => batcherKey -> NonEmptyChain.fromSeq(batch.map { case (br, _) => br }) }
            .collect { case (k, Some(vs)) => k -> vs }
        }
    }

    lazy val totalCost: Double = {
      val thisFlat = all
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

      val nt = displaced.getOrElse(default)

      val maxEnd = nt.all.map(n => nt.endTimes(n.id)).maxOption.getOrElse(0d)

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

      def go(nodes: List[NodeId]): String = {
        nodes
          .sortBy(_.id)
          .map { n0 =>
            val n = lookup(n0)
            val nEnd = endTimes(n.id)
            val nStart = nEnd - n.cost
            val disp = nt.lookup.get(n.id)
            val basePrefix = " " * (nStart / per).toInt
            val showDisp = disp
              .filter(d => nt.endTimes(d.id) != nEnd.toInt)
              .map { d =>
                val dEnd = nt.endTimes(d.id)
                val dStart = dEnd - d.cost
                val pushedPrefix = blue + ">" * ((dStart - nStart) / per).toInt + green
                s"$basePrefix${pushedPrefix}name: ${d.name}, cost: ${d.cost}, end: ${dEnd}$reset"
              }
            val showHere =
              s"$basePrefix${if (showDisp.isDefined) red else ""}name: ${n.name}, cost: ${n.cost}, end: ${nEnd}$reset"

            val all = showHere + showDisp.map("\n" + _).mkString
            val children = go(childrenLookup.get(n.id).map(_.toList).getOrElse(Nil))
            all + "\n" + children
          }
          .mkString("")
      }

      prefix + go(default.roots.map(_.id))
    }
  }
  object NodeTree {
    def computeNewEndTimes(nt: NodeTree) = {
      // lazy val optimization
      val lookupV = nt.lookup

      // we start from the leaves since that fixes the diamond problem

      def getEndTime(n: Node): State[EndTimes, Double] =
        State.inspect((a: EndTimes) => a.get(n.id)).flatMap {
          case Some(d) => State.pure(d)
          case None    =>
            // find the max end time of all parents and add the cost
            n.parents.toList
              .traverse(p => getEndTime(lookupV(p)))
              .map(_.maxOption.getOrElse(0d))
              .flatMap { parentEnd =>
                val end = parentEnd + n.cost
                State.modify[EndTimes](_ + (n.id -> end)) as end
              }
        }

      nt.leaves.traverse_(getEndTime).runS(Map.empty).value
    }

    def simple(nodes: List[Node]): NodeTree = {
      val base = NodeTree(nodes, Map.empty)
      val ends = computeNewEndTimes(base)
      NodeTree(nodes, ends)
    }

    implicit lazy val showForNodeTree: Show[NodeTree] = Show.show[NodeTree](_.show(showImprovement = true))
  }
}
