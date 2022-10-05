package gql

import gql.resolver._
import cats.effect._
import cats.implicits._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import cats.data._
import gql.PreparedQuery.PreparedDataField
import gql.PreparedQuery.PreparedFragField
import gql.PreparedQuery.PreparedLeaf
import gql.PreparedQuery.PreparedList
import gql.PreparedQuery.PreparedOption
import gql.PreparedQuery.Selection
import cats.Monad
import cats.mtl.Stateful
import cats.Eval
import scala.collection.immutable.TreeSet

object Planner {
  final case class NodeTree(root: List[Node]) {
    lazy val flattened: List[Node] = {
      def go(xs: List[Node]): Eval[List[Node]] = Eval.defer {
        xs.flatTraverse {
          case n @ Node(_, _, _, _, _, _, Nil, _, _) => Eval.now(List(n))
          case n @ Node(_, _, _, _, _, _, xs, _, _)  => go(xs).map(n :: _)
        }
      }

      go(root).value
    }
  }

  final case class Node(
      id: Int,
      name: String,
      batchName: Option[String],
      end: Double,
      cost: Double,
      elemCost: Double,
      children: List[Node],
      batcher: Option[BatchResolver.ResolverKey] = None,
      edgeId: PreparedQuery.EdgeId
  ) {
    lazy val start = end - cost
  }

  def makeBatchName[F[_]](b: Int) = s"batch_$b"

  // Use parent typename + field as fallback name since this will be unique
  // it is of utmost importance that different resolvers don't get mixed into the same statistic
  // since this will destroy the prediction precision
  def makeTypename[F[_]](df: PreparedDataField[F, _, _]): String =
    s"${df.parentTypename}_${df.name}"

  def costForPrepared[F[_]: Statistics](p: PreparedQuery.Prepared[F, Any], currentCost: Double)(implicit F: Monad[F]): F[List[Node]] =
    p match {
      case PreparedLeaf(_, _) => F.pure(Nil)
      case Selection(fields)  => costForFields[F](currentCost, fields).map(_.toList)
      case PreparedList(of)   => costForPrepared(of, currentCost)
      case PreparedOption(of) => costForPrepared(of, currentCost)
    }

  def costForEdges[F[_]](pes: NonEmptyChain[PreparedQuery.PreparedEdge[F]], cont: PreparedQuery.Prepared[F, Any], currentCost: Double)(
      implicit
      stats: Statistics[F],
      F: Monad[F]
  ): F[NonEmptyList[Node]] = {
    val x = pes.head
    val xs = pes.tail

    val resolverKey = x.resolver match {
      case BatchResolver(id, _) => Some(id)
      case _                    => None
    }

    stats
      .getStatsOpt(x.statisticsName)
      .map {
        case None    => Statistics.Stats(1000d, 5d)
        case Some(x) => x
      }
      .flatMap { s =>
        val end = currentCost + s.initialCost

        val childrenF: F[List[Node]] = NonEmptyChain.fromChain(xs) match {
          case None      => costForPrepared[F](cont, end)
          case Some(nel) => costForEdges[F](nel, cont, end).map(_.toList)
        }

        childrenF.map { children =>
          NonEmptyList.one(
            Node(
              42,
              x.statisticsName,
              null,
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

  def costForFields[F[_]](
      currentCost: Double,
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[NonEmptyList[Node]] = {
    prepared.flatTraverse {
      case df @ PreparedDataField(id, name, resolve, selection, tn, _, _, cont) =>
        costForEdges[F](cont.edges, cont.cont, currentCost)
      case PreparedFragField(_, typename, _, selection) => costForFields[F](currentCost, selection.fields)
    }
  }

  def costTree2[F[_]: Monad](
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit stats: Statistics[F]): F[NodeTree] =
    costForFields[F](0d, prepared).map(xs => NodeTree(xs.toList))

  def plan2(tree: NodeTree): NodeTree = {
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

    NodeTree(reConstruct(tree.root.map(_.edgeId)).value)
  }

  // TODO get stats for all occuring batch names in the graph before running the algorithm,
  // such that mutation during the algorithm is be avoided
  def constructCostTree[F[_]](
      currentCost: Double,
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[NonEmptyList[Node]] = {
    prepared.flatTraverse {
      case df @ PreparedDataField(id, name, resolve, selection, tn, _, _, _) =>
        val batchName = resolve match {
          case br @ BatchResolver(_, _) => Some(makeBatchName(br.id.id))
          case _                        => None
        }
        val nodeName = makeTypename(df)

        /*
         * We try to get by batch name first since this unifies a set of nodes that would otherwise have different names
         *
         * A_value with type V
         * and
         * B_value with type V
         *
         * Would not be considered the same statistic unless expilictly stated via a batch resolver with the same name
         */
        val bn = batchName.getOrElse(nodeName)

        stats
          .getStatsOpt(bn)
          .map {
            case None    => Statistics.Stats(1000d, 5d)
            case Some(x) => x
          }
          .flatMap { s =>
            val end = currentCost + s.initialCost
            def handleSelection(p: PreparedQuery.Prepared[F, Any]): F[Node] =
              p match {
                case PreparedLeaf(_, _) =>
                  F.pure(Node(id, nodeName, batchName, end, s.initialCost, s.extraElementCost, Nil, null, PreparedQuery.EdgeId(42)))
                case Selection(fields) =>
                  constructCostTree[F](end, fields).map { nel =>
                    Node(id, nodeName, batchName, end, s.initialCost, s.extraElementCost, nel.toList, null, PreparedQuery.EdgeId(42))
                  }
                case PreparedList(of)   => handleSelection(of)
                case PreparedOption(of) => handleSelection(of)
              }

            handleSelection(selection).map(NonEmptyList.one(_))
          }
      case PreparedFragField(_, typename, _, selection) => constructCostTree[F](currentCost, selection.fields)
    }
  }

  def costTree[F[_]: Monad](
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit stats: Statistics[F]): F[NodeTree] =
    constructCostTree[F](0d, prepared).map(xs => NodeTree(xs.toList))

  def plan(tree: NodeTree): NodeTree = ??? /*{
    val flatNodes = tree.flattened

    val orderedFlatNodes = flatNodes.sortBy(_.end).reverse

    val m = orderedFlatNodes.head.end

    // go through every node sorted by end decending
    // if the node has a batching name, move node to lastest possible batch (frees up most space for parents to move)
    def go(
        remaining: List[Node],
        handled: Map[PreparedQuery.EdgeId, Node],
        batchMap: Map[String, Eval[TreeSet[Double]]]
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
            r.batchName match {
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

    def reConstruct(ns: NonEmptyList[PreparedQuery.EdgeId]): Eval[NonEmptyList[Node]] = Eval.defer {
      ns.traverse { n =>
        val newN = plannedMap(n)
        newN.children match {
          case Nil => Eval.now(newN)
          case x :: xs =>
            val newChildrenF = reConstruct(NonEmptyList(x, xs).map(_.edgeId)).map(_.toList)
            newChildrenF.map(x => newN.copy(children = x))
        }
      }
    }

    NodeTree(reConstruct(tree.root.map(_.edgeId)).value)
  }*/
}
