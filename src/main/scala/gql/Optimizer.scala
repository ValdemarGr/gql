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
import gql.PreparedQuery.Selection
import cats.Monad
import cats.mtl.Stateful
import cats.Eval

object Optimizer {
  final case class Node(
      id: Int,
      name: String,
      batchName: Option[String],
      end: Double,
      cost: Double,
      elemCost: Double,
      children: List[Node]
  ) {
    lazy val start = end - cost
  }

  // TODO get stats for all occuring batch names in the graph before running the algorithm,
  // such that mutation during the algorithm is be avoided
  def constructCostTree[F[_]](
      currentCost: Double,
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]],
      parentTypename: Option[String]
  )(implicit
      F: Monad[F],
      stats: Statistics[F]
  ): F[NonEmptyList[Node]] = {
    prepared.flatTraverse {
      case PreparedDataField(id, name, resolve, selection, tn) =>
        val batchName = resolve match {
          case BatchResolver(batcher, _) => Some(s"batch_${batcher.id}")
          case _                            => None
        }
        // Use parent typename + field as fallback name since this will be unique
        // it is of utmost importance that different resolvers don't get mixed into the same statistic
        // since this will destroy the prediction precision
        val nodeName = s"${parentTypename.getOrElse("Query")}_$name"

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
                  F.pure(Node(id, nodeName, batchName, end, s.initialCost, s.extraElementCost, Nil))
                case Selection(fields) =>
                  constructCostTree[F](end, fields, Some(tn)).map { nel =>
                    Node(id, nodeName, batchName, end, s.initialCost, s.extraElementCost, nel.toList)
                  }
                case PreparedList(of) => handleSelection(of)
              }

            handleSelection(selection).map(NonEmptyList.one(_))
          }
      case PreparedFragField(_, _, selection) => constructCostTree[F](currentCost, selection.fields, parentTypename)
    }
  }

  def costTree[F[_]: Monad](
      prepared: NonEmptyList[PreparedQuery.PreparedField[F, Any]]
  )(implicit stats: Statistics[F]): F[NonEmptyList[Node]] =
    constructCostTree[F](0d, prepared, None)

  def flattenNodeTree(root: NonEmptyList[Node]): NonEmptyList[Node] = {
    def go(xs: NonEmptyList[Node]): Eval[NonEmptyList[Node]] = Eval.defer {
      xs.flatTraverse {
        case n @ Node(_, _, _, _, _, _, Nil)     => Eval.now(NonEmptyList.one(n))
        case n @ Node(_, _, _, _, _, _, x :: xs) => go(NonEmptyList(x, xs)).map(ns => NonEmptyList.one(n).concatNel(ns))
      }
    }

    go(root).value
  }

  def plan(nodes: NonEmptyList[Node]): NonEmptyList[Node] = {
    val flatNodes = flattenNodeTree(nodes)

    val orderedFlatNodes = flatNodes.sortBy(_.end).reverse

    val m = orderedFlatNodes.head.end

    // go through every node sorted by end decending
    // if the node has a batching name, move node to lastest possible batch (frees up most space for parents to move)
    def go(remaining: List[Node], handled: List[Node]): List[Node] =
      remaining match {
        case Nil     => handled
        case r :: rs =>
          // the maximum amount we can move down is the child with smallest start
          val maxEnd: Double = r.children match {
            case Nil     => m
            case x :: xs =>
              // use the already resolved if possible
              val children = NonEmptyList(x, xs)

              // TODO this is very inefficient
              children
                .map(c => handled.find(_.id == c.id).getOrElse(c).start)
                .minimum
          }

          val newEnd =
            r.batchName match {
              // No batching, free up as much space as possible for parents to move
              case None     => maxEnd
              case Some(bn) =>
                // TODO use a grouped map or something to make this an order or so asymptotically faster

                // Find nodes that we may move to:
                // All nodes that end no later than the earliest of our children but end later than us
                val compatible =
                  handled
                    .filter(h => h.batchName.contains(bn) && r.end <= h.end && h.end <= maxEnd)
                    .maximumByOption(_.end)

                compatible.map(_.end).getOrElse(maxEnd)
            }

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
