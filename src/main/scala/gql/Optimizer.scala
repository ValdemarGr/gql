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
                  F.pure(Node(id, bn, s.initialCost, end, Nil, s.extraElementCost))
                case Selection(fields) =>
                  constructCostTree[F](end, fields)
                    .map(nel => Node(id, bn, s.initialCost, end, nel.toList, s.extraElementCost))
                case PreparedList(of) => handleSelection(of)
              }

            handleSelection(selection).map(NonEmptyList.one(_))
          }
      case PreparedFragField(_, _, selection) => constructCostTree[F](currentCost, selection.fields)
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
