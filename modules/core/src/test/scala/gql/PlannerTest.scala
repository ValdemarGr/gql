package gql

import munit.CatsEffectSuite
import cats.implicits._
import scala.collection.immutable.TreeSet
import cats.kernel.Semigroup

class PlannerTest extends CatsEffectSuite {
  type Id = Int
  type Family = Int
  final case class Problem(
      families: List[List[Id]],
      arcs: Map[Id, List[Id]],
      costs: Map[Family, Int]
  ) {
    val reverseArcs: Map[Id, List[Id]] =
      arcs.toList
        .flatMap { case (parent, children) => children tupleRight parent }
        .groupMap { case (child, _) => child } { case (_, parent) => parent }

    val familyMap: Map[Id, Family] =
      families.zipWithIndex.flatMap { case (ids, family) => ids tupleRight family }.toMap

    val reverseFamilyMap: Map[Family, List[Id]] =
      familyMap.toList.groupMap { case (_, family) => family } { case (id, _) => id }
  }

  type EndTime = Int
  type StartTime = Int
  case class PlannerState(
      batches: Map[Family, TreeSet[StartTime]],
      lookup: Map[Id, EndTime]
  )
  object PlannerState {
    implicit def monoidForPlannerState: cats.Monoid[PlannerState] = new cats.Monoid[PlannerState] {
      implicit def semigroupForTreeSet[A]: Semigroup[TreeSet[A]] = new Semigroup[TreeSet[A]] {
        def combine(x: TreeSet[A], y: TreeSet[A]): TreeSet[A] = x ++ y
      }

      def empty: PlannerState = PlannerState(Map.empty, Map.empty)
      def combine(x: PlannerState, y: PlannerState): PlannerState =
        PlannerState(
          x.batches |+| y.batches,
          x.lookup ++ y.lookup
        )
    }
  }

  test("blah") {
    var i = 0
    def solve(problem: Problem): LazyList[PlannerState] = {
      val roots: List[Id] = problem.families.flatten.filter(id => problem.reverseArcs.getOrElse(id, Nil).isEmpty)

      def findMaxEnd(id: Int): Int =
        problem.arcs.getOrElse(id, Nil).map(findMaxEnd).maxOption.getOrElse(0) + problem.costs(problem.familyMap(id))

      //val maxEnd = roots.map(findMaxEnd).max

      case class PlannerStateDiff(state: PlannerState)

      sealed trait Round { def states: List[PlannerStateDiff] }
      case class TryAll(states: List[PlannerStateDiff]) extends Round
      case class FurthestMoveIsBatch(state: PlannerStateDiff) extends Round {
        def states = List(state)
      }
      case class FurthestMove(state: PlannerStateDiff) extends Round {
        def states = List(state)
      }
      def round(id: Id, state: PlannerState): Round = {
        i = i + 1
        // One of the following:
        // 1. It moves as far up as possible
        // 2. It moves up to the furthest batch

        // Furthest is the latest ending parent
        val furthest = problem.reverseArcs
          .getOrElse(id, Nil)
          .map(state.lookup)
          .maxOption
          .getOrElse(0)

        val family = problem.familyMap(id)
        val familyBatches = state.batches.get(family).getOrElse(TreeSet.empty[EndTime])

        lazy val furthestMove = PlannerStateDiff(
          PlannerState(
            lookup = Map(id -> (furthest + problem.costs(family))),
            batches = Map(family -> (familyBatches + furthest))
          )
        )

        lazy val subOptimalBatch = familyBatches.minAfter(furthest)
        if (familyBatches.contains(furthest)) {
          FurthestMoveIsBatch(furthestMove)
        } else
          subOptimalBatch match {
            case None => FurthestMove(furthestMove)
            case Some(b) =>
              TryAll(
                List(
                  furthestMove,
                  PlannerStateDiff(PlannerState(batches = Map.empty, lookup = state.lookup + (id -> (b + problem.costs(family)))))
                )
              )
          }
      }

      import scala.collection.mutable.{Set => MSet}
      var symmetryBreaker: MSet[(Set[Id], PlannerState)] = MSet.empty
      def enumerateRounds(currentRoots: Set[Id], state: PlannerState): LazyList[PlannerState] = {
        if (symmetryBreaker.contains((currentRoots, state))) LazyList.empty
        else if (currentRoots.isEmpty) LazyList(state)
        else {
          symmetryBreaker += ((currentRoots, state))
          //println(s"running for {${currentRoots.toList.sorted.map(names(_)).mkString(",")}}")
          def merge(xs: List[Round]): PlannerState =
            xs.foldMap(_.states.foldMap(_.state))

          def runFor(xs: List[(Set[Id], PlannerState)], remainingRoots: Set[Id]): LazyList[PlannerState] = {
            LazyList.from(xs).flatMap { case (consumed, merged) =>
              val newState = state |+| merged
              val freedChildren = consumed.flatMap(problem.arcs.getOrElse(_, Nil).toSet)
              enumerateRounds((remainingRoots ++ freedChildren) -- consumed, newState)
            }
          }

          def run(xs: List[(Id, Round)], remainingRoots: Set[Id]): LazyList[PlannerState] =
            runFor(List((xs.map { case (id, _) => id }.toSet, merge(xs.map { case (_, r) => r }))), remainingRoots)

          def prune(runNow: => (List[(Id, Round)], List[Id])): Option[LazyList[PlannerState]] = {
            val (r, w) = runNow
            if (r.isEmpty) None
            else Some(run(r, w.toSet))
          }

          def scanFamilies(x: Id): Set[Family] =
            problem.arcs.getOrElse(x, Nil).flatMap(scanFamilies).toSet ++ problem.familyMap.get(x).toList

          prune {
            // 1. If a node moved all the way, it is always the best move
            currentRoots.toList.partitionMap { id =>
              round(id, state) match {
                case fm: FurthestMoveIsBatch => Left((id, fm))
                case _                       => Right(id)
              }
            }
          } orElse prune {
            // 2. If a node doesn't have any family in other sub-trees it is always the best move
            currentRoots.toList.partitionMap { id =>
              round(id, state) match {
                case fm: FurthestMove =>
                  val rest = currentRoots - id
                  val otherNodeFamilies = rest.flatMap(scanFamilies)
                  if (otherNodeFamilies.contains(problem.familyMap(id))) Right(id)
                  else Left((id, fm))
                case _ => Right(id)
              }
            }
          } getOrElse {
            currentRoots.map(id => id -> scanFamilies(id))
            // n^2 <= (n - 1)^2 + (n - 2)
            LazyList.from(currentRoots).flatMap { id =>
              // we move id
              val r = round(id, state)
              runFor(r.states.map(_.state).map(s => (Set(id), s)), currentRoots)
            }
          }
        }
      }

      enumerateRounds(roots.toSet, PlannerState(Map.empty, Map.empty))
    }

    /*
Problem(
  n = 11,
  arcs = {
    (0,1),
    (0,2),
    (0,3),
    (0,4),

    (2,5),
    (5,8),

    (3,6),
    (3,7),

    (6,9),
    (7,10),
  },
  fam = [
    {0},
    {1},
    {2,6,8},
    {3,9},
    {5,7},
    {4,10},
  ],
  costs = [
    1,
    13,
    1,1,1,1
  ]
)
     */
    lazy val names = Array(
      "r1",
      "c1",
      "a1",
      "b1",
      "h1",
      "p1",
      "a2",
      "p2",
      "a3",
      "b2",
      "h2"
    )
    val familyNames = Array(
      "r",
      "c",
      "a",
      "b",
      "p",
      "h"
    )
    val problem = Problem(
      families = List(
        List(0),
        List(1),
        List(2, 6, 8),
        List(3, 9),
        List(5, 7),
        List(4, 10)
      ),
      arcs = Map(
        0 -> List(1, 2, 3, 4),
        2 -> List(5),
        5 -> List(8),
        3 -> List(6, 7),
        6 -> List(9),
        7 -> List(10)
      ),
      costs = Map(
        0 -> 1,
        1 -> 13,
        2 -> 1,
        3 -> 1,
        4 -> 1,
        5 -> 1
      )
    )
    val plans = solve(problem)

    def savings(state: PlannerState) = {
      state.batches.toList.map { case (fam, times) =>
        val original = problem.families(fam).size
        original - times.size
      }.sum
    }
    val best = plans.maxBy(savings)
    val uniques = plans.distinctBy { state =>
      val fams = state.batches.toList
      fams
        .flatMap { case (fam, _) =>
          val famNodes = problem.reverseFamilyMap(fam)
          val timesForNodes: Map[EndTime, List[Id]] = famNodes.groupBy(state.lookup(_))
          timesForNodes.toList
        }
        .groupBy { case (t, _) => t }
        .toList
        .sortBy { case (t, _) => t }
        .map { case (_, ids) => ids.sortBy { case (t, _) => t }.map { case (_, xs) => xs.sorted } }
    }

    def showPlan(state: PlannerState) = {
      s"""|batches
          |${state.batches.toList
        .map { case (fam, times) => "  " + familyNames(fam) + " -> " + times.map(_.toString()).toList.foldSmash("{", ",", "}") }
        .mkString_("\n")}
          |times
          |${state.lookup.toList.map { case (id, time) => "  " + names(id) + " -> " + time.toString() }.mkString_("\n")}""".stripMargin
    }
    println {
      s"""|savings
          |${savings(best)}
          |best:
          |${showPlan(best)}
          |$i rounds
          |visited:
          |${plans.size}
          |#unique plans
          |${uniques.size}
          |unique plans:
          |${""}//uniques.map(showPlan).mkString_("\n\nplan:\n")}
          |""".stripMargin
    }
    fail("die")
  }
}
