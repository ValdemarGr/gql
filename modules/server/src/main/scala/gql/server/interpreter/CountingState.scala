package gql.server.interpreter

import cats.effect._
import cats._
import cats.implicits._
import gql.resolver._
import gql.preparation._
import gql.server.planner.NodeTree

trait CountingState[F[_]] {
  def visitMultiplicity(n: Int): F[CountingState[F]]

  def submit[K, V](ubi: UniqueBatchInstance[K, V], k: K): F[Option[Map[K, V]]]
}

object CountingState {
  final case class State(
      childBatches: Set[NodeId],
      accum: Map[NodeId, Set[NodeId]]
  )
  object State {
    def empty = State(Set.empty, Map.empty)
    implicit val monoid: Monoid[State] = new Monoid[State] {
      def empty = State.empty
      def combine(x: State, y: State) = State(x.childBatches ++ y.childBatches, x.accum ++ y.accum)
    }
  }

  def countStep[F[_]](state: State, step: PreparedStep[F, ?, ?]): Eval[State] = Eval.defer {
    import PreparedStep._
    step match {
      case Lift(_) | EmbedError() | GetMeta(_) => Eval.now(state)
      case Compose(l, r)                       => countStep(state, r).flatMap(countStep(_, l))
      case alg: Choose[F, ?, ?, ?, ?] =>
        countStep(state, alg.fac).flatMap { s1 =>
          countStep(state, alg.fbc).map(s2 => s1 |+| s2)
        }
      case alg: First[F, ?, ?, ?] => countStep(state, alg.step)
    }
    ???
  }

  def countCont[F[_]](ps: PreparedStep[F, ?, ?], cont: Prepared[F, ?]) = Eval.defer {
    ???
  }

  def countPrep[F[_]](prep: Prepared[F, ?]) = Eval.defer {
    prep match {
      case PreparedLeaf(_, _)      => Eval.now(State(Set.empty, Map.empty))
      case Selection(fields, _)    => ???
      case PreparedList(id, of, _) => ???
      case PreparedOption(id, of)  => ???
    }
  }

  def init[F[_]](
      dag: NodeTree
  )(implicit F: Async[F]) = {
    F
  }
}
