package gql.server.interpreter

import cats._
import cats.implicits._
import gql.preparation._

object SubgraphBatches {
  final case class State(
      childBatches: List[NodeId],
      accum: Map[NodeId, List[NodeId]]
  )
  object State {
    def empty = State(List.empty, Map.empty)
    implicit val monoid: Monoid[State] = new Monoid[State] {
      def empty = State.empty
      def combine(x: State, y: State) = State(x.childBatches ++ y.childBatches, x.accum ++ y.accum)
    }
  }

  def countStep[F[_]](state: State, step: PreparedStep[F, ?, ?]): Eval[State] = Eval.defer {
    import PreparedStep._
    step match {
      case Lift(_, _) | EmbedError(_) | GetMeta(_, _) | EmbedEffect(_) | EmbedStream(_, _) => Eval.now(state)
      case Compose(_, l, r) =>
        countStep(state, r).flatMap(countStep(_, l))
      case alg: Choose[F, ?, ?, ?, ?] =>
        val s1F = countStep(state, alg.fac).map(s => s.copy(accum = s.accum + (alg.nodeId -> s.childBatches)))
        val s2F = countStep(state, alg.fbc).map(s => s.copy(accum = s.accum + (alg.nodeId -> s.childBatches)))
        (s1F, s2F).mapN(_ |+| _)
      case alg: First[F, ?, ?, ?]    => countStep(state, alg.step)
      case alg: Batch[F, ?, ?]       => Eval.now(state.copy(childBatches = alg.nodeId :: state.childBatches))
      case alg: InlineBatch[F, ?, ?] => Eval.now(state.copy(childBatches = alg.nodeId :: state.childBatches))
    }
  }

  def countCont[F[_]](ps: PreparedStep[F, ?, ?], cont: Prepared[F, ?]): Eval[State] = Eval.defer {
    countPrep(cont).flatMap(countStep(_, ps))
  }

  def countField[F[_]](pf: PreparedField[F, ?]): Eval[State] = Eval.defer {
    pf match {
      case PreparedDataField(_, _, _, cont, _, _) => countCont(cont.edges, cont.cont)
      case PreparedSpecification(_, _, selection) => selection.foldMapA(countField(_))
    }
  }

  def countPrep[F[_]](prep: Prepared[F, ?]): Eval[State] = Eval.defer {
    prep match {
      case PreparedLeaf(_, _, _)   => Eval.now(State(Nil, Map.empty))
      case Selection(_, fields, _) => fields.foldMapA(countField(_))
      case PreparedList(id, of, _) =>
        countCont(of.edges, of.cont).map(s => s.copy(accum = s.accum + (id -> s.childBatches)))
      case PreparedOption(id, of) =>
        countCont(of.edges, of.cont).map(s => s.copy(accum = s.accum + (id -> s.childBatches)))
    }
  }
}
