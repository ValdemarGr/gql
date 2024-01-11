package gql.server.interpreter

import cats._
import cats.implicits._
import gql.preparation._

object AlphaRenaming {
  def alphaStep[F[_], A, B](scope: Int, step: PreparedStep[F, A, B]): Eval[PreparedStep[F, A, B]] = Eval.defer {
    import PreparedStep._
    step match {
      case Lift(nid, a)              => Eval.now(Lift(nid.alpha(scope), a))
      case EmbedError(nid)           => Eval.now(EmbedError(nid.alpha(scope)))
      case GetMeta(nid, meta)        => Eval.now(GetMeta(nid.alpha(scope), meta))
      case alg: EmbedEffect[f, i]    => Eval.now(EmbedEffect[f, i](alg.sei.alpha(scope)))
      case alg: InlineBatch[f, k, v] => Eval.now(InlineBatch[f, k, v](alg.run, alg.sei.alpha(scope)))
      case alg: Batch[f, k, v]       => Eval.now(Batch[f, k, v](alg.id, alg.ubi.alpha(scope)))
      case Compose(nid, l, r)        => Eval.now(Compose(nid.alpha(scope), l, r))
      case alg: Choose[f, i, a, b, c] =>
        (alphaStep(scope, alg.fac), alphaStep(scope, alg.fbd)).mapN(Choose[f, i, a, b, c](alg.nodeId.alpha(scope), _, _))
      case alg: First[f, i, a, b] => alphaStep(scope, alg.step).map(First[f, i, a, b](alg.nodeId.alpha(scope), _))
      case alg: EmbedStream[f, i] => Eval.now(EmbedStream[f, i](alg.signal, alg.sei.alpha(scope)))
    }
  }

  def alphaCont[F[_], A, B](scope: Int, cont: PreparedCont[F, A, B]): Eval[PreparedCont[F, A, B]] = Eval.defer {
    (alphaStep(scope, cont.edges), alphaPrep(scope, cont.cont))
      .mapN(PreparedCont[F, A, B](_, _))
  }

  def alphaDataField[F[_], A, B](scope: Int, field: PreparedDataField[F, A, B]): Eval[PreparedDataField[F, A, B]] =
    Eval.defer {
      alphaCont(scope, field.cont).map(cont => field.copy[F, A, B](cont = cont, nodeId = field.nodeId.alpha(scope)))
    }

  def alphaField[F[_], A](scope: Int, field: PreparedField[F, A]): Eval[PreparedField[F, A]] = Eval.defer {
    field match {
      case pdf: PreparedDataField[F, A, b] => alphaDataField(scope, pdf)
      case ps: PreparedSpecification[F, A, b] =>
        ps.selection
          .traverse(alphaDataField(scope, _))
          .map(PreparedSpecification(ps.nodeId.alpha(scope), ps.specialization, _))
    }
  }

  def alphaPrep[F[_], A](scope: Int, prep: Prepared[F, A]): Eval[Prepared[F, A]] = Eval.defer {
    prep match {
      case PreparedLeaf(nid, name, f) => Eval.now(PreparedLeaf(nid.alpha(scope), name, f))
      case Selection(nodeId, fields, source) =>
        fields.traverse(alphaField(scope, _)).map(Selection(nodeId.alpha(scope), _, source))
      case alg: PreparedList[F, a, b, c] =>
        alphaCont(scope, alg.of).map(PreparedList(alg.id.alpha(scope), _, alg.toSeq))
      case alg: PreparedOption[F, i, o] =>
        alphaCont(scope, alg.of).map(PreparedOption(alg.id.alpha(scope), _))
    }
  }

  def alphaContinuation[F[_], A](scope: Int, cont: Continuation[F, A]): Eval[Continuation[F, A]] = Eval.defer {
    cont match {
      case Continuation.Done(prep) => alphaPrep(scope, prep).map(Continuation.Done(_))
      case Continuation.Continue(step, cont) =>
        (alphaStep(scope, step), alphaContinuation(scope, cont)).mapN(Continuation.Continue(_, _))
      case Continuation.Contramap(f, cont) => alphaContinuation(scope, cont).map(Continuation.Contramap(f, _))
    }
  }
}
