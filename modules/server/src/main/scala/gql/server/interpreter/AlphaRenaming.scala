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
package gql.server.interpreter

import cats._
import cats.implicits._
import gql.preparation._
import org.typelevel.scalaccompat.annotation._

object AlphaRenaming {
  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def alphaStep[F[_], A, B, S <: Stage](scope: Int, step: PreparedStep[F, A, B, S]): Eval[PreparedStep[F, A, B, S]] =
    Eval.defer[PreparedStep[F, A, B, S]] {
      import PreparedStep._
      def now(fa: PreparedStep[F, A, B, S]): Eval[PreparedStep[F, A, B, S]] = Eval.now(fa)

      step match {
        case alg: EmbedEffect[f, b]       => now(EmbedEffect[f, b](alg.sei.alpha(scope)))
        case alg: Lift[F, A, B]           => now(Lift[F, A, B](alg.nodeId.alpha(scope), alg.f))
        case alg: EmbedError[F, B]        => now(EmbedError[F, B](alg.nodeId.alpha(scope)))
        case alg: PrecompileMeta[f, a, c] => now(PrecompileMeta[f, a, c](alg.nodeId.alpha(scope), alg.meta))
        case alg: EvalMeta[f, a]          => now(EvalMeta[f, a](alg.nodeId.alpha(scope), alg.meta))
        case alg: InlineBatch[F, k, v]    => now(InlineBatch[F, k, v](alg.run, alg.sei.alpha(scope)))
        case alg: Batch[F, k, v]          => now(Batch[F, k, v](alg.id, alg.ubi.alpha(scope)))
        case alg: Compose[F, A, a, B, s] =>
          (alphaStep(scope, alg.left), alphaStep(scope, alg.right))
            .mapN(Compose[F, A, a, B, s](alg.nodeId.alpha(scope), _, _))
        case alg: Choose[f, i, a, b, c, s] =>
          (
            alphaStep(scope, alg.fac),
            alphaStep(scope, alg.fbd)
          ).mapN(Choose[f, i, a, b, c, s](alg.nodeId.alpha(scope), _, _))
        case alg: First[f, i, a, b, s] =>
          alphaStep(scope, alg.step).map(First[f, i, a, b, s](alg.nodeId.alpha(scope), _))
        case alg: EmbedStream[f, i] => now(EmbedStream[f, i](alg.sei.alpha(scope)))
      }
    }

  def alphaCont[F[_], A, B, S <: Stage](
      scope: Int,
      cont: PreparedCont[F, A, B, S]
  ): Eval[PreparedCont[F, A, B, S]] = Eval.defer {
    (alphaStep(scope, cont.edges), alphaPrep(scope, cont.cont))
      .mapN(PreparedCont[F, A, B, S](_, _))
  }

  def alphaDataField[F[_], A, B, S <: Stage](
      scope: Int,
      field: PreparedDataField[F, A, B, S]
  ): Eval[PreparedDataField[F, A, B, S]] =
    Eval.defer {
      alphaCont(scope, field.cont)
        .map(cont => field.copy[F, A, B, S](cont = cont, nodeId = field.nodeId.alpha(scope)))
    }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def alphaField[F[_], A, S <: Stage](
      scope: Int,
      field: PreparedField[F, A, S]
  ): Eval[PreparedField[F, A, S]] = Eval.defer {
    field match {
      case pdf: PreparedDataField[F, A, b, s] => alphaDataField(scope, pdf)
      case ps: PreparedSpecification[F, A, b, s] =>
        ps.selection
          .traverse(alphaDataField(scope, _))
          .map(PreparedSpecification(ps.nodeId.alpha(scope), ps.specialization, _))
    }
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def alphaPrep[F[_], A, S <: Stage](
      scope: Int,
      prep: Prepared[F, A, S]
  ): Eval[Prepared[F, A, S]] = Eval.defer[Prepared[F, A, S]] {
    prep match {
      case PreparedLeaf(nid, name, f) => Eval.now(PreparedLeaf(nid.alpha(scope), name, f))
      case Selection(nodeId, fields, source) =>
        fields.traverse(alphaField(scope, _)).map(Selection(nodeId.alpha(scope), _, source))
      case alg: PreparedList[F, a, b, c, s] =>
        alphaCont(scope, alg.of).map(PreparedList(alg.id.alpha(scope), _, alg.toSeq))
      case alg: PreparedOption[F, i, o, s] =>
        alphaCont(scope, alg.of).map(PreparedOption(alg.id.alpha(scope), _))
    }
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def alphaContinuation[F[_], A](scope: Int, cont: Continuation[F, A]): Eval[Continuation[F, A]] = Eval.defer {
    cont match {
      case Continuation.Done(prep) =>
        alphaPrep(scope, prep).map[Continuation[F, A]](Continuation.Done(_))
      case Continuation.Continue(step, cont) =>
        (alphaStep(scope, step), alphaContinuation(scope, cont)).mapN[Continuation[F, A]](Continuation.Continue(_, _))
      case Continuation.Contramap(f, cont) =>
        alphaContinuation(scope, cont).map[Continuation[F, A]](Continuation.Contramap(f, _))
      case r: Continuation.Rethrow[F, i] =>
        alphaContinuation(scope, r.inner)
          .map[Continuation[F, A]](Continuation.Rethrow(r.nodeId.alpha(scope), _))
    }
  }
}
