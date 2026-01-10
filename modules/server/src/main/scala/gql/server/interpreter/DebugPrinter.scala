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
import org.typelevel.paiges._
import gql.preparation._

trait DebugPrinter[F[_]] {
  def apply(s: => String): F[Unit]

  def eval(fa: => F[String]): F[Unit]
}

object DebugPrinter {
  def apply[F[_]: Monad](f: String => F[Unit]): DebugPrinter[F] = new DebugPrinter[F] {
    def apply(s: => String): F[Unit] = f(s)

    def eval(fa: => F[String]): F[Unit] = fa.flatMap(f)
  }

  def noop[F[_]](implicit F: Monad[F]): DebugPrinter[F] = new DebugPrinter[F] {
    def apply(s: => String): F[Unit] = F.unit

    def eval(fa: => F[String]): F[Unit] = F.unit
  }

  object Printer {
    def kv(k: String, v: Doc): Doc = Doc.text(k) + Doc.space + Doc.char('=') + Doc.space + v

    def fields(ds: Doc*): Doc = Doc.intercalate(Doc.char(',') + Doc.line, ds)

    def kvs(kvs: (String, Doc)*): Doc =
      fields(kvs.map { case (k, v) => kv(k, v) }: _*)

    def recordBy(name: String, left: Doc, right: Doc, d: Doc): Doc =
      Doc.text(name) + d.tightBracketBy(left, right)

    def record(name: String, d: Doc): Doc =
      recordBy(name, Doc.char('('), Doc.char(')'), d)

    def preparedFieldDoced[F[_]]: Document[PreparedField[F, ?, ?]] =
      Document.instance[PreparedField[F, ?, ?]] {
        case PreparedSpecification(_, spec, sels) =>
          record(
            "PreparedSpecification",
            kvs(
              "typename" -> Doc.text(spec.typename),
              "selections" -> recordBy(
                "PreparedSelections",
                Doc.char('{'),
                Doc.char('}'),
                fields(sels.map(preparedFieldDoced.document(_)).toList: _*)
              )
            )
          )
        case PreparedDataField(_, name, alias, cont, _, _) =>
          record(
            "PreparedDataField",
            kvs(
              "name" -> Doc.text(name),
              "alias" -> Doc.text(alias.toString()),
              "cont" -> preparedContDoced.document(cont)
            )
          )
      }

    def preparedDoced[F[_]]: Document[Prepared[F, ?, ?]] =
      Document.instance[Prepared[F, ?, ?]] {
        case Selection(_, fields, _) =>
          record(
            "Selection",
            Doc.intercalate(Doc.char(',') + Doc.space, fields.map(preparedFieldDoced.document(_)).toList)
          )
        case PreparedList(_, of, _)   => record("PreparedList", preparedContDoced.document(of))
        case PreparedOption(_, of)    => record("PreparedOption", preparedContDoced.document(of))
        case PreparedLeaf(_, name, _) => record("PreparedLeaf", Doc.text(name))
      }

    def preparedContDoced[F[_]]: Document[PreparedCont[F, ?, ?, ?]] =
      Document.instance[PreparedCont[F, ?, ?, ?]] { pc =>
        record(
          "PreparedCont",
          kvs(
            "edges" -> preparedStepDoced.document(pc.edges),
            "cont" -> preparedDoced.document(pc.cont)
          )
        )
      }

    def preparedStepDoced[F[_]]: Document[PreparedStep[F, ?, ?, ?]] = {
      import PreparedStep._
      Document.instance[PreparedStep[F, ?, ?, ?]] {
        case Lift(_, _)        => Doc.text("Lift(...)")
        case EmbedEffect(_)    => Doc.text("EmbedEffect")
        case InlineBatch(_, _) => Doc.text("InlineBatch")
        case EmbedStream(_) =>
          record("EmbedStream", kvs())
        case EmbedError(_) => Doc.text("EmbedError")
        case Compose(_, left, right) =>
          record("Compose", kvs("left" -> preparedStepDoced.document(left), "right" -> preparedStepDoced.document(right)))
        case EvalMeta(_, meta) =>
          record("EvalMeta", kvs("meta" -> Doc.text(meta.toString())))
        case SubstVars(_, _) =>
          Doc.text("SubstVars(...)")
        case PrecompileMeta(_, meta) =>
          record("PrecompileMeta", kvs("meta" -> Doc.text(meta.toString())))
        case First(_, step) =>
          record("First", kvs("step" -> preparedStepDoced.document(step)))
        case Batch(id, globalEdgeId) =>
          record("Batch", kvs("id" -> Doc.text(id.toString()), "globalEdgeId" -> Doc.text(globalEdgeId.toString())))
        case Choose(_, fac, fbc) =>
          record("Choose", kvs("fac" -> preparedStepDoced.document(fac), "fbc" -> preparedStepDoced.document(fbc)))
      }
    }

    def continuationDoced[F[_]]: Document[Continuation[F, ?]] = cont =>
      cont match {
        case Continuation.Done(p) => record("Continuation.Done", preparedDoced.document(p))
        case Continuation.Continue(step, next) =>
          record(
            "Continuation.Continue",
            kvs(
              "step" -> preparedStepDoced.document(step),
              "cont" -> continuationDoced.document(next)
            )
          )
        case Continuation.Contramap(_, next) =>
          record(
            "Continuation.Contramap",
            kvs(
              "cont" -> continuationDoced.document(next)
            )
          )
        case Continuation.Rethrow(_, inner) =>
          record(
            "Continuation.Rethrow",
            kvs(
              "inner" -> continuationDoced.document(inner)
            )
          )
      }
  }
}
