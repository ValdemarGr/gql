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
package gql.interpreter

import cats._
import cats.implicits._
import org.typelevel.paiges._

trait DebugPrinter[F[_]] {
  def apply(s: => String): F[Unit]

  def eval(fa: => F[String]): F[Unit]
}

object DebugPrinter {
  def apply[F[_]: Monad](f: String => F[Unit]): DebugPrinter[F] = new DebugPrinter[F] {
    def apply(s: => String): F[Unit] = f(s)

    def eval(fa: => F[String]): F[Unit] = fa.flatMap(f)
  }

  def noop[F[_]](implicit F: Monad[F]): DebugPrinter[F] = apply(_ => F.unit)

  import gql.PreparedQuery._

  object Printer {
    def kv(k: String, v: Doc): Doc = Doc.text(k) + Doc.space + Doc.char('=') + Doc.space + v

    def fields(ds: Doc*) = Doc.intercalate(Doc.char(',') + Doc.line, ds)

    def kvs(kvs: (String, Doc)*): Doc =
      fields(kvs.map { case (k, v) => kv(k, v) }: _*)

    def recordBy(name: String, left: Doc, right: Doc, d: Doc): Doc =
      Doc.text(name) + d.tightBracketBy(left, right)

    def record(name: String, d: Doc): Doc =
      recordBy(name, Doc.char('('), Doc.char(')'), d)

    def preparedFieldDoc[F[_]](pf: PreparedField[F, ?]): Doc = pf match {
      case PreparedSpecification(tn, _, sels) =>
        record(
          "PreparedSpecification",
          kvs(
            "typename" -> Doc.text(tn),
            "selections" -> recordBy(
              "PreparedSelections",
              Doc.char('{'),
              Doc.char('}'),
              fields(sels.map(preparedFieldDoc[F]).toList: _*)
            )
          )
        )
      case PreparedDataField(name, alias, cont) =>
        record(
          "PreparedDataField",
          kvs(
            "name" -> Doc.text(name),
            "alias" -> Doc.text(alias.toString()),
            "cont" -> preparedContDoc[F](cont)
          )
        )
    }

    def preparedDoc[F[_]](pc: Prepared[F, ?]): Doc = pc match {
      case Selection(fields) =>
        record(
          "Selection",
          Doc.intercalate(Doc.char(',') + Doc.space, fields.map(preparedFieldDoc[F]).toList)
        )
      case PreparedList(of, _)   => record("PreparedList", preparedContDoc(of))
      case PreparedOption(of)    => record("PreparedOption", preparedContDoc(of))
      case PreparedLeaf(name, _) => record("PreparedLeaf", Doc.text(name))
    }

    def preparedContDoc[F[_]](pc: PreparedCont[F, ?, ?]): Doc =
      record(
        "PreparedCont",
        kvs(
          "edges" -> preparedStepDoc(pc.edges),
          "cont" -> preparedDoc(pc.cont)
        )
      )

    def preparedStepDoc[F[_]](pc: PreparedStep[F, ?, ?]): Doc = {
      import PreparedStep._
      pc match {
        case Lift(_)        => Doc.text("Lift(...)")
        case EmbedEffect(_) => Doc.text("EmbedEffect")
        case EmbedStream(signal, _) =>
          record("EmbedStream", kvs("signal" -> Doc.text(signal.toString())))
        case EmbedError() => Doc.text("EmbedError")
        case Compose(left, right) =>
          record("Compose", kvs("left" -> preparedStepDoc[F](left), "right" -> preparedStepDoc[F](right)))
        case GetMeta(meta) =>
          record("GetMeta", kvs("meta" -> Doc.text(meta.toString())))
        case First(step) =>
          record("First", kvs("step" -> preparedStepDoc(step)))
        case Batch(id, globalEdgeId) =>
          record("Batch", kvs("id" -> Doc.text(id.toString()), "globalEdgeId" -> Doc.text(globalEdgeId.toString())))
        case Choose(fac, fbc) =>
          record("Choose", kvs("fac" -> preparedStepDoc(fac), "fbc" -> preparedStepDoc(fbc)))
      }
    }

    def stepContDoc[F[_]](sc: StepCont[F, ?, ?]): Doc = sc match {
      case StepCont.Done(p) => record("StepCont.Done", preparedDoc(p))
      case StepCont.Continue(step, next) =>
        record(
          "StepCont.Continue",
          kvs(
            "step" -> preparedStepDoc(step),
            "cont" -> stepContDoc(next)
          )
        )
      case StepCont.Join(_, next)      => record("StepCont.Join", stepContDoc(next))
      case StepCont.TupleWith(_, next) => record("StepCont.TupleWith", stepContDoc(next))
    }

    def streamingDataDoc[F[_]](sd: Interpreter.StreamingData[F, ?, ?]): Doc =
      record(
        "StreamingData",
        kvs(
          "originIndex" -> Doc.text(sd.originIndex.toString()),
          "edges" -> stepContDoc(sd.edges),
          "value" -> Doc.text(sd.value.leftMap(_.getMessage()).map(_.getClass().getName()).toString())
        )
      )
  }
}
