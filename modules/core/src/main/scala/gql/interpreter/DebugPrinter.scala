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
import cats.effect._

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

    def fields(ds: Doc*): Doc = Doc.intercalate(Doc.char(',') + Doc.line, ds)

    def kvs(kvs: (String, Doc)*): Doc =
      fields(kvs.map { case (k, v) => kv(k, v) }: _*)

    def recordBy(name: String, left: Doc, right: Doc, d: Doc): Doc =
      Doc.text(name) + d.tightBracketBy(left, right)

    def record(name: String, d: Doc): Doc =
      recordBy(name, Doc.char('('), Doc.char(')'), d)

    def preparedFieldDoced[F[_]]: Doced[PreparedField[F, ?]] = pf =>
      pf match {
        case PreparedSpecification(tn, _, sels) =>
          record(
            "PreparedSpecification",
            kvs(
              "typename" -> Doc.text(tn),
              "selections" -> recordBy(
                "PreparedSelections",
                Doc.char('{'),
                Doc.char('}'),
                fields(sels.map(preparedFieldDoced(_)).toList: _*)
              )
            )
          )
        case PreparedDataField(name, alias, cont) =>
          record(
            "PreparedDataField",
            kvs(
              "name" -> Doc.text(name),
              "alias" -> Doc.text(alias.toString()),
              "cont" -> preparedContDoced(cont)
            )
          )
      }

    def preparedDoced[F[_]]: Doced[Prepared[F, ?]] = pc =>
      pc match {
        case Selection(fields) =>
          record(
            "Selection",
            Doc.intercalate(Doc.char(',') + Doc.space, fields.map(preparedFieldDoced(_)).toList)
          )
        case PreparedList(of, _)   => record("PreparedList", preparedContDoced(of))
        case PreparedOption(of)    => record("PreparedOption", preparedContDoced(of))
        case PreparedLeaf(name, _) => record("PreparedLeaf", Doc.text(name))
      }

    def preparedContDoced[F[_]]: Doced[PreparedCont[F, ?, ?]] = pc =>
      record(
        "PreparedCont",
        kvs(
          "edges" -> preparedStepDoced(pc.edges),
          "cont" -> preparedDoced(pc.cont)
        )
      )

    def preparedStepDoced[F[_]]: Doced[PreparedStep[F, ?, ?]] = { pc =>
      import PreparedStep._
      pc match {
        case Lift(_)        => Doc.text("Lift(...)")
        case EmbedEffect(_) => Doc.text("EmbedEffect")
        case EmbedStream(signal, _) =>
          record("EmbedStream", kvs("signal" -> Doc.text(signal.toString())))
        case EmbedError() => Doc.text("EmbedError")
        case Compose(left, right) =>
          record("Compose", kvs("left" -> preparedStepDoced(left), "right" -> preparedStepDoced(right)))
        case GetMeta(meta) =>
          record("GetMeta", kvs("meta" -> Doc.text(meta.toString())))
        case First(step) =>
          record("First", kvs("step" -> preparedStepDoced(step)))
        case Batch(id, globalEdgeId) =>
          record("Batch", kvs("id" -> Doc.text(id.toString()), "globalEdgeId" -> Doc.text(globalEdgeId.toString())))
        case Choose(fac, fbc) =>
          record("Choose", kvs("fac" -> preparedStepDoced(fac), "fbc" -> preparedStepDoced(fbc)))
      }
    }

    def stepContDoced[F[_]]: Doced[StepCont[F, ?, ?]] = sc =>
      sc match {
        case StepCont.Done(p) => record("StepCont.Done", preparedDoced(p))
        case StepCont.Continue(step, next) =>
          record(
            "StepCont.Continue",
            kvs(
              "step" -> preparedStepDoced(step),
              "cont" -> stepContDoced(next)
            )
          )
        case StepCont.Join(_, next)      => record("StepCont.Join", stepContDoced(next))
        case StepCont.TupleWith(_, next) => record("StepCont.TupleWith", stepContDoced(next))
      }

    def streamingDataDoced[F[_]]: Doced[Interpreter.StreamingData[F, ?, ?]] = sd =>
      record(
        "StreamingData",
        kvs(
          "originIndex" -> Doc.text(sd.originIndex.toString()),
          "edges" -> stepContDoced(sd.edges),
          "value" -> Doc.text(sd.value.leftMap(_.getMessage()).map(_.getClass().getName()).toString())
        )
      )

    def leasedValueDoced[F[_], A](isOpen: Boolean, names: Map[Unique.Token, String])(implicit
        D: Doced[A]
    ): Doced[ConfiguredStreamScopes.LeasedValue[F, A]] = lv =>
      record(
        "LeasedValue",
        kvs(
          "name" -> Doc.text(names.get(lv.scope.id).getOrElse(lv.scope.id.toString())),
          "signal" -> Doc.text(lv.signal.toString()),
          "open" -> Doc.text(isOpen.toString()),
          "value" -> D(lv.value)
        )
      )

    def resourceInfoDoced[F[_], A](isOpen: Boolean, names: Map[Unique.Token, String])(implicit
        D: Doced[A]
    ): Doced[SignalScopes.ResourceInfo[F, A]] = { ri =>
      def makeName(id: Unique.Token): Doc =
        Doc.text(names.get(id).getOrElse(id.toString()))

      record(
        "ResourceInfo",
        kvs(
          "parentName" -> makeName(ri.parent.scope.id),
          "name" -> makeName(ri.scope.id),
          "open" -> Doc.text(isOpen.toString()),
          "value" -> D(ri.value)
        )
      )
    }
  }
}
