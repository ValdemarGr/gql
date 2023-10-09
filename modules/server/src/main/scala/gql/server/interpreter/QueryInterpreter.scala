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

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import cats._
import cats.data._
import io.circe._
import gql.server.planner._
import gql._
import gql.preparation._
import cats.effect.std.Supervisor

/** The [[QueryInterpreter]] will prepare a query for execution by inspecting the ast and planning the query accordingly. Once all inputs
  * have been prepared, the execution AST is passed to the [[SubqueryInterpreter]] for evaluation.
  */
trait QueryInterpreter[F[_]] {
  import QueryInterpreter._

  def interpretOne[A, B](input: Input[F, A, B], batching: BatchAccumulator[F]): F[Result[F]]

  def interpretAll(inputs: NonEmptyList[Input[F, ?, ?]]): F[Results[F]]
}

object QueryInterpreter {
  final case class Input[F[_], A, B](
      data: IndexedData[F, A],
      cont: StepCont[F, A, B]
  )

  object Input {
    def root[F[_], A](data: A, cont: Prepared[F, A], scope: Scope[F]): Input[F, A, Json] =
      Input(IndexedData(0, EvalNode.empty(data, scope)), StepCont.Done(cont))
  }

  final case class Results[F[_]](errors: Chain[EvalFailure], roots: NonEmptyList[EvalNode[F, Json]])

  final case class Result[F[_]](errors: Chain[EvalFailure], rootData: EvalNode[F, Json])

  def apply[F[_]: Async: Statistics](
      schemaState: SchemaState[F],
      ss: SignalScopes[F, StreamingData[F, ?, ?]],
      throttle: F ~> F
  )(implicit planner: Planner[F]) =
    new QueryInterpreter[F] {
      def interpretOne[A, B](input: Input[F, A, B], batching: BatchAccumulator[F]): F[Result[F]] =
        Supervisor[F].use { sup =>
          SubqueryInterpreter[F](ss, batching, sup, throttle)
            .runEdgeCont(Chain(input.data), input.cont)
            .run
            .map { case (fails, succs) =>
              val (_, j) = succs.headOption.get
              Result(fails, input.data.node.setValue(j))
            }
        }

      def interpretAll(inputs: NonEmptyList[Input[F, ?, ?]]): F[Results[F]] = {
        // We perform an alpha renaming for every input to ensure that every node is distinct
        val indexed = inputs.zipWithIndex
        for {
          costTree <- Analyzer.analyzeWith[F, Unit] { implicit analyzer =>
            indexed.traverse_ { case (input, i) => analyzeCost[F](input).modify(_.alpha(i)) }
          }
          planned <- planner.plan(costTree)
          accumulator <- BatchAccumulator[F](schemaState, planned, throttle)
          results <- indexed.parTraverse { case (input, i) => interpretOne(input, accumulator.alpha(i)) }
          batchErrors <- accumulator.getErrors
          allErrors = Chain.fromSeq(results.toList).flatMap(_.errors) ++ Chain.fromSeq(batchErrors)
        } yield Results(allErrors, results.map(_.rootData))
      }
    }

  def analyzeCost[F[_]: Monad](input: Input[F, ?, ?])(implicit
      analyzer: Analyzer[Analyzer.H[F, *]]
  ): Analyzer.H[F, Unit] = {
    def contCost(step: StepCont[F, ?, ?]): Analyzer.H[F, Unit] =
      step match {
        case d: StepCont.Done[F, i]           => analyzer.analyzePrepared(d.prep)
        case c: StepCont.Continue[F, ?, ?, ?] => analyzer.analyzeStep(c.step) *> contCost(c.next)
        case StepCont.Join(_, next)           => contCost(next)
        case StepCont.TupleWith(_, next)      => contCost(next)
      }

    contCost(input.cont)
  }
}
