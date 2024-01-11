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

  def interpretOne[A](input: Input[F, A], sgb: SubgraphBatches[F], errors: Ref[F, Chain[EvalFailure]]): F[Json]

  def interpretAll(inputs: NonEmptyList[Input[F, ?]]): F[Results]
}

object QueryInterpreter {
  final case class Input[F[_], A](
      continuation: Continuation[F, A],
      data: EvalNode[F, A]
  )

  object Input {
    def root[F[_], A](data: A, cont: Prepared[F, A], scope: Scope[F]): Input[F, A] =
      Input(Continuation.Done(cont), EvalNode.empty(data, scope))
  }

  final case class Results(
      data: NonEmptyList[(Cursor, Json)],
      errors: Chain[EvalFailure]
  )

  def apply[F[_]](
      schemaState: SchemaState[F],
      ss: SignalScopes[F, StreamData[F, ?]],
      throttle: F ~> F
  )(implicit stats: Statistics[F], planner: Planner[F], F: Async[F]) =
    new QueryInterpreter[F] {
      def interpretOne[A](input: Input[F, A], sgb: SubgraphBatches[F], errors: Ref[F, Chain[EvalFailure]]): F[Json] =
        Supervisor[F].use { sup =>
          val go = new SubqueryInterpreter(ss, sup, stats, throttle, errors, sgb)
          go.goCont(input.continuation, input.data)
        }

      def interpretAll(inputs: NonEmptyList[Input[F, ?]]): F[Results] = {
        /* We perform an alpha renaming for every input to ensure that every node is distinct
         * Every object that the SubqueryInterpreter passes a NodeId to,
         * translates that NodeId to its proper alpha-renamed id.
         *
         * For instance if SubqueryInterpreter_3 invokes an operation that regards NodeId 5 in partition 3,
         * then that NodeId must be translated to (3, 5).
         *
         * Every structure foreign to the SubqueryInterpreter, such as the plan, must record nodes as alpha renamed.
         * For all structures below, this occurs after construction.
         */
        val indexed = inputs.mapWithIndex { case (input, i) =>
          input.copy(continuation = AlphaRenaming.alphaContinuation(i, input.continuation).value)
        }
        for {
          costTree <- indexed.foldMapA(input => analyzeCost[F](input.continuation))
          planned <- planner.plan(costTree)
          counts = indexed.foldMap { in =>
            SubgraphBatches.countContinuation(SubgraphBatches.State.empty, in.continuation)
          }.value
          sb <- SubgraphBatches.make[F](schemaState, counts, planned, stats, throttle)
          errors <- F.ref(Chain.empty[EvalFailure])
          results <- indexed.parTraverse(input => interpretOne(input, sb, errors) tupleLeft input.data.cursor)
          batchErrors <- sb.getErrors
          interpreterErrors <- errors.get
          allErrors = batchErrors ++ interpreterErrors
        } yield Results(results, allErrors)
      }
    }

  def analyzeCost[F[_]: Monad: Statistics](cont: Continuation[F, ?]): F[NodeTree] = {
    Analyzer.analyzeWith[F, Unit] { analyzer =>
      def contCost(cont: Continuation[F, ?]): Analyzer.H[F, Unit] =
        cont match {
          case Continuation.Done(prep)             => analyzer.analyzePrepared(prep)
          case fa: Continuation.Continue[F, ?, ?]  => analyzer.analyzeStep(fa.step) *> contCost(fa.next)
          case fa: Continuation.Contramap[F, ?, ?] => contCost(fa.next)
        }

      contCost(cont)
    }
  }
}
