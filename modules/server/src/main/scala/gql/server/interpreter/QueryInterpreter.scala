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

import cats.effect._
import cats.implicits._
import cats._
import cats.data._
import io.circe._
import gql.server.planner._
import gql._
import gql.preparation._
import cats.effect.std.Supervisor
import fs2.concurrent.SignallingRef
import scala.collection.immutable.ArraySeq

/** The [[QueryInterpreter]] will prepare a query for execution by inspecting the ast and planning the query accordingly. Once all inputs
  * have been prepared, the execution AST is passed to the [[SubqueryInterpreter]] for evaluation.
  */
trait QueryInterpreter[F[_], A] {
  import QueryInterpreter._

  def interpret(
      values: List[A],
      delta: StreamingAdditions[F]
  ): F[Results]
}

object QueryInterpreter {
  final case class Results(
      data: List[(Cursor, PatchOp)],
      errors: Chain[EvalFailure]
  )

  def apply[F[_], A](
      root: Prepared[F, A],
      schemaState: SchemaState[F],
      throttle: F ~> F,
      sup: Supervisor[F],
      api: StreamingApi[F],
      counter: SignallingRef[F, Int],
      rootRes: Res[F]
  )(implicit stats: Statistics[F], planner: Planner[F], F: Async[F]) = {
    Analyzer
      .analyzeWith[F, Unit](_.analyzePrepared(root))
      .flatMap(planner.plan(_))
      .map { plan =>
        new QueryInterpreter[F, A] {
          def interpret(
              values: List[A],
              delta: StreamingAdditions[F]
          ): F[Results] = {
            for {
              errors <- F.ref(Chain.empty[EvalFailure])
              qb <- QueryPlanBatches.make[F](schemaState, plan, stats, errors, throttle)
              inter = new SubqueryInterpreter(sup, stats, throttle, errors, qb, api, counter, delta)
              flats <- inter.interpretPrepared(
                root,
                ArraySeq.from(values.map(a => EvalNode.empty(a, rootRes)))
              )
              errs <- errors.get
            } yield Results(flats.toList.map(x => (x.cursor, x.value)), errs)
          }
        }
      }
  }
}
