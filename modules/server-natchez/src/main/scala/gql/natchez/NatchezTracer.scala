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
package gql.natchez

import cats.effect.{Trace => _, _}
import gql._
import _root_.natchez._
import cats._
import cats.implicits._
import gql.server.planner._
import io.circe.syntax._
import io.circe._

object NatchezTracer {
  def traceCompilation[F[_]: Trace](
      query: String,
      variables: Map[String, Json],
      operationName: Option[String]
  )(outcome: => Compiler.Outcome[F])(implicit F: Monad[F]): F[Compiler.Outcome[F]] =
    Trace[F].span("graphql.compilation") {
      Trace[F].put(
        "graphql.compilation.query" -> query,
        "graphql.compilation.variables" -> io.circe.JsonObject.fromMap(variables).asJson.noSpaces,
        "graphql.compilation.operationName" -> operationName.mkString
      ) >>
        F.unit.flatMap { _ =>
          val o = outcome
          val putF = o match {
            case Left(CompilationError.Parse(err)) =>
              Trace[F].put("graphql.compilation.parsing.error" -> err.prettyError.value)
            case Left(CompilationError.Preparation(errs)) =>
              errs.zipWithIndex.traverse_ { case (pe, i) =>
                Trace[F].put(
                  s"graphql.preparation.error.${i}.message" -> pe.message,
                  s"graphql.preparation.error.${i}.path" -> pe.position.formatted
                )
              }
            case Right(_) => F.unit
          }

          putF.as(o)
        }
    }

  def traceApplication[F[_]: Trace: MonadCancelThrow](app: Application[F]): Application[F] = {
    def traceQr(opName: String, qr: QueryResult): F[Unit] =
      qr.errors.zipWithIndex.traverse_ { case (e, i) =>
        val path = e.path.mkString_(",")
        val msgF = e.error match {
          case Left(ex)   => Trace[F].attachError(ex)
          case Right(msg) => Trace[F].put(s"graphql.${opName}.error.${i}.message" -> msg)
        }
        Trace[F].put(s"graphql.${opName}.error.${i}.path" -> path) >> msgF
      }

    app match {
      case Application.Query(run) =>
        Application.Query(Trace[F].span("graphql.query")(run.flatTap(traceQr("query", _))))
      case Application.Mutation(run) =>
        Application.Mutation(Trace[F].span("graphql.mutation")(run.flatTap(traceQr("mutation", _))))
      case Application.Subscription(run) =>
        Application.Subscription {
          Trace[fs2.Stream[F, *]].span("graphql.subscription") {
            run.evalTap(traceQr("subscription", _))
          }
        }
    }
  }

  def traceQuery[F[_]: Trace: MonadCancelThrow](
      query: String,
      variables: Map[String, Json],
      operationName: Option[String]
  )(outcome: => Compiler.Outcome[F]): F[Compiler.Outcome[F]] =
    traceCompilation(query, variables, operationName)(outcome).map(_.map(traceApplication[F]))

  def tracePlanner[F[_]: Trace](planner: Planner[F])(implicit F: Monad[F]): Planner[F] =
    new Planner[F] {
      override def plan(naive: NodeTree): F[OptimizedDAG] =
        Trace[F].span("graphql.planner.plan") {
          val et = naive.endTimes.values.maxOption.getOrElse(0d)
          Trace[F].put(
            "graphql.planner.plan.size" -> naive.all.size,
            "graphql.planner.plan.endTime" -> et
          ) >>
            planner.plan(naive).flatTap { plan =>
              Trace[F].put(
                "graphql.planner.plan.totalCost" -> plan.totalCost,
                "graphql.planner.plan.optimized" -> plan.optimizedCost,
                "graphql.planner.plan.numBatches" -> plan.batches.size,
                "graphql.planner.plan.avgBatchSize" ->
                  plan.batches.iterator.map { case (ns, _) => ns.size }.sum.toFloat /
                  plan.batches.size.toFloat
              )
            }
        }
    }

  def traceSchema[F[_]: Trace, Q, M, S](schema: Schema[F, Q, M, S])(implicit M: Monad[F]): Schema[F, Q, M, S] =
    schema.copy(planner = tracePlanner(schema.planner))
}
