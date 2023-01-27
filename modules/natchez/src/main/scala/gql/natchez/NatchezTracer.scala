/*
 * Copyright 2022 Valdemar Grange
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
import cats.data._
import gql.parser.{QueryParser => P, _}
import cats.effect.std.Queue

object NatchezTracer {
  def traceParser[F[_]: Trace](
      parser: String => F[Either[ParseError, NonEmptyList[P.ExecutableDefinition]]]
  )(implicit F: Monad[F]): String => F[Either[ParseError, NonEmptyList[P.ExecutableDefinition]]] = query =>
    Trace[F].span("graphql.parse") {
      Trace[F].put("graphql.query" -> query) >>
        parser(query).flatMap {
          case Left(pe) =>
            Trace[F].span("graphql.parse.error") {
              Trace[F].put(
                "graphql.parse.error.message" -> TraceValue.stringToTraceValue(pe.prettyError.value)
              ) as Left(pe)
            }
          case Right(eds) => F.pure(Right(eds))
        }
    }

  def tracePreparation[F[_]: Trace, A](
      prepare: F[EitherNec[PreparedQuery.PositionalError, A]]
  )(implicit F: Monad[F]): F[EitherNec[PreparedQuery.PositionalError, A]] =
    Trace[F].span("graphql.preparation") {
      prepare.flatMap {
        case Left(pes) =>
          Trace[F].span("graphql.preparation.error") {
            pes.traverseWithIndexM { case (pe, i) =>
              Trace[F].put(
                s"graphql.preparation.error.$i.message" -> pe.message,
                s"graphql.preparation.error.$i.path" -> pe.position.position.map(_.name).mkString_(".")
              )
            } as Left(pes)
          }
        case Right(pfs) => F.pure(Right(pfs))
      }
    }

  def traceApplication[F[_]: Trace](app: Application[F])(implicit F: Concurrent[F]): Application[F] =
    app match {
      case Application.Mutation(run)     => Application.Mutation(Trace[F].span("graphql.mutation")(run))
      case Application.Query(run)        => Application.Query(Trace[F].span("graphql.query")(run))
      case Application.Subscription(run) =>
        /*
         * This is a hack, but Trace cannot form F ~> F
         * If you have a natchez extension in scope that does something like:
         * ```
         * spanFK[A](name: String): Resource[F, F ~> F]
         * ```
         * Then by all means use that instead.
         */
        Application.Subscription {
          fs2.Stream.eval(Queue.bounded[F, QueryResult](32)).flatMap { q =>
            fs2.Stream.fromQueueUnterminated(q).concurrently {
              fs2.Stream.eval {
                Trace[F].span("graphql.subscription") {
                  run
                    .evalTap { _ =>
                      // TODO Consider doing something with errors
                      Trace[F].span("graphql.subscription.result") {
                        F.unit
                      }
                    }
                    .enqueueUnterminated(q)
                    .compile
                    .drain
                }
              }
            }
          }
        }
    }

  def tracePlanner[F[_]: Trace](planner: Planner[F])(implicit F: Monad[F]): Planner[F] =
    new Planner[F] {
      override def plan(naive: Planner.NodeTree2): F[Planner.NodeTree2] =
        Trace[F].span("graphql.planner") {
          Trace[F].put("graphql.planner.naive.totalcost" -> naive.totalCost.toString()) >> {
            val optimizedF = Trace[F].span("graphql.planner.planning") {
              planner.plan(naive)
            }

            optimizedF.flatTap { optimized =>
              Trace[F].put(
                "graphql.planner.optimized.totalcost" -> optimized.totalCost.toString()
                // "graphql.planner.optimized.plandiff" -> optimized.show(showImprovement = true)
              )
            }
          }
        }
    }

  def traceSchema[F[_]: Trace, Q, M, S](schema: Schema[F, Q, M, S])(implicit M: Monad[F]): Schema[F, Q, M, S] =
    schema.copy(planner = tracePlanner(schema.planner))
}
