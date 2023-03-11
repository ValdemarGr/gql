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
package gql

import cats._
import cats.implicits._
import io.circe._
import cats.effect._
import gql.interpreter.{Interpreter, DebugPrinter}
import cats.data._
import scala.concurrent.duration._

sealed trait CompilationError {
  def asGraphQL: JsonObject
}
object CompilationError {
  final case class Parse(error: gql.parser.ParseError) extends CompilationError {
    lazy val asGraphQL: JsonObject = JsonObject("errors" -> Json.arr(Json.fromJsonObject(error.asGraphQL)))
  }
  final case class Preparation(error: NonEmptyChain[gql.PreparedQuery.PositionalError]) extends CompilationError {
    lazy val asGraphQL: JsonObject = JsonObject("errors" -> Json.arr(error.map(x => Json.fromJsonObject(x.asGraphQL)).toList: _*))
  }
}

sealed trait Application[F[_]] {
  def mapK[G[_]](f: F ~> G): Application[G]

  def modify(f: QueryResult => QueryResult)(implicit F: Functor[F]): Application[F]
}
object Application {
  final case class Query[F[_]](run: F[QueryResult]) extends Application[F] {
    override def mapK[G[_]](f: F ~> G): Query[G] = Query(f(run))

    override def modify(f: QueryResult => QueryResult)(implicit F: Functor[F]): Query[F] =
      Query(run.map(f))
  }
  final case class Mutation[F[_]](run: F[QueryResult]) extends Application[F] {
    override def mapK[G[_]](f: F ~> G): Mutation[G] = Mutation(f(run))

    override def modify(f: QueryResult => QueryResult)(implicit F: Functor[F]): Mutation[F] =
      Mutation(run.map(f))
  }
  final case class Subscription[F[_]](run: fs2.Stream[F, QueryResult]) extends Application[F] {
    override def mapK[G[_]](f: F ~> G): Subscription[G] = Subscription(run.translate(f))

    override def modify(f: QueryResult => QueryResult)(implicit F: Functor[F]): Subscription[F] =
      Subscription(run.map(f))
  }
}

final case class CompilerParameters(
    query: String,
    variables: Option[Map[String, Json]],
    operationName: Option[String]
)

object Compiler {
  type Outcome[F[_]] = Either[CompilationError, Application[F]]

  final class PartiallyAppliedCompiler[F[_]](val F: Async[F]) extends AnyVal {
    implicit def F0: Async[F] = F

    def compile[Q, M, S](
        schema: Schema[F, Q, M, S],
        query: String,
        variables: Map[String, Json] = Map.empty,
        operationName: Option[String] = None,
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit,
        debug: DebugPrinter[F] = DebugPrinter.noop[F],
        accumulate: Option[FiniteDuration] = Some(5.millis)
    ) = compileWith(
      schema,
      CompilerParameters(query, Some(variables), operationName),
      queryInput,
      mutationInput,
      subscriptionInput,
      debug,
      accumulate
    )

    def compileWith[Q, M, S](
        schema: Schema[F, Q, M, S],
        cp: CompilerParameters,
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit,
        debug: DebugPrinter[F] = DebugPrinter.noop[F],
        accumulate: Option[FiniteDuration] = Some(5.millis)
    ): Outcome[F] =
      parsePrep(schema, cp)
        .map(compilePrepared(schema, _, queryInput, mutationInput, subscriptionInput, debug, accumulate))

    def parsePrep[Q, M, S](
        schema: Schema[F, Q, M, S],
        cp: CompilerParameters
    ): Either[CompilationError, PreparedQuery.PrepResult[F, Q, M, S]] =
      gql.parser.parse(cp.query) match {
        case Left(pe) => Left(CompilationError.Parse(pe))
        case Right(q) =>
          PreparedQuery.prepare(q, schema.shape, cp.variables.getOrElse(Map.empty), cp.operationName) match {
            case Left(pe) => Left(CompilationError.Preparation(pe))
            case Right(x) => Right(x)
          }
      }

    def compilePrepared[Q, M, S](
        schema: Schema[F, Q, M, S],
        ps: PreparedQuery.PrepResult[F, Q, M, S],
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit,
        debug: DebugPrinter[F] = DebugPrinter.noop[F],
        accumulate: Option[FiniteDuration] = Some(5.millis)
    ): Application[F] = {
      implicit val s = schema.statistics
      implicit val p = schema.planner

      ps match {
        case PreparedQuery.PrepResult.Query(ps) =>
          Application.Query {
            queryInput.flatMap { qi =>
              Interpreter.runSync(qi, ps, schema.state, debug).map { case (e, d) => QueryResult(e, d) }
            }
          }
        case PreparedQuery.PrepResult.Mutation(ps) =>
          Application.Mutation {
            mutationInput.flatMap { mi =>
              Interpreter.runSync(mi, ps, schema.state, debug).map { case (e, d) => QueryResult(e, d) }
            }
          }
        case PreparedQuery.PrepResult.Subscription(ps) =>
          Application.Subscription {
            fs2.Stream.eval(subscriptionInput).flatMap { si =>
              Interpreter.runStreamed(si, ps, schema.state, debug, accumulate).map { case (e, d) =>
                QueryResult(e, d)
              }
            }
          }
      }
    }
  }

  def apply[F[_]](implicit F: Async[F]): PartiallyAppliedCompiler[F] = new PartiallyAppliedCompiler[F](F)
}
