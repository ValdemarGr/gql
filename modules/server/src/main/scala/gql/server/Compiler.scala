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
import gql.server.interpreter.{QueryInterpreter, DebugPrinter}
import cats.data._
import scala.concurrent.duration._
import io.circe.syntax._
import gql.preparation._
import cats.parse.Caret
import gql.server.planner.Planner

sealed trait CompilationError
object CompilationError {
  final case class Parse(error: gql.parser.ParseError) extends CompilationError
  object Parse {
    implicit val encoder: Encoder.AsObject[Parse] = Encoder.AsObject.instance[Parse] { err =>
      Map("errors" -> List(err.error)).asJsonObject
    }
  }
  final case class Preparation(error: NonEmptyChain[PositionalError[Caret]]) extends CompilationError
  object Preparation {
    implicit val encoder: Encoder.AsObject[Preparation] = Encoder.AsObject.instance[Preparation] { err =>
      Map("errors" -> err.error).asJsonObject
    }
  }

  implicit val encoder: Encoder.AsObject[CompilationError] = Encoder.AsObject.instance[CompilationError] {
    case err: Parse       => err.asJsonObject
    case err: Preparation => err.asJsonObject
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
    ): Outcome[F] = compileWith(
      schema,
      QueryParameters(query, Some(variables), operationName),
      queryInput,
      mutationInput,
      subscriptionInput,
      debug,
      accumulate
    )

    def compileWith[Q, M, S](
        schema: Schema[F, Q, M, S],
        cp: QueryParameters,
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
        cp: QueryParameters
    ): Either[CompilationError, PreparedRoot[F, Q, M, S]] =
      gql.parser.parseQuery(cp.query) match {
        case Left(pe) => Left(CompilationError.Parse(pe))
        case Right(q) =>
          RootPreparation.prepareRun(q, schema.shape, cp.variables.getOrElse(Map.empty), cp.operationName) match {
            case Left(pe) => Left(CompilationError.Preparation(pe))
            case Right(x) => Right(x)
          }
      }

    def compilePrepared[Q, M, S](
        schema: Schema[F, Q, M, S],
        ps: PreparedRoot[F, Q, M, S],
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit,
        debug: DebugPrinter[F] = DebugPrinter.noop[F],
        accumulate: Option[FiniteDuration] = Some(5.millis)
    ): Application[F] = {
      implicit val s = schema.statistics
      implicit val p = schema.planner

      compileWithInterpreter(
        ps,
        new QueryInterpreter.DefaultImpl[F](schema.state, debug, accumulate),
        queryInput,
        mutationInput,
        subscriptionInput
      )
    }

    def compileWithInterpreter[Q, M, S](
        ps: PreparedRoot[F, Q, M, S],
        interpreter: QueryInterpreter[F],
        queryInput: F[Q] = F.unit,
        mutationInput: F[M] = F.unit,
        subscriptionInput: F[S] = F.unit
    )(implicit planner: Planner[F]): Application[F] =
      ps match {
        case PreparedRoot.Query(ps) =>
          Application.Query {
            queryInput.flatMap { qi =>
              interpreter.runSync(qi, ps).map { case (e, d) => QueryResult(d, e.flatMap(_.asResult)) }
            }
          }
        case PreparedRoot.Mutation(ps) =>
          Application.Mutation {
            mutationInput.flatMap { mi =>
              interpreter.runSync(mi, ps).map { case (e, d) => QueryResult(d, e.flatMap(_.asResult)) }
            }
          }
        case PreparedRoot.Subscription(ps) =>
          Application.Subscription {
            fs2.Stream.eval(subscriptionInput).flatMap { si =>
              interpreter.runStreamed(si, ps).map { case (e, d) =>
                QueryResult(d, e.flatMap(_.asResult))
              }
            }
          }
      }
  }

  def apply[F[_]](implicit F: Async[F]): PartiallyAppliedCompiler[F] = new PartiallyAppliedCompiler[F](F)
}
