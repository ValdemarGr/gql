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

import cats.data._
import gql._
import cats.implicits._
import io.circe.syntax._

trait EvalFailure {
  def paths: Chain[Cursor]

  def error: Either[Throwable, String]

  def handleErrorWith(pf: PartialFunction[Throwable, String]): EvalFailure =
    error match {
      case Left(pf(e)) => EvalFailure.Basic(paths, Right(e))
      case _           => this
    }

  def asResult: Chain[QueryResult.Error] =
    paths.map { path =>
      val filteredPath = path.path.mapFilter {
        case GraphArc.Field(name) => Some(name.asJson)
        case GraphArc.Index(idx)  => Some(idx.asJson)
      }
      QueryResult.Error(error, filteredPath)
    }
}
object EvalFailure {
  final case class Basic(
      paths: Chain[Cursor],
      error: Either[Throwable, String]
  ) extends EvalFailure
  final case class StreamHeadResolution(
      path: Cursor,
      error: Either[Throwable, String]
  ) extends EvalFailure {
    lazy val paths = Chain(path)
  }
  final case class StreamTailResolution(
      path: Cursor,
      error: Either[Throwable, String]
  ) extends EvalFailure {
    lazy val paths = Chain(path)
  }
  final case class BatchResolution(
      paths: Chain[Cursor],
      ex: Throwable
  ) extends EvalFailure {
    lazy val exception = Some(ex)
    lazy val error = Left(ex)
  }
  final case class EffectResolution(
      path: Cursor,
      error: Either[Throwable, String]
  ) extends EvalFailure {
    lazy val paths = Chain(path)
  }
  final case class Raised(
      path: Cursor,
      raisedError: String
  ) extends EvalFailure {
    lazy val paths = Chain(path)
    lazy val error = Right(raisedError)
  }
}
