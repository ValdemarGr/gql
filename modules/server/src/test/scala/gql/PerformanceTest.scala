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

import cats.implicits._
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl._
import cats.effect._

class PerformanceTest extends CatsEffectSuite {
  case object Data
  implicit lazy val data = builder[IO, Data.type] { b =>
    b.tpe(
      "Data",
      "value" -> b(_.streamMap(_ => (fs2.Stream(1) ++ fs2.Stream(2)))),
      "value2" -> b(_.evalMap(_ => IO(1)))
    )
  }

  lazy val schemaShape = SchemaShape.unit[IO](
    fields("ping" -> lift(_ => "pong")),
    subscription = Some(
      fields(
        "data" -> lift(_ => (0 to 500).toList.as(Data))
      )
    )
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  test("warm up") {
    (0 to 10).toList.traverse_ { _ =>
      Compiler[IO]
        .compile(
          schema,
          """
          subscription {
            data {
              value
              value2
            }
          }
        """
        )
        .toOption
        .get match {
        case Application.Subscription(run) => run.head.compile.drain
        case _                             => ???
      }
    }
  }

  test("performance for effect") {
    Compiler[IO]
      .compile(
        schema,
        """
        subscription {
          data {
            value2
          }
        }
      """
      )
      .toOption
      .get match {
      case Application.Subscription(run) => run.head.compile.drain
      case _                             => ???
    }
  }

  test("performance for stream") {
    Compiler[IO]
      .compile(
        schema,
        """
        subscription {
          data {
            value
          }
        }
      """
      )
      .toOption
      .get match {
      case Application.Subscription(run) => run.head.compile.drain
      case _                             => ???
    }
  }
}
