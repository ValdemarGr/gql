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
import io.circe._
import io.circe.syntax._
import fs2.Stream
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl.all._
import cats.effect._
import scala.concurrent.duration._
import gql.resolver.Resolver

class StreamBatchTest extends CatsEffectSuite {
  lazy val schemaShape =
    (
      Resolver.batch[IO, Int, Int](xs => IO(xs.map(x => (x, x)).toMap)),
      Resolver.batch[IO, String, String](xs => IO(xs.map(x => (x, x)).toMap))
    ).mapN { case (intB, stringB) =>
      SchemaShape.unit[IO](
        fields(
          "bla" -> lift(_ => true)
        ),
        subscription = fields[IO, Unit](
          "test" -> build[IO, Unit].from {
            Resolver
              .id[IO, Unit]
              // .streamMap(_ => fs2.Stream(1, 2, 3, 4).lift[IO].meteredStartImmediately(100.millis))
              .streamMap { _ =>
                fs2.Stream(List(1), List(1, 2)).lift[IO].meteredStartImmediately(500.millis)
              }
              .andThen(intB.all[List])
              .map(_.flatten.map(_.toString))
              .streamMap(xs => fs2.Stream(xs).lift[IO].repeat.meteredStartImmediately(200.millis))
              .andThen(stringB.all[List])
              .map(_.flatten)
          }
        ).some
      )
    }

  lazy val schema = Schema.stateful(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): Stream[IO, JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                          => Stream(err.asJsonObject)
      case Right(Application.Subscription(s)) => s.map(_.asJsonObject)
      case _                                  => ???
    }

  test("the schema should be queryable") {
    query(
      """
        subscription {
          test
        }
      """
    ).compile.drain
  }
}
