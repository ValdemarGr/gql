/*
 * Copyright 2024 Valdemar Grange
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

import munit.CatsEffectSuite
import gql.ast._
import cats.effect.IO
import io.circe._
import io.circe.syntax._
import gql.dsl.all._

class SkipTest extends CatsEffectSuite {
  val effectState = IO.ref(Option.empty[Int]).unsafeRunSync()

  lazy val schemaShape = SchemaShape.unit[IO](
    builder[IO, Unit] { b =>
      b.fields(
        "num" -> b(
          _.evalMap { _ =>
            effectState.get.flatMap {
              case None    => IO(Left(20))
              case Some(i) => IO.pure(Right(i))
            }
          }.leftThrough(_.evalMap(i => effectState.modify(_ => (Some(i), i)))).map(_.merge)
        )
      )
    }
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asJsonObject)
      case Right(Application.Query(q)) => q.map(_.asJsonObject)
      case _                           => ???
    }

  def assertJsonIO(actual: IO[JsonObject])(expected: String): IO[Unit] =
    actual.map { jo =>
      val p = io.circe.parser.parse(expected)
      assert(clue(p).isRight)
      import io.circe.syntax._
      assertEquals(jo.asJson, p.toOption.get)
    }

  test("calling once should update the cache") {
    val q = """
      query TestQuery {
        num
      }
    """

    assertJsonIO(query(q)) {
      """
      {
        "data": {
          "num": 20
        }
      }
      """
    } >> effectState.get.map(o => assert(o.isDefined))
  }

  test("calling again should return the same result") {
    val q = """
      query TestQuery {
        num
      }
    """

    assertJsonIO(query(q)) {
      """
      {
        "data": {
          "num": 20
        }
      }
      """
    }
  }
}
