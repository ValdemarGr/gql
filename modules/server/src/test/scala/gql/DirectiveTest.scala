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

import cats.effect._
import munit.CatsEffectSuite
import io.circe._
import io.circe.syntax._
import munit.Location

class DirectiveTest extends CatsEffectSuite {
  lazy val schema = StarWarsSchema.schema.unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asJsonObject)
      case Right(Application.Query(q)) => q.map(_.asJsonObject)
      case _                           => ???
    }

  def assertJsonIO(actual: IO[JsonObject])(expected: String)(implicit loc: Location): IO[Unit] =
    actual.map { jo =>
      val p = io.circe.parser.parse(expected)
      assert(clue(p).isRight)
      import io.circe.syntax._
      assertEquals(jo.asJson, p.toOption.get)
    }

  test("skipping a field should leave it out") {
    val q = """
      query HeroNameQuery {
        hero {
          name @skip(if: false)
          id @skip(if: true)
        }
      }
    """

    assertJsonIO(query(q)) {
      """
      {
        "data": {
          "hero": {
            "name": "R2-D2"
          }
        }
      }
      """
    }
  }

  test("including a field with false should also leave it out") {
    val q = """
      query HeroNameQuery {
        hero {
          name @include(if: false)
          id @include(if: false)
        }
      }
    """

    assertJsonIO(query(q)) {
      """
      {
        "data": {
          "hero": {}
        }
      }
      """
    }
  }
}
