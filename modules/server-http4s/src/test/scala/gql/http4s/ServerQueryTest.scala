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
package gql.http4s

import org.http4s.implicits._
import io.circe._
import munit.CatsEffectSuite
import gql._
import cats.effect._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.client.dsl.io._
import org.http4s.circe._

class ServerQueryTest extends CatsEffectSuite {
  lazy val swSchema = StarWarsSchema.schema.unsafeRunSync()

  lazy val http4sRoutes =
    gql.http4s.Http4sRoutes.syncSimple[IO](cp => IO(Right(Compiler[IO].compileWith(swSchema, cp))))

  lazy val client = Client.fromHttpApp(http4sRoutes.orNotFound)

  def forceParse(s: String): Json = {
    val x = io.circe.parser.parse(s)
    assert(clue(x).isRight)
    x.toOption.get
  }

  test("should be able to fire some queries off against the http4s routes") {
    val body = Json.obj(
      "query" -> Json.fromString {
        """
          query NestedQuery {
            hero {
              name
              friends {
                name
                appearsIn
                friends {
                  name
                }
              }
            }
          }
      """
      }
    )

    client
      .expect[Json](POST(body, uri"https://api.acme.org/graphql"))
      .map { b =>
        assertEquals(
          b,
          forceParse {
            """
        {
          "data": {
            "hero": {
              "name": "R2-D2",
              "friends": [
                {
                  "name": "Luke Skywalker",
                  "appearsIn": [
                    "NEWHOPE",
                    "EMPIRE",
                    "JEDI"
                  ],
                  "friends": [
                    {
                      "name": "Han Solo"
                    },
                    {
                      "name": "Leia Organa"
                    },
                    {
                      "name": "C-3PO"
                    },
                    {
                      "name": "R2-D2"
                    }
                  ]
                },
                {
                  "name": "Han Solo",
                  "appearsIn": [
                    "NEWHOPE",
                    "EMPIRE",
                    "JEDI"
                  ],
                  "friends": [
                    {
                      "name": "Luke Skywalker"
                    },
                    {
                      "name": "Leia Organa"
                    },
                    {
                      "name": "R2-D2"
                    }
                  ]
                },
                {
                  "name": "Leia Organa",
                  "appearsIn": [
                    "NEWHOPE",
                    "EMPIRE",
                    "JEDI"
                  ],
                  "friends": [
                    {
                      "name": "Luke Skywalker"
                    },
                    {
                      "name": "Han Solo"
                    },
                    {
                      "name": "C-3PO"
                    },
                    {
                      "name": "R2-D2"
                    }
                  ]
                }
              ]
            }
          }
        }
        """
          }
        )
      }
  }
}
