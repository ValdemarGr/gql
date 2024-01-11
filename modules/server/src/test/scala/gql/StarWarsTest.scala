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

import cats.effect._
import munit.CatsEffectSuite
import io.circe._
import io.circe.syntax._
import munit.Location
import io.circe.literal._

// https://github.com/graphql/graphql-js/blob/main/src/__tests__/starWarsData.ts
class StarWarsTest extends CatsEffectSuite {
  lazy val schema = StarWarsSchema.schema.unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asJsonObject)
      case Right(Application.Query(q)) => q.map(_.asJsonObject)
      case _                           => ???
    }

  def assertJsonIO(actual: IO[JsonObject])(expected: Json)(implicit loc: Location): IO[Unit] =
    actual.map { jo =>
      import io.circe.syntax._
      assertEquals(jo.asJson, expected)
    }

  test("the schema should be valid") {
    assert(clue(schema.validate).isEmpty)
  }

  test("hero name query") {
    val q = """
      query HeroNameQuery {
        hero {
          name
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
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

  test("id and friends of R2D2") {
    val q = """
      query HeroNameQuery {
        hero {
          id
          name
          friends {
            name
          }
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "hero": {
            "id": "2001",
            "name": "R2-D2",
            "friends": [
              {
                "name": "Luke Skywalker"
              },
              {
                "name": "Han Solo"
              },
              {
                "name": "Leia Organa"
              }
            ]
          }
        }
      }
      """
    }
  }

  test("nested queries") {
    val q = """
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

    assertJsonIO(query(q)) {
      json"""
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
  }

  test("using ids and query parameters to refetch objects") {
    val q = """
      query FetchLukeAndC3POQuery {
        luke: human(id: "1000") {
          name
        }
        c3po: droid(id: "2000") {
          name
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "luke": {
            "name": "Luke Skywalker"
          },
          "c3po": {
            "name": "C-3PO"
          }
        }
      }
      """
    }
  }

  test("using ids and query parameters to refetch objects with aliases") {
    val q = """
      query FetchLukeAndC3POQuery {
        luke: human(id: "1000") {
          name
        }
        c3po: droid(id: "2000") {
          name
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "luke": {
            "name": "Luke Skywalker"
          },
          "c3po": {
            "name": "C-3PO"
          }
        }
      }
      """
    }
  }

  val genericQuery = """
      query FetchSomeIDQuery($someId: String!) {
        human(id: $someId) {
          name
        }
      }
    """

  test("allows us to create a generic query, then use it to fetch skywalker using his id") {
    assertJsonIO(query(genericQuery, Map("someId" -> Json.fromString("1000")))) {
      json"""
      {
        "data": {
          "human": {
            "name": "Luke Skywalker"
          }
        }
      }
      """
    }
  }

  test("allows us to create a generic qurey, then use it to fetch han solo using his id") {
    assertJsonIO(query(genericQuery, Map("someId" -> Json.fromString("1002")))) {
      json"""
      {
        "data": {
          "human": {
            "name": "Han Solo"
          }
        }
      }
      """
    }
  }

  test("allows us to create a generic query, then pass an invalid id to get null back") {
    assertJsonIO(query(genericQuery, Map("someId" -> Json.fromString("not a valid id")))) {
      json"""
      {
        "data": {
          "human": null
        }
      }
      """
    }
  }

  test("allows us to create a generic query, then pass no id to get an error") {
    assertJsonIO(query(genericQuery, Map.empty)) {
      json"""
      {
        "errors": [
          {
            "message": "Variable '$$someId' is required but was not provided.",
            "locations": [
              {
                "line": 1,
                "column": 29
              }
            ]
          }
        ]
      }
      """
    }
  }

  test("allows us to query using duplicated content") {
    val q = """
      query DuplicateFields {
        luke: human(id: "1000") {
          name
          homePlanet
        }
        leia: human(id: "1003") {
          name
          homePlanet
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "luke": {
            "name": "Luke Skywalker",
            "homePlanet": "Tatooine"
          },
          "leia": {
            "name": "Leia Organa",
            "homePlanet": "Alderaan"
          }
        }
      }
      """
    }
  }

  test("allows us to use a fragment to avoid duplicating content") {
    val q = """
      query UseFragment {
        luke: human(id: "1000") {
          ...HumanFragment
        }
        leia: human(id: "1003") {
          ...HumanFragment
        }
      }

      fragment HumanFragment on Human {
        name
        homePlanet
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "luke": {
            "name": "Luke Skywalker",
            "homePlanet": "Tatooine"
          },
          "leia": {
            "name": "Leia Organa",
            "homePlanet": "Alderaan"
          }
        }
      }
      """
    }
  }

  test("allows us to verify that R2-D2 is a droid") {
    val q = """
      query CheckTypeOfR2 {
        hero {
          __typename
          name
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "hero": {
            "__typename": "Droid",
            "name": "R2-D2"
          }
        }
      }
      """
    }
  }

  test("allows us to verify that luke is a human") {
    val q = """
      query CheckTypeOfLuke {
        hero(episode: EMPIRE) {
          __typename
          name
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "hero": {
            "__typename": "Human",
            "name": "Luke Skywalker"
          }
        }
      }
      """
    }
  }

  test("correctly reports error on accessing secretBackstory of droid") {
    val q = """
      query HeroNameQuery {
        hero {
          name
          secretBackstory
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "errors": [
          {
            "message": "secretBackstory is secret.",
            "path": [
              "hero",
              "secretBackstory"
            ]
          }
        ],
        "data": {
          "hero": {
            "secretBackstory": null,
            "name": "R2-D2"
          }
        }
      }
      """
    }
  }

  test("correctly reports error on accessing secretBackstory in a list of droids") {
    val q = """
      query HeroNameQuery {
        hero {
          name
          friends {
            name
            secretBackstory
          }
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "errors": [
          {
            "message": "secretBackstory is secret.",
            "path": [
              "hero",
              "friends",
              0,
              "secretBackstory"
            ]
          },
          {
            "message": "secretBackstory is secret.",
            "path": [
              "hero",
              "friends",
              1,
              "secretBackstory"
            ]
          },
          {
            "message" : "secretBackstory is secret.",
            "path" : [
              "hero",
              "friends",
              2,
              "secretBackstory"
            ]
          }
        ],
        "data": {
          "hero": {
            "friends": [
              {
                "secretBackstory" : null,
                "name" : "Luke Skywalker"
              },
              {
                "secretBackstory" : null,
                "name" : "Han Solo"
              },
              {
                "secretBackstory" : null,
                "name" : "Leia Organa"
              }
            ],
            "name": "R2-D2"
          }
        }
      }
      """
    }
  }

  test("correctly reports error on accessing through an alias") {
    val q = """
      query HeroNameQuery {
        mainHero: hero {
          name
          story: secretBackstory
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "errors": [
          {
            "message": "secretBackstory is secret.",
            "path": [
              "mainHero",
              "story"
            ]
          }
        ],
        "data": {
          "mainHero": {
            "story": null,
            "name": "R2-D2"
          }
        }
      }
      """
    }
  }

  test("can provide different numeric types") {
    query("""
      query {
        numeric(one: 1, two: 2.0)
      }
      """) >> query("""
      query {
        numeric(one: 1, two: 2)
      }
      """) >>
      query("""
      query {
        numeric(one: 1, two: 2.1)
      }
      """)
  }

  test("should be able to produce multiple errors") {
    val q = """
      query DeadQuery {
        cors
        mainHero: hero {
          name
          story: secretBackstory
          horse
          dorse
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "errors": [
  {
    "message" : "Field 'dorse' is not a member of `Character`.",
    "locations" : [
      {
        "line" : 7,
        "column" : 10
      }
    ],
    "path" : [
      "hero"
    ]
  },
  {
    "message" : "Field 'horse' is not a member of `Character`.",
    "locations" : [
      {
        "line" : 6,
        "column" : 10
      }
    ],
    "path" : [
      "hero"
    ]
  },
  {
    "message" : "Field 'cors' is not a member of `Query`.",
    "locations" : [
      {
        "line" : 2,
        "column" : 8
      }
    ]
  }
        ]
      }
      """
    }
  }

  test("should be able to select multiple disjunctive nested fragments, and they should be merged") {
    val q = """
      fragment FriendAppearsIn on Character {
        friends {
          appearsIn
        }
      }

      fragment FriendNames on Character {
        friends {
          name
        }
        ...FriendAppearsIn
      }

      fragment FriendNames2 on Character {
        friends {
          name2: name
        }
      }

      fragment FriendIds on Character {
        friends {
          id
        }
        ...FriendNames2
      }

      query FullQuery {
        hero {
          ...FriendNames
          ...FriendIds
        }
      }
    """

    assertJsonIO(query(q)) {
      json"""
      {
        "data": {
          "hero": {
            "friends": [
              {
                "name": "Luke Skywalker",
                "id": "1000",
                "name2": "Luke Skywalker",
                "appearsIn": [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name": "Han Solo",
                "id": "1002",
                "name2": "Han Solo",
                "appearsIn": [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              },
              {
                "name": "Leia Organa",
                "id": "1003",
                "name2": "Leia Organa",
                "appearsIn": [
                  "NEWHOPE",
                  "EMPIRE",
                  "JEDI"
                ]
              }
            ]
          }
        }
      }
      """
    }
  }
}
