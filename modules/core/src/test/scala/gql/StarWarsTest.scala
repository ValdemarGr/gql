package gql

import cats.implicits._
import cats.effect._
import cats.data._
import gql.dsl._
import gql.ast._
import munit.CatsEffectSuite
import io.circe._

// https://github.com/graphql/graphql-js/blob/main/src/__tests__/starWarsData.ts
class StarWarsTest extends CatsEffectSuite {
  import StarWarsTest._

  val luke = Human(
    "1000",
    "Luke Skywalker".some,
    "1002" :: "1003" :: "2000" :: "2001" :: Nil,
    Episode.NEWHOPE :: Episode.EMPIRE :: Episode.JEDI :: Nil,
    "Tatooine".some
  )

  val vader = Human(
    "1001",
    "Darth Vader".some,
    "1004" :: Nil,
    Episode.NEWHOPE :: Episode.EMPIRE :: Episode.JEDI :: Nil,
    "Tatooine".some
  )

  val han = Human(
    "1002",
    "Han Solo".some,
    "1000" :: "1003" :: "2001" :: Nil,
    Episode.NEWHOPE :: Episode.EMPIRE :: Episode.JEDI :: Nil,
    None
  )

  val leia = Human(
    "1003",
    "Leia Organa".some,
    "1000" :: "1002" :: "2000" :: "2001" :: Nil,
    Episode.NEWHOPE :: Episode.EMPIRE :: Episode.JEDI :: Nil,
    "Alderaan".some
  )

  val tarkin = Human(
    "1004",
    "Wilhuff Tarkin".some,
    "1001" :: Nil,
    Episode.NEWHOPE :: Nil,
    None
  )

  val humanData =
    List(luke, vader, han, leia, tarkin)
      .map(x => x.id -> x)
      .toMap

  val threepio = Droid(
    "2000",
    "C-3PO".some,
    "1000" :: "1002" :: "1003" :: "2001" :: Nil,
    Episode.NEWHOPE :: Episode.EMPIRE :: Episode.JEDI :: Nil,
    "Protocol"
  )

  val artoo = Droid(
    "2001",
    "R2-D2".some,
    "1000" :: "1002" :: "1003" :: Nil,
    Episode.NEWHOPE :: Episode.EMPIRE :: Episode.JEDI :: Nil,
    "Astromech"
  )

  val droidData =
    List(threepio, artoo)
      .map(x => x.id -> x)
      .toMap

  def getCharacter(id: String): IO[Option[Character]] =
    IO(humanData.get(id) orElse droidData.get(id))

  def getFriends(character: Character): IO[List[Character]] =
    character.friends.flatTraverse(getCharacter(_).map(_.toList))

  def getHero(episode: Option[Episode]): IO[Character] =
    if (episode.contains(Episode.EMPIRE)) IO(luke)
    else IO(artoo)

  def getHuman(id: String): IO[Option[Human]] =
    IO(humanData.get(id))

  def getDroid(id: String): IO[Option[Droid]] =
    IO(droidData.get(id))

  lazy val schemaShape = {
    implicit lazy val episode: Enum[IO, Episode] =
      enum[IO, Episode](
        "Episode",
        enumInst("NEWHOPE", Episode.NEWHOPE),
        enumInst("EMPIRE", Episode.EMPIRE),
        enumInst("JEDI", Episode.JEDI)
      )

    implicit lazy val character: Interface[IO, Character] =
      interface[IO, Character](
        "Character",
        "id" -> pure(_.id),
        "name" -> pure(_.name),
        "friends" -> eff(getFriends),
        "appearsIn" -> pure(_.appearsIn),
        "secretBackstory" -> fallible(_ => IO("secretBackstory is secret.".leftIor[String]))
      )(
        instance[Human] { case h: Human => h },
        instance[Droid] { case d: Droid => d }
      )

    implicit lazy val human: Type[IO, Human] =
      tpe[IO, Human](
        "Human",
        "homePlanet" -> pure(_.homePlanet),
        character.fields.toList: _*
      )

    implicit lazy val droid: Type[IO, Droid] =
      tpe[IO, Droid](
        "Droid",
        "primaryFunction" -> pure(_.primaryFunction),
        character.fields.toList: _*
      )

    SchemaShape[IO, Unit, Unit, Unit](
      Some(
        tpe[IO, Unit](
          "Query",
          "hero" -> eff(arg[Option[Episode]]("episode")) { case (_, ep) => getHero(ep) },
          "human" -> eff(arg[String]("id")) { case (_, id) => getHuman(id) },
          "droid" -> eff(arg[String]("id")) { case (_, id) => getDroid(id) }
        )
      )
    )
  }

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asGraphQL)
      case Right(Application.Query(q)) => q.map(_.asGraphQL)
      case _                           => ???
    }

  def assertJsonIO(actual: IO[JsonObject])(expected: String): IO[Unit] =
    actual.map { jo =>
      val p = io.circe.parser.parse(expected)
      assert(clue(p).isRight)
      import io.circe.syntax._
      assertEquals(jo.asJson, p.toOption.get)
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
      """
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
      """
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
      """
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
      """
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
      """
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
      """
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
      """
      {
        "errors": [
          {
            "message": "Variable '$someId' is required but was not provided.",
            "locations": [
              {
                "line": 1,
                "column": 45
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
      """
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
      """
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
      """
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
      """
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
      """
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
      """
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
      """
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
}

object StarWarsTest {
  sealed trait Episode

  object Episode {
    case object NEWHOPE extends Episode
    case object EMPIRE extends Episode
    case object JEDI extends Episode
  }

  trait Character {
    def id: String
    def name: Option[String]
    def friends: List[String]
    def appearsIn: List[Episode]
  }

  final case class Human(
      id: String,
      name: Option[String],
      friends: List[String],
      appearsIn: List[Episode],
      homePlanet: Option[String]
  ) extends Character

  final case class Droid(
      id: String,
      name: Option[String],
      friends: List[String],
      appearsIn: List[Episode],
      primaryFunction: String
  ) extends Character
}
