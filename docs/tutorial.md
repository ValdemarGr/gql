---
title: Tutorial
---

For this showcase, Star Wars will be our domain of choice.

We'll go through setting up a GraphQL server that represents our Star Wars datatypes by:
1. Defining idiomatic scala types for the Star Wars domain.
2. Introducing GraphQL.
3. Defining a gql schema for our datatypes.
## Setup
We'll have to introduce some dependencies first.
For this tutorial, we'll be pulling everything needed to construct a server.
```scala
libraryDependencies ++= Seq(
  "io.github.valdemargr" %% "gql-server" % "@VERSION@",
)
```

We'll define the domain types for characters and episodes as follows.
```scala mdoc
sealed trait Episode

object Episode {
  case object NewHope extends Episode
  case object Empire extends Episode
  case object Jedi extends Episode
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
```

Now that we have defined our domain, we can define our Star Wars algebra.
```scala mdoc
trait Repository[F[_]] {
  def getHero(episode: Option[Episode]): F[Character]

  def getCharacter(id: String): F[Option[Character]]

  def getHuman(id: String): F[Option[Human]]

  def getDroid(id: String): F[Option[Droid]]
}
```
The respository is abstract and as such, we can implement it in any way we want.

These types will be the foundation of our GraphQL implementation.

To construct the schema later on, we need some imports.
```scala mdoc
import gql._
import gql.dsl.all._
import gql.ast._
import cats.effect._
import cats.implicits._
```

## GraphQL
GraphQL comes in two parts:
- A schema that is provided by a server which acts as a type system for the data.
- A query language that is used to query the data.

Consider the following schema:
```graphql
# 'enum' defines an enumeration
enum Episode {
  NEWHOPE
  EMPIRE
  JEDI
}

# 'type' defined a record type
type Human {
  id: String! # ! means non-nullable, which is the default in gql

  name: String # name is nullable, represented as `Option` in gql

  friends: [Human!]! # [T] is a list of T
  # notice that you must specify nullability for the list and its elements

  appearsIn: [Episode!]! # Lists can contain any types, including enums

  homePlanet: String
}

# the Query type must be present in the schema
# it is the entry point for queries
type Query {
  humans: [Human!]!
}
```
The above defined schema looks like some of our Scala types, and we will in fact be defining our own gql schema later on in a way that syntatically resembles graphql schemas.

Notice that the `friends` field on the `Human` type is a list of `Human`s, that is, it is a recursive type.
In graphql recursive types are allowed.

Now that we have defined our type system, we would like to query it also.
The [GraphQL specification](https://spec.graphql.org/draft/) states that the schema should expose a `Query` type.
We'll query all human names by constructing the following query:
```graphql
# we specify that we'd like to query the api
query {
  # the query type defines the 'humans' field
  # which returns a list of humans
  humans {
    # we perform a "selection" on humans
    # which tells the schema that we would like to select some fields
    # for every human in the list

    # we select the 'name' field
    name
  }
}
```

GraphQL also features interfaces, unions and a variant of pattern matching.
We'll take a look at these features later on, but for now let's try to implement the above schema.

### gql schema
We'll start by defining the enum `Episode` and `Human` type.
```scala mdoc:silent
// We let gql know how to use the scala type `Episode` in our schema
// by associating the `Episode` cases with a string representation
implicit val episode: Enum[Episode] = enumType[Episode](
  // Name as first parameter
  "Episode",
  // The rest of the parameters are the enum values
  "NEWHOPE" -> enumVal(Episode.NewHope),
  "EMPIRE" -> enumVal(Episode.Empire),
  "JEDI" -> enumVal(Episode.Jedi)
)

// Notice how episode is also an implicit (given in scala 3)
// such that the `Human` type can use it wihtout having to reference it directly

implicit lazy val human: Type[IO, Human] = tpe[IO, Human](
  // The typename that will appear in our schema
  "Human",
  // Now we can start defining the fields of our type.
  // Fields in gql are more or less regular functions with some extra information attached to them
  // Here the "id" field is defined as a function `Human => Option[String]` which is lifted into a field
  "id" -> lift(_.id),
  "name" -> lift(_.name),

  // We'll leave friends empty for now :-)
  "friends" -> lift(_ => List.empty[Human]),
  
  // Notice how we implicitly use the `episode` type here
  // If we were to remove the implicit `episode` from scope
  // we would get a "missing implicit" compiler error
  "appearsIn" -> lift(_.appearsIn),

  "homePlanet" -> lift(_.homePlanet)
)
```
Now let's take a look at the schema.
```scala mdoc
SchemaShape.unit[IO](
  fields(
    // We'll also leave the 'humans' field empty for now
    "humans" -> lift(_ => List.empty[Human])
  )
).render
```

Very cool! We have defined our first gql schema.
Now let's try to add the rest of the types into a neat package.

## Schema
Let's define a schema for our whole Star Wars API:
```scala mdoc
// We define a schema as a class since we want some dependencies.
final class StarWarsSchema[F[_]](repo: Repository[F])(implicit F: Async[F]) {
  implicit lazy val episode: Enum[Episode] = enumType[Episode](
    "Episode",
    "NEWHOPE" -> enumVal(Episode.NewHope),
    "EMPIRE" -> enumVal(Episode.Empire),
    "JEDI" -> enumVal(Episode.Jedi)
  )

  // We can define our Character interface from the shared field definitions
  implicit lazy val character: Interface[F, Character] = interface[F, Character](
    "Character",
    "id" -> lift(_.id),
    "name" -> lift(_.name),
    "friends" -> eff(_.friends.traverse(repo.getCharacter)),
    "appearsIn" -> lift(_.appearsIn),
    "secretBackstory" -> 
      build[F, Character](_.emap(_ => "secretBackstory is secret.".leftIor[String]))
  )

  // Human has the character fields along with its own unique "homePlanet" field
  implicit lazy val human: Type[F, Human] = tpe[F, Human](
    "Human",
    "homePlanet" -> lift(_.homePlanet)
  ).subtypeImpl[Character]

  // Droid has the character fields along with its own unique "primaryFunction" field
  implicit lazy val droid: Type[F, Droid] = tpe[F, Droid](
    "Droid",
    "primaryFunction" -> lift(_.primaryFunction)
  ).subtypeImpl[Character]

  // Arguments types can be defined as values as well
  val episodeArg = arg[Option[Episode]]("episode")
  val idArg = arg[String]("id")

  val makeSchema = Schema.query(
    tpe[F, Unit](
      "Query",
      // There are various ways to define fields depending on how unorthodox a usecase is
      // `build` is a more explicit way of defining a field
      "hero" -> build.from(arged(episodeArg).evalMap(repo.getHero)),
      "human" -> build.from(arged(idArg).evalMap(repo.getHuman)),
      "droid" -> eff(idArg){ case (id, _) => repo.getDroid(id) }
    )
  )
}
```

Lets construct a simple in-memory repository for our schema:
```scala mdoc:silent
val luke = Human(
  "1000",
  "Luke Skywalker".some,
  "1002" :: "1003" :: "2000" :: "2001" :: Nil,
  Episode.NewHope :: Episode.Empire :: Episode.Jedi :: Nil,
  "Tatooine".some
)

val vader = Human(
  "1001",
  "Darth Vader".some,
  "1004" :: Nil,
  Episode.NewHope :: Episode.Empire :: Episode.Jedi :: Nil,
  "Tatooine".some
)

val han = Human(
  "1002",
  "Han Solo".some,
  "1000" :: "1003" :: "2001" :: Nil,
  Episode.NewHope :: Episode.Empire :: Episode.Jedi :: Nil,
  None
)

val leia = Human(
  "1003",
  "Leia Organa".some,
  "1000" :: "1002" :: "2000" :: "2001" :: Nil,
  Episode.NewHope :: Episode.Empire :: Episode.Jedi :: Nil,
  "Alderaan".some
)

val tarkin = Human(
  "1004",
  "Wilhuff Tarkin".some,
  "1001" :: Nil,
  Episode.NewHope :: Nil,
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
  Episode.NewHope :: Episode.Empire :: Episode.Jedi :: Nil,
  "Protocol"
)

val artoo = Droid(
  "2001",
  "R2-D2".some,
  "1000" :: "1002" :: "1003" :: Nil,
  Episode.NewHope :: Episode.Empire :: Episode.Jedi :: Nil,
  "Astromech"
)

val droidData =
  List(threepio, artoo)
    .map(x => x.id -> x)
    .toMap

def repo: Repository[IO] = new Repository[IO] {
  def getHero(episode: Option[Episode]): IO[Character] =
    if (episode.contains(Episode.Empire)) IO(luke)
    else IO(artoo)
    
  def getCharacter(id: String): IO[Option[Character]] =
    IO(humanData.get(id) orElse droidData.get(id))
    
  def getHuman(id: String): IO[Option[Human]] =
    IO(humanData.get(id))
    
  def getDroid(id: String): IO[Option[Droid]] =
    IO(droidData.get(id))
}
```

The following GraphQL query will be used to test our schema:
```graphql
query {
  # 1
  hero(episode: NEWHOPE) {
    id
    name

    # 2
    __typename

    # 3
    ... on Droid {
      primaryFunction
      friends {
        name
        __typename
        appearsIn
      }
    }
    ... HumanDetails
  }
  c3po: droid(id: "2000") {
    name
  }
}

# 4
fragment HumanDetails on Human {
  homePlanet
}
```
Some new things are going on in this query:
1. `(episode: NEWHOPE)` is used to pass arguments to the `hero` field.
2. The `__typename` field is used to get the type of the object returned.
This field is available on all types and interfaces.
3. The `... on ` syntax is used to pattern match on specific types.
Since the `hero` returns a `Character` interface we must match it to a `Droid` to get the `primaryFunction` field.
4. The `fragment` syntax is used to define a reusable block of fields akin to a CTE in SQL.

Now let us introduce the query in scala:
```scala mdoc
def query = """
  query {
    hero(episode: NEWHOPE) {
      id
      name
      __typename
      ... on Droid {
        primaryFunction
        friends {
          name
          __typename
          appearsIn
        }
      }
      ... HumanDetails
    }
    c3po: droid(id: "2000") {
      name
    }
  }

  fragment HumanDetails on Human {
    homePlanet
  }
"""
```

Finally we can parse, plan and evaluate the query:

```scala mdoc:passthrough
import io.circe.syntax._
mdoc.IORunPrint(
(new StarWarsSchema[IO](repo))
  .makeSchema
  .map(Compiler[IO].compile(_, query))
  .flatMap { case Right(Application.Query(run)) => run.map(_.asJson) }
)("""
import io.circe.syntax._
(new StarWarsSchema[IO](repo))
  .makeSchema
  .map(Compiler[IO].compile(_, query))
  .flatMap { case Right(Application.Query(run)) => run.map(_.asJson) }
""")
```
And that's the end of this tutorial!
The docs contain more examples and information about the library, so be sure to check them out.