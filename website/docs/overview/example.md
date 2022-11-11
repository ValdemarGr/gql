---
title: Example
---

# Example
gql is a GraphQL implementation.
gql comes with many of the standard features of a GraphQL implementation such as a dsl, parser and interpreter.
But also some unique features such as, herustic query planning and signals.
The most important goals of gql is to be simple, predictable and composable.

For this showcase, Star Wars will be our domain of choice:
```scala
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
```

Lets define where our data comes from:
```scala
trait Repository[F[_]] {
  def getHero(episode: Option[Episode]): F[Character]

  def getCharacter(id: String): F[Option[Character]]

  def getHuman(id: String): F[Option[Human]]

  def getDroid(id: String): F[Option[Droid]]
}
```

To construct the schema, we need some imports.
```scala
import gql._
import gql.dsl._
import gql.ast._
import cats.effect._
import cats.implicits._
```

Now we can define our schema:
```scala
def schema[F[_]](implicit repo: Repository[F], F: Async[F]) = {
  implicit lazy val episode: Enum[F, Episode] = enumType[F, Episode](
    "Episode",
    "NEWHOPE" -> enumVal(Episode.NEWHOPE),
    "EMPIRE" -> enumVal(Episode.EMPIRE),
    "JEDI" -> enumVal(Episode.JEDI)
  )

  implicit lazy val character: Interface[F, Character] = interface[F, Character](
    "Character",
    "id" -> pure(_.id),
    "name" -> pure(_.name),
    "friends" -> eff(_.friends.traverse(repo.getCharacter)),
    "appearsIn" -> pure(_.appearsIn),
    "secretBackstory" -> fallible(_ => F.pure("secretBackstory is secret.".leftIor[String]))
  )

  implicit lazy val human: Type[F, Human] = tpe[F, Human](
    "Human",
    "homePlanet" -> pure(_.homePlanet)
  ).subtypeOf[Character].addFields(character.fields.toList: _*)

  implicit lazy val droid: Type[F, Droid] = tpe[F, Droid](
    "Droid",
    "primaryFunction" -> pure(_.primaryFunction)
  ).subtypeOf[Character] .addFields(character.fields.toList: _*)

  Schema.query(
    tpe[F, Unit](
      "Query",
      "hero" -> eff(arg[Option[Episode]]("episode")) { case (_, ep) => repo.getHero(ep) },
      "human" -> eff(arg[String]("id")) { case (_, id) => repo.getHuman(id) },
      "droid" -> eff(arg[String]("id")) { case (_, id) => repo.getDroid(id) }
    )
  )
}
```

Lets construct a simple in-memory repository:
```scala
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

implicit def repo: Repository[IO] = new Repository[IO] {
  def getHero(episode: Option[Episode]): IO[Character] =
    if (episode.contains(Episode.EMPIRE)) IO(luke)
    else IO(artoo)
    
  def getCharacter(id: String): IO[Option[Character]] =
    IO(humanData.get(id) orElse droidData.get(id))
    
  def getHuman(id: String): IO[Option[Human]] =
    IO(humanData.get(id))
    
  def getDroid(id: String): IO[Option[Droid]] =
    IO(droidData.get(id))
}
```

Lets construct a query:
```scala
def query = """
 query ExampleQuery {
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

Now we can parse, plan and evaluate the query:

```scala
      schema[IO]
        .map(Compiler[IO].compile(_, query))
        .flatMap { case Right(Application.Query(run)) => run.map(_.asGraphQL) }
      
// object[data -> {
//   "c3po" : {
//     "name" : "C-3PO"
//   },
//   "hero" : {
//     "friends" : [
//       {
//         "appearsIn" : [
//           "NEWHOPE",
//           "EMPIRE",
//           "JEDI"
//         ],
//         "__typename" : "Human",
//         "name" : "Luke Skywalker"
//       },
//       {
//         "appearsIn" : [
//           "NEWHOPE",
//           "EMPIRE",
//           "JEDI"
//         ],
//         "__typename" : "Human",
//         "name" : "Han Solo"
//       },
//       {
//         "appearsIn" : [
//           "NEWHOPE",
//           "EMPIRE",
//           "JEDI"
//         ],
//         "__typename" : "Human",
//         "name" : "Leia Organa"
//       }
//     ],
//     "primaryFunction" : "Astromech",
//     "__typename" : "Droid",
//     "name" : "R2-D2",
//     "id" : "2001"
//   }
// }]
```

<!-- <details> 
<summary>Output</summary>

lol

</details> --->
