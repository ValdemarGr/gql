---
title: Example
---

For this showcase, Star Wars will be our domain of choice:
```scala mdoc
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
```scala mdoc
trait Repository[F[_]] {
  def getHero(episode: Option[Episode]): F[Character]

  def getCharacter(id: String): F[Option[Character]]

  def getHuman(id: String): F[Option[Human]]

  def getDroid(id: String): F[Option[Droid]]
}
```

To construct the schema, we need some imports.
```scala mdoc
import gql._
import gql.dsl._
import gql.ast._
import cats.effect._
import cats.implicits._
```

Now we can define our schema:
```scala mdoc
def schema[F[_]](implicit repo: Repository[F], F: Async[F]) = {
  implicit lazy val episode: Enum[Episode] = enumType[Episode](
    "Episode",
    "NEWHOPE" -> enumVal(Episode.NEWHOPE),
    "EMPIRE" -> enumVal(Episode.EMPIRE),
    "JEDI" -> enumVal(Episode.JEDI)
  )

  lazy val characterFields = fields[F, Character](
    "id" -> lift(_.id),
    "name" -> lift(_.name),
    "friends" -> eff(_.friends.traverse(repo.getCharacter)),
    "appearsIn" -> lift(_.appearsIn),
    "secretBackstory" -> build[F, Character](_.as("secretBackstory is secret.".leftIor[String]).rethrow)
  )

  implicit lazy val character: Interface[F, Character] = interfaceFromNel[F, Character](
    "Character",
    characterFields
  )

  implicit lazy val human: Type[F, Human] = tpe[F, Human](
    "Human",
    "homePlanet" -> lift(_.homePlanet)
  ).subtypeOf[Character].addFields(characterFields.toList: _*)

  implicit lazy val droid: Type[F, Droid] = tpe[F, Droid](
    "Droid",
    "primaryFunction" -> lift(_.primaryFunction)
  ).subtypeOf[Character].addFields(characterFields.toList: _*)

  val episodeArg = arg[Option[Episode]]("episode")
  val idArg = arg[String]("id")

  Schema.query(
    tpe[F, Unit](
      "Query",
      "hero" -> build.from(arged(episodeArg).evalMap(repo.getHero)),
      "human" -> build.from(arged(idArg).evalMap(repo.getHuman)),
      "droid" -> eff(idArg){ case (id, _) => 
        repo.getDroid(id) 
      }
    )
  )
}
```

Lets construct a simple in-memory repository:
```scala mdoc:silent
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
```scala mdoc
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

```scala mdoc:passthrough
mdoc.IORunPrint(
schema[IO]
  .map(Compiler[IO].compile(_, query))
  .flatMap { case Right(Application.Query(run)) => run.map(_.asGraphQL) }
)("""
schema[IO]
  .map(Compiler[IO].compile(_, query))
  .flatMap { case Right(Application.Query(run)) => run.map(_.asGraphQL) }
""")
```