---
title: Example
---

```scala mdoc
import cats._
import gql._
import gql.ast._
import gql.dsl._

sealed trait Episode
object Episode {
  case object NewHope extends Episode
  case object Empire extends Episode
  case object Jedi extends Episode

  implicit def gqlType[F[_]]: Enum[F, Episode] =
    enum(
      "Episode",
      "NEWHOPE" -> NewHope,
      "EMPIRE" -> Empire,
      "JEDI" -> Jedi
    )
}

trait Character {
  def id: String
  def name: Option[String]
  def friends: Option[List[Character]]
  def appearsIn: Option[List[Episode]]
}
object Character {
  implicit def gqlType[F[_]: Applicative]: Interface[F, Character] =
    interface[F, Character](
      "Character",
      "id" -> pure(_.id),
      "name" -> pure(_.name),
      "friends" -> pure(_.friends),
      "appearsIn" -> pure(_.appearsIn)
    )(
      instance[Human] { case x: Human => x },
      instance[Droid] { case x: Droid => x }
    )
}

final case class Human(
    id: String,
    name: Option[String],
    friends: Option[List[Character]],
    appearsIn: Option[List[Episode]],
    homePlanet: Option[String]
) extends Character
object Human {
  implicit def gqlType[F[_]: Applicative]: Type[F, Human] =
    tpe(
      "Human",
      "homePlanet" -> pure(_.homePlanet),
      Character.gqlType[F].fields.toList: _*
    )
}

final case class Droid(
    id: String,
    name: Option[String],
    friends: Option[List[Character]],
    appearsIn: Option[List[Episode]],
    primaryFunction: Option[String]
) extends Character
object Droid {
  implicit def gqlType[F[_]: Applicative]: Type[F, Droid] =
    tpe(
      "Droid",
      "primaryFunction" -> pure(_.primaryFunction),
      Character.gqlType[F].fields.toList: _*
    )
}

trait Repository[F[_]] {
  def getHero(episode: Episode): F[Character]

  def getCharacter(id: String): F[Character]

  def getHuman(id: String): F[Human]

  def getDroid(id: String): F[Droid]
}

def schema[F[_]: Applicative](implicit repo: Repository[F]) = {
  SchemaShape[F, Unit](
    tpe(
      "Query",
      "hero" -> eff(arg[Episode]("episode")) { case (_, episode) => repo.getHero(episode) },
      "character" -> eff(arg[ID[String]]("id")) { case (_, id) => repo.getCharacter(id.value) },
      "human" -> eff(arg[ID[String]]("id")) { case (_, id) => repo.getHuman(id.value) },
      "droid" -> eff(arg[ID[String]]("id")) { case (_, id) => repo.getDroid(id.value) }
    )
  )
}

import cats.effect._
implicit def repo = new Repository[IO] {
  def getHero(episode: Episode): IO[Character] = ???
  def getCharacter(id: String): IO[Character] = ???
  def getHuman(id: String): IO[Human] = ???
  def getDroid(id: String): IO[Droid] = ???
}
schema[IO]
```
