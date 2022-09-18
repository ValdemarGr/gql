---
title: Example
---

```scala mdoc
import gql._
import gql.ast._
import gql.dsl._

sealed trait Episode
object Episode {
  case object NewHope extends Episode
  case object Empire extends Episode
  case object Jedi extends Episode

  implicit lazy val gqlType = 
    enum(
      "Episode"
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
  implicit def gqlType[F[_]]: Interface[F, Character] =
    interface(
      "Character",
      "id" -> pure(_.id),
      "name" -> pure(_.name),
      "friends" -> pure(_.friends),
      "appearsIn" -> pure(_.appearsIn)
    )(
      instance[Human]{ case x: Human => x },
      instance[Droid]{ case x: Droid => x}
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
  implicit def gqlType[F[_]]: Type[F, Human] =
    obj(
      "Human",
      "homePlanet" -> pure(_.homePlanet),
      Character.gqlType[F].fields: _*
    )
}

final case class Droid(
  id: String,
  name: Option[String],
  friends: Option[List[Character]],
  appearsIn: Option[List[Episode]],
  primaryFunction: Option[String]
) extends Character
object Human {
  implicit def gqlType[F[_]]: Type[F, Droid] =
    obj(
      "Droid",
      "primaryFunction" -> pure(_.primaryFunction),
      Character.gqlType[F].fields: _*
    )
}

trait Repository[F[_]] {
  def getHero(episode: Episode): F[Character] = ???

  def getCharacter(id: String): F[Character] = ???

  def getHuman(id: String): F[Human] = ???

  def getDroid(id: String): F[Droid] = ???
}

def schema[F[_]](implicit repo: Repository[F]) =  SchemaShape[F, Unit](
  obj(
    "Query",
    "hero"      -> effect(arg[Episode]("episode")){ case (_, episode) => repo.getHero(episode) },
    "character" -> effect(arg[ID[String]]("id")){ case (_, id) => repo.getCharacter(id) },
    "human"     -> effect(arg[ID[String]]("id")){ case (_, id) => repo.getHuman(id) },
    "droid"     -> effect(arg[ID[String]]("id")){ case (_, id) => repo.getDroid(id) }
  )
)
```
