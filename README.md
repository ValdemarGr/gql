# gql-test

# Schema
```scala
// ....scala
object syntax {
  implicit def intScalar[F[_]]: ScalarType[F, Int] = ???
  implicit def stringScalar[F[_]]: ScalarType[F, Int] = ???
  implicit def listType[F[_], A](implicit t: OutputType[A]): ListOutputType[F, Int] = ???
  
  def pure[F[_], I, A](resolve: I => A)(implicit tpe: => OutputType[F, A]): Field[F, I, A] = ???
  def effect[F[_], I, A](resolve: I => F[A])(implicit tpe: => OutputType[F, A]): Field[F, I, A] = ???
  def outputObject[F[_], I](
    name: String,
    hd: (String, Field[F, I, _]),
    tl: (String, Field[F, I, _])*
  ): ObjectOutputType[F, I, A] = ???
}

// App.scala
import ....syntax._

final case class Data[F[_]](
  a: Int,
  b: F[Int],
  c: F[List[Data[F]]]
)

object Data {
  implicit def gql[F[_]]: OutputType[Data[F]] = 
    outputObject(
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b),
      "c" -> effect(_.c)
    )
}

def makeData[F[_]]: Data[F] = ???
```

# Getting started
## Intro
### Deps
...

## Example

```scala mdoc
import gql._
import gql.out
import gql.in
import gql.dsl._

sealed trait Episode
object Episode {
  case object NewHope extends Episode
  case object Empire extends Episode
  case object Jedi extends Episode

  implicit lazy val gqlType = 
    enumType(
      NewHope -> "NEWHOPE",
      Empire -> "EMPIRE",
      Jedi -> "JEDI"
    )
}

trait Character {
  def id: String
  def name: Option[String]
  def friends: Option[List[Character]]
  def appearsIn: Option[List[Episode]]
}
object Character {
  implicit def gqlType[F[_]]: out.Interface[F, Character] =
    out.interface(
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
  implicit def gqlType[F[_]]: out.Obj[F, Human] =
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
  implicit def gqlType[F[_]]: out.Obj[F, Droid] =
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
  out.obj(
    "Query",
    "hero"      -> effect(arg[Episode]("episode")){ case (_, episode) => repo.getHero(episode) },
    "character" -> effect(arg[ID[String]]("id")){ case (_, id) => repo.getCharacter(id) },
    "human"     -> effect(arg[ID[String]]("id")){ case (_, id) => repo.getHuman(id) },
    "droid"     -> effect(arg[ID[String]]("id")){ case (_, id) => repo.getDroid(id) }
  )
)
```
TODO show simple example