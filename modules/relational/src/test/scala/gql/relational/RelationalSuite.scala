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
package gql.relational

import cats.effect._
import cats.data._
import cats.implicits._
import gql.Schema
import gql.dsl.all._
import gql.ast
import munit.CatsEffectSuite
import gql.Application

abstract class RelationalSuiteTables[QA <: QueryAlgebra](val algebra: QA) extends CatsEffectSuite {
  val dsl = new QueryDsl[QA](algebra) {}
  import dsl.algebra._
  import dsl._

  def intDecoder: Decoder[Int]

  def textDecoder: Decoder[String]

  def encodeText(str: String): Frag

  sealed trait Episode extends Product with Serializable
  object Episode {
    case object NewHope extends Episode
    case object Empire extends Episode
    case object Jedi extends Episode

    implicit lazy val out: ast.Enum[Episode] = enumType(
      "Episode",
      "NEW_HOPE" -> enumVal(Episode.NewHope),
      "EMPIRE" -> enumVal(Episode.Empire),
      "JEDI" -> enumVal(Episode.Jedi)
    )

    def fromString(str: String): Episode = str match {
      case "new_hope" => Episode.NewHope
      case "empire"   => Episode.Empire
      case "jedi"     => Episode.Jedi
      case _          => throw new Exception(s"got unexpected string $str")
    }

    def asString(ep: Episode): String = ep match {
      case Episode.NewHope => "new_hope"
      case Episode.Empire  => "empire"
      case Episode.Jedi    => "jedi"
    }
  }

  trait SuiteTable extends Table {
    def sel[A](col: String, dec: Decoder[A]): (Frag, Query.Select[A]) = {
      val c = aliasedFrag(stringToFrag(col))
      (c, Query.Select(Chain(c), dec))
    }
  }

  case class AppearsInTable(alias: String) extends SuiteTable {
    def table = stringToFrag(s"appears_in")

    val (characterIdCol, characterId) = sel[String]("character_id", textDecoder)
    val (episodeIdCol, episodeId) = sel[Episode]("episode_id", textDecoder.fmap(Episode.fromString))

    def tableKey = (characterId, episodeId).tupled
  }

  case class FriendTable(alias: String) extends SuiteTable {
    def table = stringToFrag(s"friend")

    val (characterId1Col, characterId1) = sel[String]("character_id_1", textDecoder)
    val (characterId2Col, characterId2) = sel[String]("character_id_2", textDecoder)

    def tableKey = (characterId1, characterId2).tupled
  }

  case class UnknownCharacter(id: Frag)
  trait CharacterTable extends SuiteTable {
    val (idCol, id) = sel[String]("id", textDecoder)
    val (nameCol, name) = sel[Option[String]]("name", optDecoder(textDecoder))

    def appearsIn: Query[List, Query.Select[Episode]] =
      dsl
        .table(AppearsInTable)
        .join[List](a => a.characterIdCol |+| stringToFrag(" = ") |+| idCol)
        .map(_.episodeId)

    def friends: Query[List, UnknownCharacter] =
      dsl
        .table(FriendTable)
        .join[List](_.characterId1Col |+| stringToFrag(" = ") |+| idCol)
        .map(x => UnknownCharacter(x.characterId2Col))

    def tableKey: Query.Select[String] = id
  }

  case class HumanTable(alias: String) extends CharacterTable {
    def table = stringToFrag("human")
    val (homePlanetCol, homePlanet) = sel[Option[String]]("home_planet", optDecoder(textDecoder))
  }
  val humanTable = table(HumanTable)

  case class DroidTable(alias: String) extends CharacterTable {
    def table = stringToFrag("droid")
    val (primaryFunctionCol, primaryFunction) = sel[Option[String]]("primary_function", optDecoder(textDecoder))
  }
  val droidTable = table(DroidTable)

  case class HeroTable(alias: String) extends SuiteTable {
    def table = stringToFrag("hero")
    val (episodeCol, episode) = sel[Episode]("episode", textDecoder.fmap(Episode.fromString))
    val (characterIdCol, characterId) = sel[String]("character_id", textDecoder)
    def tableKey = episode
  }
  val heroTable = table(HeroTable)

  def schema[F[_]: Concurrent: Queryable](conn: Connection[F]): F[Schema[F, Unit, Unit, Unit]] = {
    implicit lazy val unknownCharacter: ast.Interface[F, QueryContext[UnknownCharacter]] =
      interface[F, QueryContext[UnknownCharacter]](
        "Character",
        "id" -> abst[F, String],
        "name" -> abst[F, Option[Episode]],
        "friends" -> abst[F, List[QueryContext[UnknownCharacter]]],
        "appearsIn" -> abst[F, List[Episode]]
      )

    implicit lazy val human: ast.Type[F, QueryContext[HumanTable]] =
      tpe[F, QueryContext[HumanTable]](
        "Human",
        "id" -> query(_.id),
        "name" -> query(_.name),
        "friends" -> cont(_.friends),
        "appearsIn" -> query(_.appearsIn),
        "homePlanet" -> query(_.homePlanet)
      ).contImplements[UnknownCharacter] { u =>
        humanTable.join[Option](_.idCol |+| stringToFrag(" = ") |+| u.id)
      }

    implicit lazy val droid: ast.Type[F, QueryContext[DroidTable]] =
      tpe[F, QueryContext[DroidTable]](
        "Droid",
        "id" -> query(_.id),
        "name" -> query(_.name),
        "friends" -> cont(_.friends),
        "appearsIn" -> query(_.appearsIn),
        "primaryFunction" -> query(_.primaryFunction)
      ).contImplements[UnknownCharacter] { u =>
        droidTable.join[Option](_.idCol |+| stringToFrag(" = ") |+| u.id)
      }

    Schema.query(
      tpe[F, Unit](
        "Query",
        "hero" -> runFieldSingle(conn, arg[Episode]("episode")) { (_, ep) =>
          heroTable
            .join(_.episodeCol |+| stringToFrag(" = ") |+| encodeText(Episode.asString(ep)))
            .map(t => UnknownCharacter(t.characterIdCol))
        },
        "human" -> runFieldSingle(conn, arg[String]("id")) { (_, id) =>
          humanTable.join(_.idCol |+| stringToFrag(" = ") |+| encodeText(id))
        },
        "droid" -> runFieldSingle(conn, arg[String]("id")) { (_, id) =>
          droidTable.join(_.idCol |+| stringToFrag(" = ") |+| encodeText(id))
        }
      )
    )
  }

  def tests(conn: => Connection[IO])(implicit Q: Queryable[IO]) = {
    test("run query") {
      schema[IO](conn)
        .map { schema =>
          gql
            .Compiler[IO]
            .compile(
              schema,
              """
        query {
          hero(episode: NEW_HOPE) {
            name
          }
          human(id: "1000") {
            name
            appearsIn
            friends {
              name
            }
            ... on Character {
              charTypename: __typename
              ... on Droid {
                doidTypename: __typename
                primaryFunction
              }
            }
          }
          droid(id: "2001") {
            name
            appearsIn
            friends {
              appearsIn
              name
            }
            ... on Character {
              charTypename: __typename
              ... on Human {
                humanTypename: __typename
                homePlanet
              }
            }
          }
        }
        """
            )
        }
        .flatMap {
          case Right(Application.Query(fa)) =>
            fa.map(_.handleErrors(_.getMessage())).map { qr =>
              assert(clue(qr.errors).isEmpty)
              assertEquals(qr.data("hero").get.asObject.get("name").get.asString.get, "R2-D2")
              assertEquals(qr.data("human").get.asObject.get("name").get.asString.get, "Luke Skywalker")
              assertEquals(
                qr.data("human").get.asObject.get("friends").get.asArray.get.map(_.asObject.get("name").get.asString.get).toSet,
                Set("Han Solo", "Leia Organa", "C-3PO", "R2-D2")
              )
            }
          case _ => ???
        }
    }
  }

  val ddlQueries = List(
    "drop table if exists human",
    "drop table if exists droid",
    "drop table if exists appears_in",
    "drop table if exists friend",
    "drop table if exists hero",
    """
        create table if not exists human (
            id text not null primary key,
            name text,
            home_planet text
        )
    """,
    """
        create table if not exists droid (
            id text not null primary key,
            name text,
            primary_function text not null
        )
    """,
    """
        create table if not exists appears_in (
            character_id text not null,
            episode_id text not null,
            primary key (character_id, episode_id)
        )
    """,
    """
        create table if not exists friend (
            character_id_1 text not null,
            character_id_2 text not null,
            primary key (character_id_1, character_id_2)
        )
    """,
    """
        create table if not exists hero (
            episode text not null primary key,
            character_id text not null
        )
    """
  )

  val dataQueries = List(
    """
        insert into human (id, name, home_planet) values
            ('1000', 'Luke Skywalker', 'Tatooine'),
            ('1001', 'Darth Vader', 'Tatooine'),
            ('1002', 'Han Solo', null),
            ('1003', 'Leia Organa', 'Alderaan'),
            ('1004', 'Wilhuff Tarkin', null)
    """,
    """
        insert into droid (id, name, primary_function) values
            ('2000', 'C-3PO', 'Protocol'),
            ('2001', 'R2-D2', 'Astromech')
    """,
    """
        insert into appears_in (character_id, episode_id) values
            ('1000', 'new_hope'),
            ('1000', 'empire'),
            ('1000', 'jedi'),
            ('1001', 'new_hope'),
            ('1001', 'empire'),
            ('1001', 'jedi'),
            ('1002', 'new_hope'),
            ('1002', 'empire'),
            ('1002', 'jedi'),
            ('1003', 'new_hope'),
            ('1003', 'empire'),
            ('1003', 'jedi'),
            ('1004', 'new_hope'),
            ('2000', 'new_hope'),
            ('2000', 'empire'),
            ('2000', 'jedi'),
            ('2001', 'new_hope'),
            ('2001', 'empire'),
            ('2001', 'jedi')
    """,
    """
        insert into friend (character_id_1, character_id_2) values
            ('1000', '1002'),
            ('1000', '1003'),
            ('1000', '2000'),
            ('1000', '2001'),
            ('1001', '1004'),
            ('1002', '1000'),
            ('1002', '1003'),
            ('1002', '2001'),
            ('1003', '1000'),
            ('1003', '1002'),
            ('1003', '2000'),
            ('1003', '2001'),
            ('1004', '1001'),
            ('2000', '1000'),
            ('2000', '1002'),
            ('2000', '1003'),
            ('2000', '2001'),
            ('2001', '1000'),
            ('2001', '1002'),
            ('2001', '1003')
    """,
    """
        insert into hero (episode, character_id) values
            ('new_hope', '2001'),
            ('empire', '1000'),
            ('jedi', '2001')
    """
  )
}
