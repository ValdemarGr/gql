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

import cats.implicits._
import io.circe._
import io.circe.syntax._
import fs2.Stream
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl.all._
import cats.effect._
import gql.resolver.Resolver

class BatchWithErrorsTest extends CatsEffectSuite {
  case class Level1(value: Int)
  case class Level2(value: Int)

  lazy val schemaShape = Resolver.batch[IO, Int, Int](is => IO.pure(is.map(i => (i, i)).toMap)).map { r =>
    implicit lazy val level2: Type[IO, Level2] = builder[IO, Level2] { b =>
      b.tpe(
        "Level2",
        "data" -> b(
          _.map(_.value)
            .andThen(r.opt)
        )
      )
    }

    implicit lazy val level1: Type[IO, Level1] = builder[IO, Level1] { b =>
      b.tpe(
        "Level1",
        "next" -> b(
          _.map(_.value)
            .emap[Level2] {
              case 1 => Level2(1).rightIor
              case n => s"Oh no, an error! $n".leftIor
            }
        )
      )
    }

    SchemaShape[IO, Unit, Unit, Unit](
      tpe[IO, Unit](
        "Query",
        "level1" -> lift(_ => Level1(0))
      ),
      subscription = Some(
        tpe[IO, Unit](
          "Subscription",
          "level1s" -> lift(_ => List(Level1(1), Level1(2)))
        )
      )
    )
  }

  lazy val schema = Schema.stateful(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): Stream[IO, JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                          => Stream(err.asJsonObject)
      case Right(Application.Subscription(s)) => s.map(_.asJsonObject)
      case _                                  => ???
    }

  implicit class PathAssertionSyntax(j: Json) {
    def field(f: String): Json = {
      assert(clue(j).isObject, "should be object")
      val o = j.asObject.get
      assert(clue(o(f)).isDefined, s"should have field $f in $j")
      o(f).get
    }

    def number = {
      assert(clue(j).isNumber, "should be number")
      j.asNumber.get
    }

    def int: Int = {
      val x = number.toInt
      assert(clue(x).isDefined, "should be int")
      x.get
    }

    def isNull(): Unit = {
      assert(clue(j).isNull, "should be null")
    }
  }

  def assertJsonStream(actual: Stream[IO, JsonObject])(expected: String*): IO[Unit] =
    actual.take(expected.size.toLong).compile.toList.map { xs =>
      val e = expected.toList

      assert {
        clue(xs)
        clue(e)
        xs.size == e.size
      }

      (xs zip e).foreach { case (x, e0) =>
        val p = io.circe.parser.parse(e0)
        assert(clue(p).isRight)
        import io.circe.syntax._
        assertEquals(x.asJson, p.toOption.get)
      }
    }

  test("the schema should be valid") {
    assert(clue(schema.validate).isEmpty)
  }

  test("should get some of the data") {
    val q = """
      subscription {
        level1s {
          next {
            data
          }
        }
     }
    """

    query(q)
      .take(1)
      .map { jo =>
        val xs = Json.fromJsonObject(jo).field("data").field("level1s").asArray.get
        xs(0).field("next").field("data").int
        xs(1).field("next").isNull()
      }
      .compile
      .drain
  }
}
