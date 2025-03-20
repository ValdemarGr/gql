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
import fs2.{Stream, Pull}
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl.all._
import cats.effect._
import scala.concurrent.duration._

final case class Level1(value: Int)
final case class Level2(value: Int)

class StreamingTest extends CatsEffectSuite {
  val level1UsersRef = IO.ref(0).unsafeRunSync()
  def level1Users = level1UsersRef.get.unsafeRunSync()
  val level1Resource = Resource.make(level1UsersRef.update(_ + 1))(_ => level1UsersRef.update(_ - 1))

  val level2UsersRef = IO.ref(0).unsafeRunSync()
  def level2Users = level2UsersRef.get.unsafeRunSync()
  val level2Resource = Resource.make(level2UsersRef.update(_ + 1))(_ => level2UsersRef.update(_ - 1))

  implicit lazy val level1: Type[IO, Level1] = builder[IO, Level1] { b =>
    tpe[IO, Level1](
      "Level1",
      "value" -> lift(_.value),
      "level2" -> b {
        _.streamMap(_ =>
          Stream
            .iterate(0)(_ + 1)
            .lift[IO]
            .meteredStartImmediately(500.millis)
            .flatMap(x => fs2.Stream.resource(level1Resource) as Level2(x))
        )
      }
    )
  }

  implicit lazy val level2: Type[IO, Level2] = builder[IO, Level2] { b =>
    tpe[IO, Level2](
      "Level2",
      "value" -> lift(_.value),
      "level1" -> b {
        _.streamMap(_ =>
          Stream
            .iterate(0)(_ + 1)
            .lift[IO]
            .meteredStartImmediately(1000.millis)
            .flatMap(x => fs2.Stream.resource(level2Resource) as Level1(x))
        )
      }
    )
  }

  lazy val schemaShape = SchemaShape[IO, Unit, Unit, Unit](
    tpe[IO, Unit](
      "Query",
      "level1" -> lift(_ => Level1(0))
    ),
    subscription = Some(
      tpe[IO, Unit](
        "Subscription",
        "level1" -> lift(_ => Level1(0)),
        "throwhd" -> build[IO, Unit](
          _.streamMap(_ => fs2.Stream(1).covary[IO].evalMap(_ => IO.raiseError[Int](new Exception("err"))).repeat)
        ),
        "throwtl" -> build[IO, Unit](
          _.streamMap(_ => fs2.Stream(1) ++ fs2.Stream(1).covary[IO].evalMap(_ => IO.raiseError[Int](new Exception("err"))).repeat)
        )
      )
    )
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

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

  test("should stream out some elements") {
    assertEquals(clue(level1Users), 0)
    assertEquals(clue(level2Users), 0)

    val q = """
      subscription {
        level1 {
          level2 {
            value
          }
        }
     }
    """

    query(q)
      .take(5)
      .map { jo =>
        Json.fromJsonObject(jo).field("data").field("level1").field("level2").field("value").int
      }
      .zipWithNext
      .collect { case (x, Some(y)) =>
        assert(x < y)
      }
      .compile
      .drain >> IO {
      assertEquals(clue(level1Users), 0)
      assertEquals(clue(level2Users), 0)
    }
  }

  test("should be able to throw exceptions in head of stream") {
    val q = """
     subscription {
       throwhd
     }
    """

    query(q)
      .take(1)
      .compile
      .lastOrError
      .map(jo => Json.fromJsonObject(jo).field("data").field("throwhd").isNull)
  }

  test("should be able to throw exceptions in tail of stream") {
    val q = """
     subscription {
       throwtl
     }
    """

    query(q)
      .drop(1)
      .take(1)
      .compile
      .lastOrError
      .map(jo => Json.fromJsonObject(jo).field("data").field("throwtl").isNull)
  }

  test("should stream out some nested elements") {
    assertEquals(clue(level1Users), 0)
    assertEquals(clue(level2Users), 0)
    // Run test some times
    (0 to 100).toList.parTraverse_ { _ =>
      // if inner re-emits, outer will remain the same
      // if outer re-emits, inner will restart
      val q = """
      subscription {
        level1 {
          level2 {
            level1 {
              value
            }
            value
          }
        }
      }
    """

      query(q)
        .take(10)
        .map { jo =>
          Json.fromJsonObject(jo).field("data").field("level1").field("level2").field("value").int
          Json.fromJsonObject(jo).field("data").field("level1").field("level2").field("level1").field("value").int
        }
        .compile
        .drain
    } >> IO {
      assertEquals(clue(level1Users), 0)
      assertEquals(clue(level2Users), 0)
    }
  }

  test("nesting with fragments works") {
    assertEquals(clue(level1Users), 0)
    assertEquals(clue(level2Users), 0)
    val q = """
      subscription {
        level1 {
          ... A
        }
      }

      fragment A on Level1 {
        level2 {
          ... B
        }
        value
      }

      fragment B on Level2 {
        level1 {
          ... C
        }
        value
      }

      fragment C on Level1 {
        level2 {
          ... D
        }
        value
      }

      fragment D on Level2 {
        level1 {
          value
        }
        value
      }
    """

    query(q)
      .take(10)
      .map(Json.fromJsonObject(_).field("data").field("level1"))
      .zipWithIndex
      .compile
      .drain >> IO {
      assertEquals(clue(level1Users), 0)
      assertEquals(clue(level2Users), 0)
    }
  }

  test("resource aquisition should work as expected") {
    assertEquals(clue(level1Users), 0)
    assertEquals(clue(level2Users), 0)

    val q = """
      subscription {
        level1 {
          level2 {
            level1 {
              value
            }
            value
          }
        }
      }
    """

    query(q).pull.uncons1
      .flatMap {
        case None         => ???
        case Some((_, _)) =>
          // There should be one lease on both resources if we await
          // When a new element arrives, that is, tl.head, then a lease on level2 and level1 should be present
          val check = IO {
            assert(clue(level1Users) >= 1)
            assert(clue(level2Users) >= 1)
          }

          val tryComplete =
            Stream
              .repeatEval(check.attempt)
              .meteredStartImmediately(100.millis)
              .find(_.isRight)
              .compile
              .drain

          Pull.eval(IO.race(tryComplete, IO.sleep(5.seconds) >> check)).void
      }
      .stream
      .compile
      .drain >>
      IO {
        assert(clue(level1Users) == 0)
        assert(clue(level2Users) == 0)
      }
  }
}
