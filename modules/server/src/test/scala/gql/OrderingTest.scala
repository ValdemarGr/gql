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
import cats.data.NonEmptyChain
import cats.Apply

class OrderingTest extends CatsEffectSuite {
  // lazy val schemaShape = SchemaShape[IO, Unit, Unit, Unit](
  //   tpe[IO, Unit](
  //     "Query",
  //     "level1" -> lift(_ => Level1(0))
  //   ),
  //   subscription = Some(
  //     tpe[IO, Unit](
  //       "Subscription",
  //       "level1" -> lift(_ => Level1(0)),
  //       "throwhd" -> build[IO, Unit](
  //         _.streamMap(_ => fs2.Stream(1).covary[IO].evalMap(_ => IO.raiseError[Int](new Exception("err"))).repeat)
  //       ),
  //       "throwtl" -> build[IO, Unit](
  //         _.streamMap(_ => fs2.Stream(1) ++ fs2.Stream(1).covary[IO].evalMap(_ => IO.raiseError[Int](new Exception("err"))).repeat)
  //       )
  //     )
  //   )
  // )

  // lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  // def query(q: String, variables: Map[String, Json] = Map.empty): Stream[IO, JsonObject] =
  //   Compiler[IO].compile(schema, q, variables = variables) match {
  //     case Left(err)                          => Stream(err.asJsonObject)
  //     case Right(Application.Subscription(s)) => s.map(_.asJsonObject)
  //     case _                                  => ???
  //   }

  // implicit class PathAssertionSyntax(j: Json) {
  //   def field(f: String): Json = {
  //     assert(clue(j).isObject, "should be object")
  //     val o = j.asObject.get
  //     assert(clue(o(f)).isDefined, s"should have field $f in $j")
  //     o(f).get
  //   }

  //   def number = {
  //     assert(clue(j).isNumber, "should be number")
  //     j.asNumber.get
  //   }

  //   def int: Int = {
  //     val x = number.toInt
  //     assert(clue(x).isDefined, "should be int")
  //     x.get
  //   }

  //   def isNull(): Unit = {
  //     assert(clue(j).isNull, "should be null")
  //   }
  // }

  // def assertJsonStream(actual: Stream[IO, JsonObject])(expected: String*): IO[Unit] =
  //   actual.take(expected.size.toLong).compile.toList.map { xs =>
  //     val e = expected.toList

  //     assert {
  //       clue(xs)
  //       clue(e)
  //       xs.size == e.size
  //     }

  //     (xs zip e).foreach { case (x, e0) =>
  //       val p = io.circe.parser.parse(e0)
  //       assert(clue(p).isRight)
  //       import io.circe.syntax._
  //       assertEquals(x.asJson, p.toOption.get)
  //     }
  //   }

  test("ordering of args should be stable") {
    // final case class L[X]()
    // implicit val applyForL: Apply[L] = ???
    // val li: L[String] = ???
    // applyForL.tuple2(li, li)
    val xs = (
      arg[String]("first"),
      arg[String]("second"),
      arg[String]("third"),
      arg[String]("fourth")
    ).tupled.entries.map(_.name)
    assert(clue(xs) == NonEmptyChain("first", "second", "third", "fourth"))
  }
}
