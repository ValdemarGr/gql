package gql

import io.circe._
import fs2.Stream
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl._
import cats.effect._
import cats.implicits._

class StreamingTest extends CatsEffectSuite {
  lazy val schemaShape = {

    @volatile var level1Users = 0
    val level1Resource = Resource.make(IO { level1Users += 1 })(_ => IO { level1Users -= 1 })
    final case class Level1(value: Int)

    @volatile var level2Users = 0
    val level2Resource = Resource.make(IO { level2Users += 1 })(_ => IO { level2Users -= 1 })
    final case class Level2(value: Int)

    implicit lazy val level1: Type[IO, Level1] =
      tpe[IO, Level1](
        "Level1",
        "value" -> pure(_.value),
        "level2" -> field {
          stream(_ => Stream.iterate(0)(_ + 1).lift[IO].flatMap(x => fs2.Stream.resource(level1Resource) as Level2(x)))
        }
      )

    implicit lazy val level2: Type[IO, Level2] = tpe[IO, Level2](
      "Level2",
      "value" -> pure(_.value),
      "level1" -> field {
        stream(_ => Stream.iterate(0)(_ + 1).lift[IO].flatMap(x => fs2.Stream.resource(level2Resource) as Level1(x)))
      }
    )

    SchemaShape[IO, Unit, Unit, Unit](
      subscription = Some(
        tpe[IO, Unit](
          "Subscription",
          "level1" -> pure(_ => Level1(0))
        )
      )
    )
  }

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): Stream[IO, JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                          => Stream(err.asGraphQL)
      case Right(Application.Subscription(s)) => s.map(_.asGraphQL)
      case _                                  => ???
    }

  def assertJsonStream(actual: Stream[IO, JsonObject])(expected: String*): IO[Unit] =
    actual.take(expected.size).compile.toList.map { xs =>
      val e = expected.toList

      assert {
        clue(xs)
        clue(e)
        xs.size == e.size
      }

      (xs zip e).map { case (x, e0) =>
        val p = io.circe.parser.parse(e0)
        assert(clue(p).isRight)
        import io.circe.syntax._
        assertEquals(x.asJson, p.toOption.get)
      }
    }

  test("should stream out some elements") {
    val q = """
      subscription {
        level1 {
          level2 {
            value
          }
        }
     }
    """

    assertJsonStream(query(q))(
      """
        {
          "data": {
            "level1": {
              "level2": {
                "value": 0
              }
            }
          }
        }
      """,
      """
        {
          "data": {
            "level1": {
              "level2": {
                "value": 1
              }
            }
          }
        }
      """,
      """
        {
          "data": {
            "level1": {
              "level2": {
                "value": 2
              }
            }
          }
        }
      """
    )
  }

  // test("should stream out some nested elements") {
  //   val q = """
  //     subscription {
  //       value
  //       level1 {
  //         level2 {
  //           level1{
  //             value
  //           }
  //         }
  //       }
  //     }
  //   """

  //   query(q).take(1).compile.drain
  // }
}
