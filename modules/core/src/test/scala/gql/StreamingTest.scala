package gql

import io.circe._
import fs2.Stream
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl._
import cats.effect._
import cats.implicits._
import fs2.Pull

final case class Level1(value: Int)
final case class Level2(value: Int)

class StreamingTest extends CatsEffectSuite {
  val level1UsersRef = IO.ref(0).unsafeRunSync()
  def level1Users = level1UsersRef.get.unsafeRunSync()
  val level1Resource = Resource.make(level1UsersRef.update(_ + 1))(_ => level1UsersRef.update(_ - 1))

  val level2UsersRef = IO.ref(0).unsafeRunSync()
  def level2Users = level2UsersRef.get.unsafeRunSync()
  val level2Resource = Resource.make(level2UsersRef.update(_ + 1))(_ => level2UsersRef.update(_ - 1))

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

  lazy val schemaShape = SchemaShape[IO, Unit, Unit, Unit](
    tpe[IO, Unit](
      "Query",
      "level1" -> pure(_ => Level1(0))
    ),
    subscription = Some(
      tpe[IO, Unit](
        "Subscription",
        "level1" -> pure(_ => Level1(0))
      )
    )
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): Stream[IO, JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                          => Stream(err.asGraphQL)
      case Right(Application.Subscription(s)) => s.map(_.asGraphQL)
      case _                                  => ???
    }

  implicit class PathAssertionSyntax(j: Json) {
    def field(f: String): Json = {
      assert(clue(j).isObject, "should be object")
      val o = j.asObject.get
      assert(clue(o(f)).isDefined, s"should have field $f")
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
      .take(3)
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

  test("should stream out some nested elements") {
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
        val l2 = Json.fromJsonObject(jo).field("data").field("level1").field("level2").field("value").int
        val l1 = Json.fromJsonObject(jo).field("data").field("level1").field("level2").field("level1").field("value").int
        (l2, l1)
      }
      .zipWithNext
      .collect { case ((xl2, xl1), Some((yl2, yl1))) =>
        // either inner re-emitted
        val innerReemit = (xl2 == yl2) && (xl1 < yl1)
        // or outer re-emitted and inner was restarted
        val outerReemit = (xl2 < yl2) && (xl1 == 0)
        assert(innerReemit || outerReemit)
      }
      .compile
      .drain
  }

  test("nesting with fragments works") {
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
      .compile
      .drain
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
        case None => ???
        case Some((hd, _)) =>
          Pull.eval {
            IO {
              // There should be one lease on both resources
              assert(clue(level1Users) >= 1)
              assert(clue(level2Users) >= 1)
            }
          }
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