package gql

import munit.CatsEffectSuite
import gql.dsl._
import gql.ast._
import cats.effect.IO
import io.circe._
import gql.resolver.Resolver

class SkipTest extends CatsEffectSuite {
  val effectState = IO.ref(Option.empty[Int]).unsafeRunSync()

  lazy val schemaShape = SchemaShape.make[IO](
    builder[IO, Unit] { b =>
      tpe(
        "Query",
        //"num" -> (_.pure(_ => 5)),
        //"num" -> pure(_ => 5),
        //"num" -> field[Unit][IO, Int](Resolver.pure(_ => 5))
        //"num" -> field(Resolver.pure(_ => 5))
        //"num" -> b.pure(_ => 5)
        // "num" -> b.eff(_ => IO(5))
        //"num" -> b(_.map(_ => 5))
        //"num" -> b.from(Resolver.pure(_ => 5))
        "num" -> b(_.map(_ => 5).evalMap(x => IO(x * 2)).evalMap(x => IO(x / 2)))
        // "num" -> b(
        //   _.evalMap{ _ =>
        //     effectState.get.flatMap {
        //       case None    => IO(Left(10))
        //       case Some(i) => IO.pure(Right(i))
        //     }
        //   }.skipThatWith(r => r.evalMap(i => effectState.modify(_ => (Some(i), i))))
        // )
        /* _.evalMap{ _ =>
            effectState.get.flatMap {
              case None    => IO(Left(10))
              case Some(i) => IO.pure(Right(i))
            }
          }.skipThatWith[Unit, Int, Int](res => res.evalMap(i => effectState.modify(_ => (Some(i), i))))
        }*/
      )
    }
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  def query(q: String, variables: Map[String, Json] = Map.empty): IO[JsonObject] =
    Compiler[IO].compile(schema, q, variables = variables) match {
      case Left(err)                   => IO.pure(err.asGraphQL)
      case Right(Application.Query(q)) => q.map(_.asGraphQL)
      case _                           => ???
    }

  def assertJsonIO(actual: IO[JsonObject])(expected: String): IO[Unit] =
    actual.map { jo =>
      val p = io.circe.parser.parse(expected)
      assert(clue(p).isRight)
      import io.circe.syntax._
      assertEquals(jo.asJson, p.toOption.get)
    }

  test("calling once should update the cache") {
    val q = """
      query TestQuery {
        num
      }
    """

    assertJsonIO(query(q)) {
      """
      {
        "data": {
          "num": 20
        }
      }
      """
    } >> effectState.get.map(o => assert(o.isDefined))
  }

  test("calling again should return the same result") {
    val q = """
      query TestQuery {
        num
      }
    """

    assertJsonIO(query(q)) {
      """
      {
        "data": {
          "num": 20
        }
      }
      """
    }
  }
}
