package gql

import munit.CatsEffectSuite
import gql.ast._
import gql.dsl.all._
import cats.effect._
import cats.implicits._
import gql.resolver.Resolver
import gql.resolver.ShowMissingKeys
import io.circe.JsonObject

class BatchTest extends CatsEffectSuite {
  case class Batchers(
      getString: Resolver[IO, Set[String], Map[String, String]],
      getInt: Resolver[IO, Set[Int], Map[Int, Int]],
      getUnit: Resolver[IO, Set[Unit], Map[Unit, Unit]]
  )

  lazy val intInputs = IO.ref(List.empty[Set[Int]]).unsafeRunSync()
  lazy val stringInputs = IO.ref(List.empty[Set[String]]).unsafeRunSync()
  def resetCounts = intInputs.set(List.empty[Set[Int]]) *> stringInputs.set(List.empty[Set[String]])

  lazy val batchersF = (
    Resolver.batch[IO, String, String](xs => IO(xs.map(x => (x, x)).toMap) <* stringInputs.update(xs :: _)),
    Resolver.batch[IO, Int, Int](xs => IO(xs.map(x => (x, x)).toMap) <* intInputs.update(xs :: _)),
    Resolver.batch[IO, Unit, Unit](xs => IO(xs.map(x => (x, x)).toMap))
  ).mapN(Batchers.apply)

  lazy val schema: Schema[IO, Unit, Unit, Unit] = Schema
    .stateful {
      batchersF.map { bs =>
        implicit val smkString: ShowMissingKeys[String] = ShowMissingKeys.showForKey("string")
        implicit val smkInt: ShowMissingKeys[Int] = ShowMissingKeys.showForKey("int")
        implicit val smkUnit: ShowMissingKeys[Unit] = ShowMissingKeys.showForKey("unit")

        case object OpaqueTpe
        implicit lazy val opaqueTpe: Type[IO, OpaqueTpe.type] = builder[IO, OpaqueTpe.type] { b =>
          b.tpe(
            "Opaque",
            "b1" -> b(_.as("hello") andThen bs.getString.opt),
            "b2" -> b(_.as(1) andThen bs.getInt.opt)
          )
        }

        def diamondFields[A] = builder[IO, A] { b =>
          b.fields(
            "diamondL" -> b(
              _.as("42".asLeft[Int]).bothThrough(bs.getString.one)(bs.getInt.one.map(_.toString())).map(_.merge)
            ),
            "diamondR" -> b(
              _.as(43.asRight[String]).bothThrough(bs.getString.one)(bs.getInt.one.map(_.toString())).map(_.merge)
            ),
            "diamondB" -> b(
              _.as(44.asRight[Int]).bothThrough(bs.getInt.one)(bs.getInt.one).map(_.merge).map(_.toString())
            ),
            "diamondPrefix" -> b(
              _.void
                .andThen(bs.getUnit.one)
                .as(45.asRight[Int])
                .bothThrough(bs.getInt.one)(bs.getInt.one)
                .map(_.merge)
                .void
                .andThen(bs.getUnit.one)
                .as("45")
            )
          )
        }

        case object DiamondTpe
        implicit lazy val diamondTpe: Type[IO, DiamondTpe.type] = builder[IO, DiamondTpe.type] { b =>
          b.tpe(
            "Diamond",
            diamondFields[DiamondTpe.type].head,
            diamondFields[DiamondTpe.type].tail: _*
          )
        }

        SchemaShape.unit[IO](
          builder[IO, Unit] { b =>
            b.fields(
              "opaque" -> b(_.as(OpaqueTpe)),
              "b2" -> b(_.as(2) andThen bs.getInt.opt),
              "diamonds" -> b(_.as(List(Some(DiamondTpe), Option.empty[DiamondTpe.type], Some(DiamondTpe))))
            ) ::: diamondFields[Unit]
          }
        )
      }
    }
    .unsafeRunSync()

  def runQuery(query: String): IO[JsonObject] =
    Compiler[IO]
      .compile(schema, query)
      .traverse {
        case Application.Query(fa) =>
          fa.map { res =>
            assert(clue(res.errors).isEmpty)
            res.data
          }
        case x => fail(s"unexpected program ${x.toString()}")
      }
      .flatMap(e => IO.fromEither(e.leftMap(es => new Exception(es.toString()))))

  test("should batch the two int resolvers") {
    val query = """
      query {
        opaque {
          b1
          b2
        }
        b2
      }
    """

    runQuery(query).flatMap { data =>
      assertEquals(data("b2").get.asNumber.get.toInt.get, 2)
      val opq = data("opaque").get.asObject.get
      assertEquals(opq("b1").get.asString.get, "hello")
      assertEquals(opq("b2").get.asNumber.get.toInt.get, 1)
      (intInputs.get, stringInputs.get).mapN { (ii, si) =>
        assertEquals(ii, List(Set(1, 2)))
        assertEquals(si, List(Set("hello")))
      }
    }
  }

  test("running diamonds individually should work") {
    List(
      "diamondL",
      "diamondR",
      "diamondB",
      "diamondPrefix"
    ).zipWithIndex.traverse { case (dia, i) =>
      val query = s"""
        query {
          $dia
        }
      """
      resetCounts *>
        runQuery(query).flatMap { data =>
          assertEquals(data(dia).get.asString.get, (42 + i).toString())
          (intInputs.get, stringInputs.get).mapN { (ii, si) =>
            assertEquals(ii.foldMap(_.size) + si.foldMap(_.size), 1, s"for $dia")
          }
        }
    }
  }

  test("running all diamonds should also work") {
    val query = """
      query {
        diamondL
        diamondR
        diamondB
        diamondPrefix
      }
    """

    resetCounts *>
      runQuery(query).flatMap { data =>
        assertEquals(data("diamondL").get.asString.get, "42")
        assertEquals(data("diamondR").get.asString.get, "43")
        assertEquals(data("diamondB").get.asString.get, "44")
        assertEquals(data("diamondPrefix").get.asString.get, "45")
        (intInputs.get, stringInputs.get).mapN { (ii, si) =>
          assertEquals(ii.foldMap(_.size) + si.foldMap(_.size), 4)
        }
      }
  }

  test("running diamonds inside and outside of a list should work too") {
    val query = """
      query {
        diamondL
        diamondR
        diamondB
        diamondPrefix
        diamonds {
          diamondL
          diamondR
          diamondB
          diamondPrefix
        }
        opaque {
          b1
          b2
        }
        b2
      }
    """

    resetCounts *> runQuery(query)
  }
}
