package gql

import munit.CatsEffectSuite
import gql.ast._
import gql.dsl.all._
import cats.effect._
import cats.implicits._
import gql.resolver.Resolver

class BatchTest extends CatsEffectSuite {
  case class Batchers(
      getString: Resolver[IO, Set[String], Map[String, String]],
      getInt: Resolver[IO, Set[Int], Map[Int, Int]]
  )

  lazy val intInputs = IO.ref(List.empty[Set[Int]]).unsafeRunSync()
  lazy val stringInputs = IO.ref(List.empty[Set[String]]).unsafeRunSync()

  lazy val batchersF = (
    Resolver.batch[IO, String, String](xs => IO(xs.map(x => (x, x)).toMap) <* stringInputs.update(xs :: _)),
    Resolver.batch[IO, Int, Int](xs => IO(xs.map(x => (x, x)).toMap) <* intInputs.update(xs :: _))
  ).mapN(Batchers.apply)

  lazy val schema: Schema[IO, Unit, Unit, Unit] = Schema
    .stateful {
      batchersF.map { bs =>
        case object OpaqueTpe
        implicit lazy val opaqueTpe: Type[IO, OpaqueTpe.type] = builder[IO, OpaqueTpe.type] { b =>
          b.tpe(
            "Opaque",
            "b1" -> b(_.as("hello") andThen bs.getString.opt),
            "b2" -> b(_.as(1) andThen bs.getInt.opt)
          )
        }

        SchemaShape.unit[IO](
          builder[IO, Unit] { b =>
            b.fields(
              "opaque" -> b(_.as(OpaqueTpe)),
              "b2" -> b(_.as(2) andThen bs.getInt.opt)
            )
          }
        )
      }
    }
    .unsafeRunSync()

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

    Compiler[IO]
      .compile(schema, query)
      .traverse {
        case Application.Query(fa) =>
          fa.flatMap { qr =>
            assert(clue(qr.errors).isEmpty)
            assertEquals(qr.data("b2").get.asNumber.get.toInt.get, 2)
            val opq = qr.data("opaque").get.asObject.get
            assertEquals(opq("b1").get.asString.get, "hello")
            assertEquals(opq("b2").get.asNumber.get.toInt.get, 1)
            (intInputs.get, stringInputs.get).mapN { (ii, si) =>
              assertEquals(ii, List(Set(1, 2)))
              assertEquals(si, List(Set("hello")))
            }
          }
        case x => fail(s"unexpected program ${x.toString()}")
      }
  }
}
