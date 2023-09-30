package gql.relational.skunk

import gql.relational.RelationalSuiteTables
import skunk._
import skunk.implicits._
import skunk.codec.all._
import munit.CatsEffectSuite
import cats.effect._
import cats.implicits._
import skunk.util.Typer
import cats.effect.std.UUIDGen
import munit.catseffect.IOFixture
import munit.AnyFixture
import gql.Application

class SkunkRelationalSuite extends CatsEffectSuite {
  val suite = new RelationalSuiteTables(SkunkIntegration) {
    def intDecoder: Decoder[Int] = int4
    def textDecoder: Decoder[String] = text
    def encodeText(str: String): AppliedFragment = sql"${text}".apply(str)
  }

  implicit val t = natchez.noop.NoopTrace[IO]()

  val connF = ResourceSuiteLocalFixture(
    "setup",
    Resource.unit[IO] >> {
      def connect(db: String) = Session.single[IO](
        host = "127.0.0.1",
        port = 5432,
        user = "postgres",
        database = db,
        password = Some("1234"),
        strategy = Typer.Strategy.SearchPath
      )

      def postgres = connect("postgres")

      Resource.eval(UUIDGen.randomString[IO]).flatMap{ dbid =>
        Resource.make(postgres.use(_.execute(sql"""create database "#$dbid"""".command)))(_ => 
          postgres.use(_.execute(sql"""drop database "#$dbid"""".command).void)
        ).as(connect(dbid))
      }
    }
  )

  override def munitFixtures: Seq[AnyFixture[_]] = Seq(connF) ++ super.munitFixtures

  lazy val conn = connF.apply()

  test("setup") {
    conn.use{ ses =>
      (suite.ddlQueries ++ suite.dataQueries).traverse_(x => ses.execute(sql"#$x".command))
    }
  }

  test("fire query") {
    import io.circe.syntax._
    suite.schema[IO](conn).map{ schema =>
      gql.Compiler[IO].compile(
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
    }.flatMap{ case Right(Application.Query(fa)) => 
      fa.map(_.handleErrors{e => println(e.getMessage()); ""}.asJson.spaces2) 
    }.map(fail(_): Unit)
  }
}