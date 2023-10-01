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
package gql.relational.skunk

import gql.relational.RelationalSuiteTables
import skunk._
import skunk.implicits._
import skunk.codec.all._
import cats.effect._
import cats.implicits._
import skunk.util.Typer
import cats.effect.std.UUIDGen
import munit.AnyFixture

class SkunkRelationalSuite extends RelationalSuiteTables(SkunkIntegration) {
  def intDecoder: Decoder[Int] = int4
  def textDecoder: Decoder[String] = text
  def encodeText(str: String): AppliedFragment = sql"${text}".apply(str)

  implicit val t: natchez.Trace[IO] = natchez.noop.NoopTrace[IO]()

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

      Resource.eval(UUIDGen.randomString[IO]).flatMap { dbid =>
        Resource
          .make(postgres.use(_.execute(sql"""create database "#$dbid"""".command)))(_ =>
            postgres.use(_.execute(sql"""drop database "#$dbid"""".command).void)
          )
          .as(connect(dbid))
      }
    }
  )

  override def munitFixtures: Seq[AnyFixture[_]] = Seq(connF) ++ super.munitFixtures

  lazy val conn = connF.apply()

  test("setup") {
    conn.use { ses =>
      (ddlQueries ++ dataQueries).traverse_(x => ses.execute(sql"#$x".command))
    }
  }

  tests(conn)
}
