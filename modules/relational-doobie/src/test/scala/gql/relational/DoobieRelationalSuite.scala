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
package gql.relational.doobie

import gql.relational.RelationalSuiteTables
import cats.effect._
import cats.implicits._
import cats.effect.std.UUIDGen
import munit.AnyFixture
import doobie._
import doobie.implicits._

class DoobieRelationalSuite extends RelationalSuiteTables(DoobieIntegraion) {
  def intDecoder: Read[Int] = Read[Int]
  def textDecoder: Read[String] = Read[String]
  def encodeText(str: String): Fragment = sql"$str"

  def connect(db: String) = Transactor.fromDriverManager[IO](
    driver = "org.postgresql.Driver",
    url = s"jdbc:postgresql://127.0.0.1:5432/$db?user=postgres&password=1234",
    logHandler = None
  )

  val connF = ResourceSuiteLocalFixture(
    "setup",
    Resource.unit[IO] >> {

      def postgres = Transactor.after.set(
        Transactor.before.set(connect("postgres"), FC.setAutoCommit(true)),
        FC.unit
      )

      Resource.eval(UUIDGen.randomString[IO]).map(_.replace("-", "")).flatMap { dbid =>
        val d = Fragment.const0(dbid)
        Resource
          .make(sql"""create database "$d"""".update.run.transact(postgres))(_ =>
            sql"""drop database "$d"""".update.run.transact(postgres).void
          )
          .as(connect(dbid))
      }
    }
  )

  override def munitFixtures: Seq[AnyFixture[_]] = Seq(connF) ++ super.munitFixtures

  lazy val conn = connF.apply()

  test("setup") {
    (ddlQueries ++ dataQueries).traverse_(x => Fragment.const0(x).update.run.transact(conn))
  }

  tests(conn)
}
