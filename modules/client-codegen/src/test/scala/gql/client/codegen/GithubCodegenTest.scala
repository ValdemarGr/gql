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
package gql.client.codegen

import munit.CatsEffectSuite
import fs2.io.file._
import cats.effect._

class GithubCodegenTest extends CatsEffectSuite {
  val qry = """
query Test {
  organization(login: "casehubdk") {
    projectV2(number: 12) {
      fields(first: 20) {
        nodes {
          ... on ProjectV2FieldCommon {
            id
            name
            dataType
          }
        }
      }
    }
  }
}
  """

  test("gen the schema") {
    Files[IO].tempDirectory.use { dir =>
      val shared = dir / "shared.graphql"
      val schema = dir / "schema.graphql"
      val qryloc = dir / "query.graphql"
      val input = Generator.Input(query = qryloc, output = dir / "output.scala")
      val writeF =
        fs2.Stream(qry).through(fs2.text.utf8.encode).through(Files[IO].writeAll(qryloc)).compile.drain >>
          fs2.io.readClassLoaderResource[IO]("./schema.graphql").through(Files[IO].writeAll(schema)).compile.drain

      writeF *>
        Generator.mainGenerate[IO](
          schema,
          shared,
          validate = false,
          packageName = None
        )(List(input))
    }
  }
}
