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
package gql.parser

import cats.implicits._
import munit.CatsEffectSuite
import cats.effect._

class GithubParserTest extends CatsEffectSuite {
  test(s"parsing github query") {
    fs2.io.readClassLoaderResource[IO]("./schema.graphql").through(fs2.text.utf8.decode).compile.string.map { sch =>
      assert(clue(gql.parser.parseSchema(sch).leftMap(_.prettyError.value)).isRight)
    }
    // assert(clue(gql.parser.parseQuery(q).leftMap(_.prettyError.value)).isRight)
  }
}
