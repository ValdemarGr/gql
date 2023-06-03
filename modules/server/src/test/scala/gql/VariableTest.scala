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
package gql

import gql.ast._
import gql.dsl._
import cats.effect._
import munit.CatsEffectSuite

final case class CustomScalar(value: String)

class VariableTest extends CatsEffectSuite {
  implicit val customScalar: Scalar[CustomScalar] = stringScalar
    .eimap[CustomScalar](x => Right(CustomScalar(x)))(_.value)
    .rename("CustomScalar")

  lazy val schemaShape = SchemaShape
    .unit[IO](fields("getMeAString" -> lift(arg[CustomScalar]("cs"))((cs, _) => cs)))

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  import io.circe.syntax._
  test("should be able to accept an non-primitive scalar variable") {
    assert {
      clue {
        Compiler[IO].compile(
          schema,
          """
            query MyQuery($myVar: CustomScalar!) {
              getMeAString(cs: $myVar)
            }
          """,
          variables = Map("myVar" -> "Hello World".asJson)
        )
      }.isRight
    }
  }

  test("should fail when given a nullable scalar variable") {
    assert {
      clue {
        Compiler[IO].compile(
          schema,
          """
            query MyQuery($myVar: CustomScalar) {
              getMeAString(cs: $myVar)
            }
          """,
          variables = Map("myVar" -> "Hello World".asJson)
        )
      }.isLeft
    }
  }
}
