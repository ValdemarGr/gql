/*
 * Copyright 2022 Valdemar Grange
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

import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl._
import cats.effect._
import cats.implicits._

class ValidationTest extends CatsEffectSuite {
  import ValidationTest._

  implicit def cyclicInput: Input[CyclicInput] =
    input[CyclicInput](
      "CyclicInput",
      (
        arg[Int]("value"),
        arg[Option[CyclicInput]]("cyclicInput")
      ).mapN(CyclicInput.apply)
    )

  val duplicateFields = fieldGroup[IO, MutRecInterface](
    "value" -> pure(_.value),
    "missing" -> pure(_ => 42)
  )

  implicit lazy val duplicateInterface: Interface[IO, MutRecInterface] = 
    interfaceFrom[IO, MutRecInterface](    "MutRecInterface",    duplicateFields  )

  implicit def mr1: Type[IO, MutuallyRecursive1] =
    tpe[IO, MutuallyRecursive1](
      "MutuallyRecursive1",
      "value" -> pure(_.value),
      "two" -> pure(x => MutuallyRecursive2(x.value * 2))
    )
      .subtypeOf[MutRecInterface]
      .subtypeOf[MutRecInterface]

  implicit lazy val mr2: Type[IO, MutuallyRecursive2] =
    tpe[IO, MutuallyRecursive2](
      "MutuallyRecursive2",
      "value" -> pure(_.value),
      "one" -> pure(x => MutuallyRecursive1(x.value * 2))
    )
      .subtypeOf[MutRecInterface]

  implicit lazy val badStructure: Type[IO, BadStructure] =
    tpe[IO, BadStructure](
      "BadStructure",
      "0value" -> pure(arg[String]("@value")) { case _ => 42 },
      "real" -> pure(_ => 24),
      "real" -> pure((arg[String]("x"), arg[String]("x")).tupled) { case _ => 24 },
      "ci" -> pure(
        arg[CyclicInput](
          "ci",
          value.obj(
            "nonExistent" -> value.scalar(42),
            "value" -> value.scalar("42"),
            "value" -> value.scalar(42)
          )
        )
      ) { case _ => 24 }
    )

  implicit lazy val duplicateUnion: Union[IO, MutRecUnion] = union[IO, MutRecUnion]("MutRecUnion")
    .variant { case x: MutuallyRecursive2 => x }
    .variant { case x: MutuallyRecursive1 => x }
    .variant { case x: MutuallyRecursive2 => x }

  lazy val schemaShape: SchemaShape[IO, Unit, Unit, Unit] = SchemaShape.make[IO](
    tpe[IO, Unit](
      "Query",
      "badStructure" -> pure(_ => BadStructure()),
      "duplicateUnion" -> pure(_ => (MutuallyRecursive1(42): MutRecUnion)),
      "duplicateInterface" -> pure(_ => (MutuallyRecursive1(42): MutRecInterface))
    )
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  lazy val errors = schema.validate.toList.map(x => (x.error, x.path))

  // test("no errors") {
  //   assertEquals(errors, Nil)
  // }

  test("should catch cyclic outputs that are not reference equal") {
    val err = errors.collect { case (SchemaShape.ValidationError.CyclicDivergingTypeReference("MutuallyRecursive1"), path) =>
      path.toList
    }

    assert(clue(err).size == 1)
    val s = err.toSet
    assert(clue(s).contains {
      SchemaShape.ValidationEdge.OutputType("Query") ::
        SchemaShape.ValidationEdge.Field("duplicateUnion") ::
        SchemaShape.ValidationEdge.OutputType("MutRecUnion") ::
        SchemaShape.ValidationEdge.OutputType("MutuallyRecursive1") ::
        SchemaShape.ValidationEdge.Field("two") ::
        SchemaShape.ValidationEdge.OutputType("MutuallyRecursive2") ::
        SchemaShape.ValidationEdge.Field("one") ::
        SchemaShape.ValidationEdge.OutputType("MutuallyRecursive1") ::
        Nil
    })
    // assert(clue(s).contains {
    //   SchemaShape.ValidationEdge.OutputType("Query") ::
    //     SchemaShape.ValidationEdge.Field("duplicateInterface") ::
    //     SchemaShape.ValidationEdge.OutputType("MutRecInterface") ::
    //     SchemaShape.ValidationEdge.OutputType("MutuallyRecursive1") ::
    //     SchemaShape.ValidationEdge.Field("two") ::
    //     SchemaShape.ValidationEdge.OutputType("MutuallyRecursive2") ::
    //     SchemaShape.ValidationEdge.Field("one") ::
    //     SchemaShape.ValidationEdge.OutputType("MutuallyRecursive1") ::
    //     Nil
    // })
  }
}

object ValidationTest {
  sealed trait MutRecInterface { def value: Int }
  sealed trait MutRecUnion
  final case class MutuallyRecursive1(value: Int) extends MutRecUnion with MutRecInterface
  final case class MutuallyRecursive2(value: Int) extends MutRecUnion with MutRecInterface

  final case class CyclicInput(
      value: Int,
      nested: Option[CyclicInput]
  )

  final case class BadStructure()
}
