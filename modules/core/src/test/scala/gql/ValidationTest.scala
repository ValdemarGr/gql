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

  val duplicateFields = fields[IO, MutRecInterface](
    "value" -> lift(_.value),
    "missing" -> lift(_ => 42)
  )

  implicit lazy val duplicateInterface: Interface[IO, MutRecInterface] =
    interfaceFromNel[IO, MutRecInterface]("MutRecInterface", duplicateFields)

  implicit def mr1: Type[IO, MutuallyRecursive1] =
    tpe[IO, MutuallyRecursive1](
      "MutuallyRecursive1",
      "value" -> lift(_.value),
      "two" -> lift(x => MutuallyRecursive2(x.value * 2))
    )
      .subtypeOf[MutRecInterface]
      .subtypeOf[MutRecInterface]

  implicit lazy val mr2: Type[IO, MutuallyRecursive2] =
    tpe[IO, MutuallyRecursive2](
      "MutuallyRecursive2",
      "value" -> lift(_.value),
      "one" -> lift(x => MutuallyRecursive1(x.value * 2))
    )
      .subtypeOf[MutRecInterface]

  implicit lazy val badStructure: Type[IO, BadStructure] =
    tpe[IO, BadStructure](
      "BadStructure",
      "0value" -> lift(arg[String]("@value")) { case _ => 42 },
      "real" -> lift(_ => 24),
      "real" -> lift((arg[String]("x"), arg[String]("x")).tupled) { case _ => 24 },
      "ci" -> lift(
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

  def sharedFields[A] = fields[IO, A](
    "one" -> lift(_ => "heya"),
    "two" -> lift(arg[String]("x")) { case _ => "heya" },
    "three" -> lift(arg[Int]("x", value.scalar(42))) { case _ => "heya" },
    "four" -> lift((arg[Int]("x"), arg[String]("h")).tupled) { case _ => "heya" }
  )

  trait Catch
  implicit lazy val baseInterface = interfaceFromNel[IO, Catch](
    "BaseInterface",
    sharedFields
  )

  case object CatchSub extends Catch
  implicit lazy val subtpe = tpe[IO, CatchSub.type](
    "Subtype",
    "one" -> lift(arg[String]("x")){ case _ => "heya" },
    "two" -> lift { case _ => "heya" },
    "three" -> lift(arg[Int]("x", value.scalar(43))) { case _ => "heya" },
    "four" -> lift(arg[Int]("x")) { case _ => "heya" }
  ).subtypeOf[Catch]

  lazy val schemaShape: SchemaShape[IO, Unit, Unit, Unit] = SchemaShape.make[IO](
    tpe[IO, Unit](
      "Query",
      "badStructure" -> lift(_ => BadStructure()),
      "duplicateUnion" -> lift(_ => (MutuallyRecursive1(42): MutRecUnion)),
      "duplicateInterface" -> lift(_ => (MutuallyRecursive1(42): MutRecInterface)),
      "sub" -> lift(_ => CatchSub),
    )
  )

  lazy val schema = Schema.simple(schemaShape).unsafeRunSync()

  lazy val errors = schema.validate.toList.map(x => (x.error, x.path))

  // test("no errors") {
  //   assertEquals(errors, Nil)
  // }

  test("should catch cyclic outputs that are not reference equal") {
    /*fail(errors.map{ case (e, at) =>
      s"${e.message}\nat ${at.map(_.name).mkString_(".")}"
    }.mkString("\n")): Unit*/

    val err = errors.collect { case (Validation.Error.CyclicDivergingTypeReference("MutuallyRecursive1"), path) =>
      path.toList
    }

    assert(clue(err).size == 1)
    val s = err.toSet
    assert(clue(s).contains {
      Validation.Edge.OutputType("Query") ::
        Validation.Edge.Field("duplicateUnion") ::
        Validation.Edge.OutputType("MutRecUnion") ::
        Validation.Edge.OutputType("MutuallyRecursive1") ::
        Validation.Edge.Field("two") ::
        Validation.Edge.OutputType("MutuallyRecursive2") ::
        Validation.Edge.Field("one") ::
        Validation.Edge.OutputType("MutuallyRecursive1") ::
        Nil
    })
    // assert(clue(s).contains {
    //   Validation.Edge.OutputType("Query") ::
    //     Validation.Edge.Field("duplicateInterface") ::
    //     Validation.Edge.OutputType("MutRecInterface") ::
    //     Validation.Edge.OutputType("MutuallyRecursive1") ::
    //     Validation.Edge.Field("two") ::
    //     Validation.Edge.OutputType("MutuallyRecursive2") ::
    //     Validation.Edge.Field("one") ::
    //     Validation.Edge.OutputType("MutuallyRecursive1") ::
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
