package gql

import io.circe._
import munit.CatsEffectSuite
import gql._
import gql.ast._
import gql.dsl._
import cats.effect._
import cats.implicits._

class ValidationTest {
  import ValidationTest._

  implicit def cyclicInput: Input[CyclicInput] =
    input[CyclicInput](
      "CyclicInput",
      (
        arg[Int]("value"),
        arg[Option[CyclicInput]]("cyclicInput")
      ).mapN(CyclicInput.apply)
    )

  implicit def mr1: Type[IO, MutuallyRecursive1] =
    tpe[IO, MutuallyRecursive1](
      "MutuallyRecursive1",
      "value" -> pure(_.value),
      "two" -> pure(x => MutuallyRecursive2(x.value * 2))
    )

  implicit lazy val mr2: Type[IO, MutuallyRecursive2] =
    tpe[IO, MutuallyRecursive2](
      "MutuallyRecursive2",
      "value" -> pure(_.value),
      "one" -> pure(x => MutuallyRecursive1(x.value * 2))
    )

  implicit lazy val badStructure: Type[IO, BadStructure] =
    tpe[IO, BadStructure](
      "BadTypeName",
      "0value" -> pure(arg[String]("@value")) { case _ => 42 },
      "real" -> pure(_ => 24),
      "real" -> pure((arg[String]("x"), arg[String]("x")).tupled) { case _ => 24 },
      "ci" -> pure(
        arg[CyclicInput](
          "ci",
          default.obj(
            "nonExistent" -> default(42),
            "value" -> default("42"),
            "value" -> default(42)
          )
        )
      ) { case _ => 24 }
    )

  implicit lazy val duplicateUnion =
    union[IO, MutRecUnion](
      "MutRec",
      instance[MutuallyRecursive1] { case x: MutuallyRecursive1 => x },
      instance[MutuallyRecursive2] { case x: MutuallyRecursive2 => x },
      instance[MutuallyRecursive1] { case x: MutuallyRecursive1 => x }
    )

  implicit lazy val duplicateInterface =
    interface[IO, MutRecInterface](
      "MutRec",
      "value" -> pure(_.value),
      "missing" -> pure(_ => 42)
    )(
      instance[MutuallyRecursive1] { case x: MutuallyRecursive1 => x },
      instance[MutuallyRecursive2] { case x: MutuallyRecursive2 => x },
      instance[MutuallyRecursive1] { case x: MutuallyRecursive1 => x }
    )
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
