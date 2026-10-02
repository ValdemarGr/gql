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
package gql.preparation

import cats.Eval
import cats.data.{EitherNec, NonEmptyChain}
import cats.implicits._
import cats.effect.kernel.Unique
import gql.Cursor
import gql.parser.Type
import io.circe.Json
import munit.FunSuite

class AlgTest extends FunSuite {
  private val ops = Alg.Ops[Unit]
  private val firstVars: VariableMap[Unit] = Map("value" -> Variable(Type.Named("Int"), Left(Json.fromInt(1))))
  private val secondVars: VariableMap[Unit] = Map("value" -> Variable(Type.Named("Int"), Left(Json.fromInt(2))))
  private val staticError = PositionalError(Cursor.empty, List(()), "static failure")
  private val dynamicError = PositionalError(Cursor.empty.field("dynamic"), List(()), "dynamic failure")

  test("run returns static errors and reuses completed values") {
    assertEquals(ops.raiseError(staticError).run, Left(NonEmptyChain.one(staticError)))

    val bind = ops.nextId.run.fold(errors => fail(errors.toString), identity)
    val first = bind(firstVars).fold(errors => fail(errors.toString), identity)
    val second = bind(secondVars).fold(errors => fail(errors.toString), identity)
    assert(first eq second)
  }

  test("run caches the prefix and isolates bindings across repeated variable demands") {
    var preparations = 0
    val program = Alg.FlatMap[Unit, Unique.Token, (Unique.Token, VariableMap[Unit])](
      ops.nextId,
      token => {
        preparations += 1
        Alg.NeedVars(first =>
          Eval.now(Alg.NeedVars(second => {
            assertEquals(second, first)
            Eval.now(
              if (first.contains("value")) Alg.Pure((token, second))
              else ops.raiseError(dynamicError)
            )
          }))
        )
      }
    )
    val bind = program.run.fold(errors => fail(errors.toString), identity)
    assertEquals(preparations, 1)
    assertEquals(bind(Map.empty), Left(NonEmptyChain.one(dynamicError)))

    val first = bind(firstVars).fold(errors => fail(errors.toString), identity)
    val second = bind(secondVars).fold(errors => fail(errors.toString), identity)
    assert(first._1 eq second._1)
    assertEquals(first._2, firstVars)
    assertEquals(second._2, secondVars)
    assertEquals(bind(firstVars), Right(first))
    assertEquals(preparations, 1)
  }

  test("parallel preparation caches independent static tokens and allocates dynamic tokens per binding") {
    var staticRuns = 0
    var dynamicRuns = 0
    val static = ops.nextId.flatMap { token =>
      staticRuns += 1
      ops.pure(token)
    }
    val program = (ops.getVariables, static).parTupled.flatMap { case (variables, staticToken) =>
      dynamicRuns += 1
      ops.nextId.map(dynamicToken => (variables, staticToken, dynamicToken))
    }

    val bind = program.run.fold(errors => fail(errors.toString), identity)
    assertEquals(staticRuns, 1)
    assertEquals(dynamicRuns, 0)
    val first = bind(firstVars).fold(errors => fail(errors.toString), identity)
    val second = bind(secondVars).fold(errors => fail(errors.toString), identity)
    assertEquals(first._1, firstVars)
    assertEquals(second._1, secondVars)
    assert(first._2 eq second._2)
    assert(first._3 ne second._3)
    assertEquals(staticRuns, 1)
    assertEquals(dynamicRuns, 2)
  }

  test("cached static and request-dependent usage remain isolated across bindings") {
    val program = for {
      _ <- ops.useVariable("static")
      variables <- ops.getVariables
      _ <- ops.useVariable(variables("value").value.fold(_.noSpaces, _ => "default"))
      used <- ops.usedVariables
    } yield used
    val bind = program.run.fold(errors => fail(errors.toString), identity)

    assertEquals(bind(firstVars), Right(Set("static", "1")))
    assertEquals(bind(secondVars), Right(Set("static", "2")))
    assertEquals(bind(firstVars), Right(Set("static", "1")))
  }

  test("parallel siblings read inherited usage while their writes merge afterward") {
    val left = ops.getVariables *> ops.useVariable("left")
    val right = ops.usedVariables.map(used => (_: Unit) => used)
    val program = for {
      _ <- ops.useVariable("before")
      observed <- ops.parAp(left)(right)
      used <- ops.usedVariables
    } yield (observed, used)
    val bind = program.run.fold(errors => fail(errors.toString), identity)

    assertEquals(bind(firstVars), Right((Set("before"), Set("before", "left"))))
    assertEquals(bind(secondVars), Right((Set("before"), Set("before", "left"))))
  }

  test("cursor and cycle scopes survive suspension and restore their enclosing context") {
    val cursor = Cursor.empty.field("scope").index(3)
    val inner = ops.cycleOver(
      "scope",
      ops.cursorOver(cursor, ops.getVariables *> ops.getVariables *> ops.useVariable("inside") *> (ops.cursorAsk, ops.cycleAsk).tupled)
    )
    val program = for {
      _ <- ops.useVariable("before")
      inside <- inner
      outsideCursor <- ops.cursorAsk
      outsideCycles <- ops.cycleAsk
      used <- ops.usedVariables
    } yield (inside, outsideCursor, outsideCycles, used)
    val bind = program.run.fold(errors => fail(errors.toString), identity)

    List(firstVars, secondVars).foreach { variables =>
      assertEquals(bind(variables), Right(((cursor, Set("scope")), Cursor.empty, Set.empty[String], Set("before", "inside"))))
    }
  }

  test("attempt rolls failed static and deferred usage back while retaining successful writes") {
    val static = ops.useVariable("static-failed") *> ops.raiseError(staticError)
    val dynamic = ops.useVariable("dynamic-failed") *> ops.getVariables *> ops.getVariables *> ops.raiseError(dynamicError)
    val program = for {
      _ <- ops.useVariable("before")
      staticResult <- ops.attempt(static)
      dynamicResult <- ops.attempt(dynamic)
      successResult <- ops.attempt(ops.useVariable("successful") *> ops.getVariables)
      used <- ops.usedVariables
    } yield (staticResult, dynamicResult, successResult, used)
    val bind = program.run.fold(errors => fail(errors.toString), identity)

    List(firstVars, secondVars).foreach { variables =>
      val result = bind(variables).fold(errors => fail(errors.toString), identity)
      assertEquals(result._1, Left(NonEmptyChain.one(staticError)))
      assertEquals(result._2, Left(NonEmptyChain.one(dynamicError)))
      assertEquals(result._3, Right(variables))
      assertEquals(result._4, Set("before", "successful"))
    }
  }

  test("attempt does not capture errors raised by its enclosing continuation") {
    val program = ops.attempt(ops.getVariables *> ops.raiseError(dynamicError)).flatMap {
      case Left(errors) =>
        assertEquals(errors, NonEmptyChain.one(dynamicError))
        ops.raiseError(staticError)
      case Right(_) => fail("Expected deferred failure")
    }
    val bind = program.run.fold(errors => fail(errors.toString), identity)
    assertEquals(bind(firstVars), Left(NonEmptyChain.one(staticError)))
    assertEquals(bind(secondVars), Left(NonEmptyChain.one(staticError)))
  }

  test("written errors do not stop static token allocation") {
    var continued = false
    val errors = NonEmptyChain.of(staticError, dynamicError)
    val program = ops.writeErrors(errors) *> ops.nextId.map { token =>
      continued = true
      token
    }

    assertEquals(program.run, Left(errors))
    assert(continued)
  }

  test("written errors precede hard failures") {
    val program = ops.writeError(staticError) *> ops.raiseError(dynamicError)
    assertEquals(program.run, Left(NonEmptyChain.of(staticError, dynamicError)))
  }

  test("cached written prefixes and request errors remain isolated across bindings") {
    val program = ops.writeError(staticError) *> ops.getVariables.flatMap { variables =>
      ops.writeErr(variables("value").value.fold(_.noSpaces, _ => "default"), List(()))
    }
    val bind = program.run.fold(errors => fail(errors.toString), identity)
    val firstError = PositionalError(Cursor.empty, List(()), "1")
    val secondError = PositionalError(Cursor.empty, List(()), "2")

    assertEquals(bind(firstVars), Left(NonEmptyChain.of(staticError, firstError)))
    assertEquals(bind(secondVars), Left(NonEmptyChain.of(staticError, secondError)))
    assertEquals(bind(firstVars), Left(NonEmptyChain.of(staticError, firstError)))
  }

  test("written errors retain suspended cursor and cycle scope then restore outer context") {
    val cursor = Cursor.empty.field("scope").index(3)
    val inner = ops.cycleOver(
      "scope",
      ops.cursorOver(
        cursor,
        for {
          _ <- ops.writeErr("inside static", List(()))
          _ <- ops.getVariables
          cycles <- ops.cycleAsk
          _ <- ops.writeErr("inside dynamic", List(()))
        } yield {
          assertEquals(cycles, Set("scope"))
        }
      )
    )
    val program = for {
      _ <- ops.writeError(staticError)
      _ <- inner
      cycles <- ops.cycleAsk
      _ <- ops.writeErr("outside", List(()))
    } yield {
      assertEquals(cycles, Set.empty[String])
    }
    val bind = program.run.fold(errors => fail(errors.toString), identity)
    val expected = NonEmptyChain.of(
      staticError,
      PositionalError(cursor, List(()), "inside static"),
      PositionalError(cursor, List(()), "inside dynamic"),
      PositionalError(Cursor.empty, List(()), "outside")
    )

    assertEquals(bind(firstVars), Left(expected))
    assertEquals(bind(secondVars), Left(expected))
  }

  test("attempt retains successful written errors and rolls failed local writes back") {
    val kept = PositionalError(Cursor.empty, List(()), "kept")
    val discarded = PositionalError(Cursor.empty, List(()), "discarded")
    val successful = ops.writeError(kept) *> ops.getVariables
    val staticFailure = ops.writeError(discarded) *> ops.raiseError(staticError)
    val dynamicFailure = ops.writeError(discarded) *> ops.getVariables *> ops.writeError(discarded) *> ops.raiseError(dynamicError)
    val program = for {
      _ <- ops.writeError(staticError)
      success <- ops.attempt(successful)
      caughtStatic <- ops.attempt(staticFailure)
      caughtDynamic <- ops.attempt(dynamicFailure)
    } yield {
      assert(success.isRight)
      assertEquals(caughtStatic, Left(NonEmptyChain.one(staticError)))
      assertEquals(caughtDynamic, Left(NonEmptyChain.one(dynamicError)))
    }
    val bind = program.run.fold(errors => fail(errors.toString), identity)

    assertEquals(bind(firstVars), Left(NonEmptyChain.of(staticError, kept)))
    assertEquals(bind(secondVars), Left(NonEmptyChain.of(staticError, kept)))
  }

  List("success", "failure", "pending").foreach { leftMode =>
    List("success", "failure", "pending").foreach { rightMode =>
      test(s"parallel written errors occur once for $leftMode left and $rightMode right") {
        val prefix = PositionalError(Cursor.empty, List(()), "prefix")
        val leftError = PositionalError(Cursor.empty.field("left"), List(()), "left written")
        val rightError = PositionalError(Cursor.empty.field("right"), List(()), "right written")
        val left: Alg[Unit, Int] = leftMode match {
          case "success" => ops.writeError(leftError).as(2)
          case "failure" => ops.writeError(leftError) *> ops.raiseError(staticError)
          case _ =>
            ops.writeError(leftError) *> ops.getVariables.flatMap { variables =>
              if (variables.isEmpty) ops.raiseError(staticError) else ops.pure(2)
            }
        }
        val right: Alg[Unit, Int => Int] = rightMode match {
          case "success" => ops.writeError(rightError).as((value: Int) => value + 1)
          case "failure" => ops.writeError(rightError) *> ops.raiseError(dynamicError)
          case _ =>
            ops.writeError(rightError) *> ops.getVariables.flatMap { variables =>
              if (variables.isEmpty) ops.raiseError(dynamicError) else ops.pure((value: Int) => value + 1)
            }
        }
        val prepared = (ops.writeError(prefix) *> ops.parAp(left)(right)).run

        List(firstVars, Map.empty[String, Variable[Unit]], secondVars).foreach { variables =>
          val hardErrors = List(
            Option.when(leftMode == "failure" || (leftMode == "pending" && variables.isEmpty))(staticError),
            Option.when(rightMode == "failure" || (rightMode == "pending" && variables.isEmpty))(dynamicError)
          ).flatten
          val expected = NonEmptyChain.fromSeq(List(prefix, leftError, rightError) ++ hardErrors).toLeft(3)
          assertEquals(prepared.flatMap(_(variables)), expected)
        }
      }
    }
  }

  List("success", "failure", "pending").foreach { leftMode =>
    List("success", "failure", "pending").foreach { rightMode =>
      test(s"parallel application combines $leftMode left and $rightMode right") {
        val leftSuccess = ops.useVariable("left") *> ops.pure(2)
        val rightSuccess = ops.useVariable("right") *> ops.pure((value: Int) => value + 1)
        val left: Alg[Unit, Int] = leftMode match {
          case "success" => leftSuccess
          case "failure" => ops.raiseError(staticError)
          case _         => ops.getVariables.flatMap(variables => if (variables.isEmpty) ops.raiseError(staticError) else leftSuccess)
        }
        val right: Alg[Unit, Int => Int] = rightMode match {
          case "success" => rightSuccess
          case "failure" => ops.raiseError(dynamicError)
          case _         => ops.getVariables.flatMap(variables => if (variables.isEmpty) ops.raiseError(dynamicError) else rightSuccess)
        }
        val program = for {
          _ <- ops.useVariable("before")
          value <- ops.parAp(left)(right)
          used <- ops.usedVariables
        } yield (value, used)
        val prepared = program.run

        List(firstVars, Map.empty[String, Variable[Unit]], secondVars).foreach { variables =>
          val errors = List(
            Option.when(leftMode == "failure" || (leftMode == "pending" && variables.isEmpty))(staticError),
            Option.when(rightMode == "failure" || (rightMode == "pending" && variables.isEmpty))(dynamicError)
          ).flatten
          val expected: EitherNec[PositionalError[Unit], (Int, Set[String])] =
            NonEmptyChain.fromSeq(errors).toLeft((3, Set("before", "left", "right")))
          assertEquals(prepared.flatMap(_(variables)), expected)
        }
      }
    }
  }

  test("deep variable demands and sequential writes remain stack safe") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.unit) { (acc, index) =>
      acc *> ops.getVariables *> ops.useVariable(index.toString)
    } *> ops.usedVariables
    val expected = (0 until depth).map(_.toString).toSet
    val bind = program.run.fold(errors => fail(errors.toString), identity)

    assertEquals(bind(firstVars), Right(expected))
    assertEquals(bind(secondVars), Right(expected))
  }

  test("deep static binds retain usage without overflowing the stack") {
    val depth = 20000
    def program(remaining: Int): Alg[Unit, Unit] =
      if (remaining == 0) ops.unit
      else ops.useVariable(remaining.toString).flatMap(_ => program(remaining - 1))
    val expected = (1 to depth).map(_.toString).toSet
    val bind = (program(depth) *> ops.usedVariables).run.fold(errors => fail(errors.toString), identity)

    assertEquals(bind(firstVars), Right(expected))
    assertEquals(bind(secondVars), Right(expected))
  }

  test("deep parallel variable demands remain stack safe") {
    val program = (0 until 20000).foldLeft(ops.unit) { (acc, _) =>
      ops.parAp(acc)(ops.getVariables.as((_: Unit) => ()))
    }
    val bind = program.run.fold(errors => fail(errors.toString), identity)
    assertEquals(bind(firstVars), Right(()))
    assertEquals(bind(secondVars), Right(()))
  }
}
