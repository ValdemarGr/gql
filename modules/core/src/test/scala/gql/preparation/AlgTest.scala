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

import cats.data.{EitherNec, NonEmptyChain}
import cats.implicits._
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

  test("static programs finish during staging") {
    assertEquals(Alg.eval[Unit, Int](ops.pure(42)), Alg.Staged.Done[Unit, Int](Right(42)))
    assertEquals(Alg.eval(ops.raiseError(staticError)), Alg.Staged.Done[Unit, Nothing](Left(NonEmptyChain.one(staticError))))
  }

  test("cached static usage and request-dependent usage remain isolated across binds") {
    val program = for {
      _ <- ops.useVariable("static")
      variables <- ops.getVariables
      _ <- ops.useVariable(variables("value").value.fold(_.noSpaces, _ => "default"))
      used <- ops.usedVariables
    } yield used
    val staged = Alg.eval(program)

    assertEquals(staged.runToCompletion(firstVars), Right(Set("static", "1")))
    assertEquals(staged.runToCompletion(secondVars), Right(Set("static", "2")))
    assertEquals(staged.runToCompletion(firstVars), Right(Set("static", "1")))
  }

  test("resume preserves cached static usage in another interpreter") {
    val staged = Alg.eval(ops.useVariable("cached") *> ops.getVariables)
    val program = ops.useVariable("outer") *> ops.resume(staged) *> ops.usedVariables

    assertEquals(program.runToCompletion(firstVars), Right(Set("cached", "outer")))
    assertEquals(program.runToCompletion(secondVars), Right(Set("cached", "outer")))
  }

  test("force captures static and deferred usage until resumed") {
    val inner = ops.useVariable("static") *> ops.getVariables *> ops.useVariable("dynamic")
    val program = for {
      forced <- ops.force(inner)
      before <- ops.usedVariables
      _ <- ops.resume(forced)
      after <- ops.usedVariables
    } yield (before, after)

    assertEquals(program.runToCompletion(firstVars), Right((Set.empty[String], Set("static", "dynamic"))))
    assertEquals(program.runToCompletion(secondVars), Right((Set.empty[String], Set("static", "dynamic"))))
  }

  test("attempt rolls failed static and deferred usage back") {
    val static = ops.useVariable("static-failed") *> ops.raiseError(staticError)
    val dynamic = ops.useVariable("dynamic-failed") *> ops.getVariables *> ops.raiseError(dynamicError)
    val program = for {
      _ <- ops.useVariable("before")
      _ <- ops.attempt(static)
      _ <- ops.attempt(dynamic)
      _ <- ops.attempt(ops.useVariable("successful") *> ops.getVariables)
      used <- ops.usedVariables
    } yield used

    assertEquals(program.runToCompletion(firstVars), Right(Set("before", "successful")))
    assertEquals(program.runToCompletion(secondVars), Right(Set("before", "successful")))
  }

  test("parallel right state reads include deferred successful left usage") {
    val left = ops.getVariables *> ops.useVariable("left")
    val right = ops.usedVariables.map(used => (_: Unit) => used)
    val program = ops.useVariable("before") *> ops.parAp(left)(right)

    assertEquals(program.runToCompletion(firstVars), Right(Set("before", "left")))
  }

  test("parallel failed left usage does not reach right state reads") {
    var observed = Set.empty[String]
    val left: Alg[Unit, Unit] = ops.getVariables *> ops.useVariable("failed") *> ops.raiseError(dynamicError)
    val right = ops.usedVariables.map { used =>
      observed = used
      (_: Unit) => ()
    }
    val program = ops.useVariable("before") *> ops.attempt(ops.parAp(left)(right)) *> ops.usedVariables

    assertEquals(program.runToCompletion(firstVars), Right(Set("before")))
    assertEquals(observed, Set("before"))
  }

  test("cursor and cycle scopes preserve variable usage") {
    val inner = ops.cycleOver("scope", ops.cursorOver(Cursor.empty.field("scope"), ops.useVariable("inside") *> ops.getVariables))
    val program = inner *> ops.usedVariables

    assertEquals(program.runToCompletion(firstVars), Right(Set("inside")))
  }

  test("deep repeated demands with distinct variable usage complete once and remain stack safe") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.pure(()): Alg[Unit, Unit]) { (acc, index) =>
      acc *> ops.getVariables *> ops.useVariable(index.toString)
    } *> ops.usedVariables
    val staged = Alg.eval(program)
    val expected = (0 until depth).map(_.toString).toSet

    assertEquals(staged.runToCompletion(firstVars), Right(expected))
    assertEquals(staged.runToCompletion(secondVars), Right(expected))
  }

  test("deep parallel demands retain distinct variable usage") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.pure(()): Alg[Unit, Unit]) { (acc, index) =>
      ops.parAp(acc)((ops.getVariables *> ops.useVariable(index.toString)).as((_: Unit) => ()))
    } *> ops.usedVariables
    val expected = (0 until depth).map(_.toString).toSet

    assertEquals(Alg.eval(program).runToCompletion(firstVars), Right(expected))
  }

  test("deep right-associated static usage remains stack safe") {
    val depth = 20000
    def program(remaining: Int): Alg[Unit, Unit] =
      if (remaining == 0) ops.unit
      else ops.useVariable(remaining.toString).flatMap(_ => program(remaining - 1))

    val expected = (1 to depth).map(_.toString).toSet
    val staged = Alg.eval(program(depth) *> ops.usedVariables)

    assertEquals(staged.runToCompletion(firstVars), Right(expected))
    assertEquals(staged.runToCompletion(secondVars), Right(expected))
  }

  test("cached staging reuses independent static work and rebinds dynamic work") {
    var staticRuns = 0
    var dynamicRuns = 0
    val static = ops.nextId.flatMap { id =>
      staticRuns += 1
      ops.pure(id)
    }
    val program = (ops.getVariables, static).parTupled.flatMap { case (variables, staticId) =>
      dynamicRuns += 1
      ops.nextId.map(dynamicId => (variables, staticId, dynamicId))
    }

    val staged = Alg.eval(program)
    assertEquals(staticRuns, 1)
    assertEquals(dynamicRuns, 0)

    val first = staged.runToCompletion(firstVars).fold(errors => fail(errors.toString), identity)
    val second = staged.runToCompletion(secondVars).fold(errors => fail(errors.toString), identity)
    assertEquals(first._1, firstVars)
    assertEquals(second._1, secondVars)
    assert(first._2 eq second._2)
    assert(first._3 ne second._3)
    assertEquals(staticRuns, 1)
    assertEquals(dynamicRuns, 2)
  }

  test("one resume completes recursive variable demands") {
    val program = ops.getVariables.flatMap(first => ops.getVariables.map(second => (first, second)))
    val run = Alg.eval(program) match {
      case Alg.Staged.Deferred(cont) => cont
      case _                         => fail("Expected initial variable demand")
    }

    val first: Alg.Staged.Done[Unit, (VariableMap[Unit], VariableMap[Unit])] = run(firstVars, Set.empty).value
    val second: Alg.Staged.Done[Unit, (VariableMap[Unit], VariableMap[Unit])] = run(secondVars, Set.empty).value
    assertEquals(first.result, Right((firstVars, firstVars)))
    assertEquals(second.result, Right((secondVars, secondVars)))
  }

  test("force preserves nested deferrals and resume completes them") {
    val forced = Alg.eval(ops.force(ops.getVariables))
    val nested = forced match {
      case Alg.Staged.Done(Right(value), _) => value
      case _                                => fail("Force must return staged value before binding variables")
    }

    assert(nested.isInstanceOf[Alg.Staged.Deferred[?, ?]])
    val resumed = Alg.eval(ops.resume(nested))
    assertEquals(resumed.runToCompletion(firstVars), Right(firstVars))
    assertEquals(resumed.runToCompletion(secondVars), Right(secondVars))
  }

  test("force captures errors until resume") {
    val nested = Alg.eval(ops.force(ops.raiseError(staticError))) match {
      case Alg.Staged.Done(Right(value), _) => value
      case _                                => fail("Force must capture staged errors")
    }

    assertEquals(nested.runToCompletion(firstVars), Left(NonEmptyChain.one(staticError)))
    assertEquals(ops.resume(nested).runToCompletion(firstVars), Left(NonEmptyChain.one(staticError)))
  }

  test("parallel application accumulates static and deferred errors") {
    val static: Alg[Unit, Int] = ops.raiseError(staticError)
    val dynamic: Alg[Unit, Int => Int] = ops.getVariables.flatMap(_ => ops.raiseError(dynamicError))
    val staged = Alg.eval(ops.parAp(static)(dynamic))

    assertEquals(staged.runToCompletion(firstVars), Left(NonEmptyChain.of(staticError, dynamicError)))
    assertEquals(staged.runToCompletion(secondVars), Left(NonEmptyChain.of(staticError, dynamicError)))
  }

  test("parallel application accumulates deferred and static errors") {
    val dynamic: Alg[Unit, Int] = ops.getVariables.flatMap(_ => ops.raiseError(dynamicError))
    val static: Alg[Unit, Int => Int] = ops.raiseError(staticError)

    assertEquals(
      Alg.eval(ops.parAp(dynamic)(static)).runToCompletion(firstVars),
      Left(NonEmptyChain.of(dynamicError, staticError))
    )
  }

  test("parallel application accumulates errors from recursively deferred branches") {
    val left: Alg[Unit, Int] = ops.getVariables.flatMap(_ => ops.getVariables.flatMap(_ => ops.raiseError(staticError)))
    val right: Alg[Unit, Int => Int] = ops.getVariables.flatMap(_ => ops.getVariables.flatMap(_ => ops.raiseError(dynamicError)))

    assertEquals(
      Alg.eval(ops.parAp(left)(right)).runToCompletion(firstVars),
      Left(NonEmptyChain.of(staticError, dynamicError))
    )
  }

  test("attempt captures errors across recursive deferrals") {
    val program: Alg[Unit, Int] = ops.getVariables.flatMap(_ => ops.getVariables.flatMap(_ => ops.raiseError(dynamicError)))
    val staged = Alg.eval(ops.attempt(program))

    assertEquals(staged.runToCompletion(firstVars), Right(Left(NonEmptyChain.one(dynamicError))))
    assertEquals(staged.runToCompletion(secondVars), Right(Left(NonEmptyChain.one(dynamicError))))
  }

  test("attempt recovery does not capture errors outside its scope") {
    val program = ops.attempt(ops.getVariables.flatMap(_ => ops.raiseError(dynamicError))).flatMap {
      case Left(errors) =>
        assertEquals(errors, NonEmptyChain.one(dynamicError))
        ops.raiseError(staticError)
      case Right(_) => fail("Expected deferred error")
    }

    assertEquals(Alg.eval(program).runToCompletion(firstVars), Left(NonEmptyChain.one(staticError)))
  }

  test("pause preserves cursor and cycle scope when resumed outside their boundaries") {
    val cursor = Cursor.empty.field("outer").index(3)
    val nested = ops.cycleOver(
      "outer",
      ops.cursorOver(
        cursor,
        ops.pause(for {
          _ <- ops.getVariables
          _ <- ops.getVariables
          position <- ops.cursorAsk
          cycles <- ops.cycleAsk
        } yield (position, cycles))
      )
    )
    val program = for {
      paused <- nested
      inside <- paused
      outsideCursor <- ops.cursorAsk
      outsideCycles <- ops.cycleAsk
    } yield (inside, outsideCursor, outsideCycles)

    assertEquals(
      Alg.eval(program).runToCompletion(firstVars),
      Right(((cursor, Set("outer")), Cursor.empty, Set.empty[String]))
    )
  }

  test("deep static flatmaps are stack safe") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.pure(0): Alg[Unit, Int]) { (acc, _) =>
      acc.flatMap(n => ops.pure(n + 1))
    }

    assertEquals(Alg.eval(program), Alg.Staged.Done[Unit, Int](Right(depth)))
  }

  test("deep variable-dependent flatmaps are stack safe") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.getVariables.as(0)) { (acc, _) =>
      acc.flatMap(n => ops.pure(n + 1))
    }
    val staged = Alg.eval(program)

    assertEquals(staged.runToCompletion(firstVars), Right(depth))
    assertEquals(staged.runToCompletion(secondVars), Right(depth))
  }

  test("deep recursive variable demands are stack safe") {
    val depth = 20000
    def program(remaining: Int): Alg[Unit, Int] =
      if (remaining == 0) ops.pure(depth)
      else ops.getVariables.flatMap(_ => program(remaining - 1))

    val staged = Alg.eval(program(depth))
    assertEquals(staged.runToCompletion(firstVars), Right(depth))
    assertEquals(staged.runToCompletion(secondVars), Right(depth))
  }

  test("one resume completes deep left-associated repeated variable demands") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.pure(0): Alg[Unit, Int]) { (acc, _) =>
      acc.flatMap(n => ops.getVariables.as(n + 1))
    }
    val run = Alg.eval(program) match {
      case Alg.Staged.Deferred(cont) => cont
      case _                         => fail("Expected variable demand")
    }

    val first: Alg.Staged.Done[Unit, Int] = run(firstVars, Set.empty).value
    val second: Alg.Staged.Done[Unit, Int] = run(secondVars, Set.empty).value
    assertEquals(first.result, Right(depth))
    assertEquals(second.result, Right(depth))
  }

  test("one resume completes deep parallel variable demands") {
    val depth = 20000
    val program = (0 until depth).foldLeft(ops.pure(0): Alg[Unit, Int]) { (acc, _) =>
      ops.parAp(acc)(ops.getVariables.as((n: Int) => n + 1))
    }
    val run = Alg.eval(program) match {
      case Alg.Staged.Deferred(cont) => cont
      case _                         => fail("Expected variable demand")
    }

    val first: Alg.Staged.Done[Unit, Int] = run(firstVars, Set.empty).value
    val second: Alg.Staged.Done[Unit, Int] = run(secondVars, Set.empty).value
    assertEquals(first.result, Right(depth))
    assertEquals(second.result, Right(depth))
  }

  test("one resume completes deep attempts around deferred errors") {
    val depth = 20000
    val attempted = ops.attempt(ops.getVariables.flatMap(_ => ops.raiseError(dynamicError)))
    val program = (0 until depth).foldLeft(attempted: Alg[Unit, EitherNec[PositionalError[Unit], Nothing]]) { (acc, _) =>
      ops.attempt(acc).map(_.flatten)
    }
    val run = Alg.eval(program) match {
      case Alg.Staged.Deferred(cont) => cont
      case _                         => fail("Expected variable demand")
    }

    val first: Alg.Staged.Done[Unit, EitherNec[PositionalError[Unit], Nothing]] = run(firstVars, Set.empty).value
    val second: Alg.Staged.Done[Unit, EitherNec[PositionalError[Unit], Nothing]] = run(secondVars, Set.empty).value
    assertEquals(first.result, Right(Left(NonEmptyChain.one(dynamicError))))
    assertEquals(second.result, Right(Left(NonEmptyChain.one(dynamicError))))
  }
}
