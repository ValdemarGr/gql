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
package gql.std

import cats.Eval
import cats.data.EitherNec
import cats.effect.kernel.Unique
import cats.implicits._
import gql.parser.Type
import gql.preparation.{Alg, Variable, VariableMap}
import io.circe.Json
import munit.FunSuite

class LazyTTest extends FunSuite {
  private type Result[A] = EitherNec[String, A]

  List("ordinary", "parallel").foreach { mode =>
    val app = mode match {
      case "ordinary" => LazyT.applicativeForApplicativeLazyT[Result, Unit]
      case _          => LazyT.applicativeForParallelLazyT[Result, Unit]
    }

    test(s"$mode pure does not construct metadata") {
      val value = app.pure(42)
      assertEquals(value.fb.map(_.isLeft), Right(true))
      assertEquals(value.runWithValue(_ => fail("metadata must not be constructed")), Right(42))
    }

    for {
      dependentFunction <- List(false, true)
      dependentArgument <- List(false, true)
    } test(s"$mode application: dependent function=$dependentFunction, argument=$dependentArgument") {
      var applications = 0
      val function: Int => Int = value => {
        applications += 1
        value + 2
      }
      val ff =
        if (dependentFunction) LazyT.lift[Result, Unit, Int => Int](_ => function)
        else LazyT.liftF[Result, Unit, Int => Int](Right(function))
      val fa =
        if (dependentArgument) LazyT.lift[Result, Unit, Int](_ => 22)
        else LazyT.liftF[Result, Unit, Int](Right(22))
      val applied = app.ap(ff)(fa)
      val eager = !dependentFunction && !dependentArgument

      assertEquals(applied.fb.map(_.isLeft), Right(eager))
      assertEquals(applications, if (eager) 1 else 0)
      assertEquals(applied.runWithValue(_ => ()), Right(24))
      assertEquals(applications, 1)
    }
  }

  test("runWithBoth constructs metadata from an eager value") {
    var metadataCalls = 0
    val value = LazyT.liftF[Result, String, Int](Right(42))
    assertEquals(
      value.runWithBoth { n =>
        metadataCalls += 1
        n.toString
      },
      Right(("42", 42))
    )
    assertEquals(metadataCalls, 1)
  }

  test("metadata refers to the completed value and is memoized") {
    case class Node(owner: Eval[Owner])
    case class Owner(node: Node)

    var metadataCalls = 0
    val prepared = LazyT.lift[Result, Owner, Node](Node(_))
    val node = prepared
      .runWithValue { node =>
        metadataCalls += 1
        Owner(node)
      }
      .fold(errors => fail(errors.toString), identity)

    val first = node.owner.value
    val second = node.owner.value
    assert(first.node eq node)
    assert(first eq second)
    assertEquals(metadataCalls, 1)
  }

  test("parallel Alg preparation caches an eager composite while its sibling waits for variables") {
    type Prep[A] = Alg[Unit, A]
    case class Composite(left: Unique.Token, right: Unique.Token)

    val ops = Alg.Ops[Unit]
    val app = LazyT.applicativeForParallelLazyT[Prep, Unit]
    var constructions = 0
    val static = app.map2(LazyT.liftF[Prep, Unit, Unique.Token](ops.nextId), LazyT.liftF[Prep, Unit, Unique.Token](ops.nextId)) {
      (left, right) =>
        constructions += 1
        Composite(left, right)
    }
    val dynamic = LazyT.liftF[Prep, Unit, VariableMap[Unit]](ops.getVariables)
    val program = app.product(static, dynamic).runWithValue(_ => fail("metadata must not be constructed"))
    val bind = program.run.fold(errors => fail(errors.toString), identity)
    assertEquals(constructions, 1)

    val firstVars: VariableMap[Unit] = Map("value" -> Variable(Type.Named("Int"), Left(Json.fromInt(1))))
    val secondVars: VariableMap[Unit] = Map("value" -> Variable(Type.Named("Int"), Left(Json.fromInt(2))))
    val first = bind(firstVars).fold(errors => fail(errors.toString), identity)
    val second = bind(secondVars).fold(errors => fail(errors.toString), identity)

    assert(first._1 eq second._1)
    assertEquals(first._2, firstVars)
    assertEquals(second._2, secondVars)
    assertEquals(constructions, 1)
  }
}
