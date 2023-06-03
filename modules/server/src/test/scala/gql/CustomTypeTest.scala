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
import cats._
import cats.data._
import cats.implicits._

final case class TraversableStructure[A](value1: A, value2: A)

object TraversableStructure {
  implicit val traversableForTraversableStructure: Traverse[TraversableStructure] = new Traverse[TraversableStructure] {
    override def foldLeft[A, B](fa: TraversableStructure[A], b: B)(f: (B, A) => B): B =
      f(f(b, fa.value1), fa.value2)

    override def foldRight[A, B](fa: TraversableStructure[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.value1, f(fa.value2, lb))

    override def traverse[G[_]: Applicative, A, B](fa: TraversableStructure[A])(f: A => G[B]): G[TraversableStructure[B]] =
      (f(fa.value1), f(fa.value2)).mapN(TraversableStructure.apply)
  }
}

class CustomTypeTest extends CatsEffectSuite {
  lazy val schemaShape = SchemaShape.unit[IO](
    fields(
      "list" -> lift(_ => List(1)),
      "nel" -> lift(_ => NonEmptyList.of(1, 2, 3)),
      "nec" -> lift(_ => NonEmptyChain.of(1)),
      "traversable" -> lift(_ => TraversableStructure(1, 2).map(_.toString()))
    )
  )

  test("if it compiles it works") {}
}
