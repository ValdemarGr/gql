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

import munit.CatsEffectSuite
import cats.data._
import gql.server.planner._
import cats.implicits._
import gql.resolver.Step
import gql.preparation.UniqueBatchInstance
import cats.effect._
import cats.mtl._
import cats._
import gql.server.interpreter.BatchAccumulator
import cats.arrow.FunctionK

class BatchPlanningTest extends CatsEffectSuite {
  test("batch planning test") {
    type Effect[B] = WriterT[State[Int, *], List[Node], B]
    val T = Tell[Effect, List[Node]]
    val S = Stateful[Effect, Int]
    def node(cost: Double, parents: Set[NodeId], familyId: Option[Int]): Effect[NodeId] = {
      S.get.flatMap { i =>
        val nid = NodeId(i)
        val n = Node(
          nid,
          s"$i",
          cost,
          cost,
          parents,
          familyId.map(j => BatchRef(Step.BatchKey(j), UniqueBatchInstance(NonEmptyList.one(i))))
        )
        T.tell(List(n)) as nid
      } <* S.modify(_ + 1)
    }

    val roots = List.fill(5)(0).zipWithIndex.map { case (_, i) => i }

    val g = roots.foldMap { i =>
      val nodes = node(100, Set.empty, Some(1)).written.runA(0).value
      NodeTree(nodes).alpha(i)
    }

    val ss = SchemaState(
      0,
      roots.map(i => Step.BatchKey(i) -> SchemaState.BatchFunction[IO, String, String](_ => IO(Map.empty))).toMap,
      Nil
    )

    val opt = Planner[Eval].plan(g).value

    assert(clue(opt).plan.values.toList.toSet.size == 1)

    Statistics[IO].flatMap { implicit stats =>
      BatchAccumulator[IO](ss, opt, FunctionK.id[IO]).flatMap { ba =>
        roots.traverse { r =>
          ba.alpha(r).submit(UniqueBatchInstance(NonEmptyList.one(0)), Chain.empty)
        }
      }
    }
  }
}
