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
import cats.mtl._
import cats._

class BatchPlanningTest extends CatsEffectSuite {
  test("batch planning test") {
    type Effect[B] = WriterT[State[Int, *], List[Node], B]
    val T = Tell[Effect, List[Node]]
    val S = Stateful[Effect, Int]
    def node(cost: Double, parents: Set[NodeId], familyId: Option[Int]): Effect[NodeId] = {
      S.get.flatMap { i =>
        val nid = NodeId(i)
        val node =
          Node(nid, s"$i", cost, cost, parents, familyId.map(j => BatchRef(Step.BatchKey(j), UniqueBatchInstance(NonEmptyList.one(i)))))
        T.tell(List(node)) as nid
      } <* S.modify(_ + 1)
    }

    val g =
      for {
        _ <- node(100, Set.empty, Some(1))
        _ <- node(100, Set.empty, Some(1))
        _ <- node(100, Set.empty, Some(1))
        _ <- node(100, Set.empty, Some(1))
        _ <- node(100, Set.empty, Some(1))
      } yield ()

    val dag = NodeTree(g.written.runA(0).value)

    val opt = Planner[Eval].plan(dag).value

    fail(opt.show(ansiColors = true)): Unit
  }
}
