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
package gql.server.planner

import gql.preparation._
import cats.data._
import cats._
import cats.mtl._
import gql.Statistics
import cats.implicits._

trait Analyzer[F[_]] {
  def analyzeStep[G[_]](step: PreparedStep[G, ?, ?]): F[Unit]

  def analyzeFields[G[_]](prepared: NonEmptyList[PreparedField[G, ?]]): F[Unit]

  def analyzePrepared[G[_]](p: Prepared[G, ?]): F[Unit]

  def analyzeCont[G[_]](edges: PreparedStep[G, ?, ?], cont: Prepared[G, ?]): F[Unit]
}

object Analyzer {
  type H[F[_], A] = StateT[F, TraversalState, A]
  def liftStatistics[F[_]: Applicative](stats: Statistics[F]): Statistics[H[F, *]] =
    stats.mapK(StateT.liftK[F, TraversalState])

  def runCostAnalysisFor[F[_]: Monad, A](f: Statistics[H[F, *]] => H[F, A])(implicit stats: Statistics[F]): F[A] =
    f(liftStatistics[F](stats)).runA(TraversalState(1, Set.empty, Chain.empty))

  def runCostAnalysis[F[_]: Monad: Statistics, A](f: Statistics[H[F, *]] => H[F, A]): F[NodeTree] =
    runCostAnalysisFor[F, List[Node]](s => f(s).get.map(_.nodes.toList)).map(NodeTree(_))

  def analyzeWith[F[_]: Monad: Statistics, A](f: Analyzer[H[F, *]] => H[F, A]): F[NodeTree] =
    runCostAnalysis[F, A](implicit s2 => f(apply))

  final case class TraversalState(
      id: Int,
      parents: Set[NodeId],
      nodes: Chain[Node]
  )

  def apply[F[_]](implicit
      stats: Statistics[F],
      F: Monad[F],
      S: Stateful[F, TraversalState]
  ) = {
    def getId: F[NodeId] =
      S.inspect(x => NodeId(x.id)) <* S.modify(s => s.copy(id = s.id + 1))

    def addNode(node: Node): F[Unit] =
      S.modify(s => s.copy(nodes = s.nodes :+ node))

    new Analyzer[F] {
      def analyzeStep[G[_]](step: PreparedStep[G, ?, ?]): F[Unit] = {
        def goParallel(l: PreparedStep[G, ?, ?], r: PreparedStep[G, ?, ?]): F[Unit] = {
          // A parallel op is disjunctive so the parent must be the same for both branches
          // This creates a diamond shape in the graph
          // Parent -> Left -> Child
          // Parent -> Right -> Child
          S.inspect(_.parents).flatMap { ps =>
            val setParents = S.modify(s => s.copy(parents = ps))
            val left = setParents *> analyzeStep(l) *> S.inspect(_.parents)
            val right = setParents *> analyzeStep(r) *> S.inspect(_.parents)

            (left, right).flatMapN { case (lp, rp) =>
              val parents = lp ++ rp
              S.modify(s => s.copy(parents = parents))
            }
          }
        }

        import PreparedStep._
        step match {
          case Lift(_) | EmbedError() | GetMeta(_) => F.unit
          case Compose(l, r)                       => analyzeStep[G](l) *> analyzeStep[G](r)
          case alg: Choose[G, ?, ?, ?, ?]          => goParallel(alg.fac, alg.fbc)
          case alg: First[G, ?, ?, ?]              => analyzeStep[G](alg.step)
          case Batch(_, _) | EmbedEffect(_) | EmbedStream(_, _) =>
            val name = step match {
              case Batch(id, _)           => s"batch_$id"
              case EmbedEffect(cursor)    => cursor.asString
              case EmbedStream(_, cursor) => cursor.asString
              case _                      => ???
            }

            val costF = stats
              .getStatsOpt(name)
              .map(_.getOrElse(Statistics.Stats(100d, 5d)))

            costF.flatMap { cost =>
              getId.flatMap { id =>
                S.inspect(_.parents).flatMap { parentIds =>
                  addNode {
                    Node(
                      id,
                      name,
                      cost.initialCost,
                      cost.extraElementCost,
                      parentIds,
                      step match {
                        case Batch(batcherId, uniqueNodeId) => Some(BatchRef(batcherId, uniqueNodeId))
                        case _                              => None
                      }
                    )
                  }
                }
              }
            }
        }
      }

      def analyzeFields[G[_]](prepared: NonEmptyList[PreparedField[G, ?]]): F[Unit] =
        prepared.toList.traverse_ { pf =>
          pf match {
            case PreparedDataField(_, _, cont)          => analyzeCont[G](cont.edges, cont.cont)
            case PreparedSpecification(_, _, selection) => analyzeFields[G](selection)
          }
        }

      def analyzePrepared[G[_]](p: Prepared[G, ?]): F[Unit] = p match {
        case PreparedLeaf(_, _)          => F.unit
        case Selection(fields)           => analyzeFields[G](fields)
        case l: PreparedList[G, ?, ?, ?] => analyzeCont[G](l.of.edges, l.of.cont)
        case o: PreparedOption[G, ?, ?]  => analyzeCont[G](o.of.edges, o.of.cont)
      }

      def analyzeCont[G[_]](edges: PreparedStep[G, ?, ?], cont: Prepared[G, ?]): F[Unit] =
        analyzeStep[G](edges) *> analyzePrepared[G](cont)
    }
  }
}
