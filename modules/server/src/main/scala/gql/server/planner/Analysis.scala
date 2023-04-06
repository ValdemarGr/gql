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
object Analysis {
  final case class NodeTree(all: List[Node])

  type H[F[_], A] = StateT[F, TraversalState, A]
  def liftStatistics[F[_]: Applicative](stats: Statistics[F]): Statistics[H[F, *]] =
    stats.mapK(StateT.liftK[F, TraversalState])

  def runCostAnalysisFor[F[_]: Monad, A](f: Statistics[H[F, *]] => H[F, A])(implicit stats: Statistics[F]): F[A] =
    f(liftStatistics[F](stats)).runA(TraversalState(1, Set.empty, Chain.empty))

  def runCostAnalysis[F[_]: Monad: Statistics, A](f: Statistics[H[F, *]] => H[F, A]): F[NodeTree] =
    runCostAnalysisFor[F, List[Node]](s => f(s).get.map(_.nodes.toList)).map(NodeTree(_))

  final case class BatchRef[K, V](
      batcherId: gql.resolver.Step.BatchKey[K, V],
      uniqueNodeId: UniqueBatchInstance[K, V]
  )

  final case class NodeId(id: Int) extends AnyVal

  final case class Node(
      id: NodeId,
      name: String,
      cost: Double,
      elemCost: Double,
      parents: Set[NodeId],
      batchId: Option[BatchRef[?, ?]]
  )

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