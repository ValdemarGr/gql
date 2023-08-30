package gql.server.interpreter

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import cats._
import cats.data._
import io.circe._
import gql.server.planner._
import gql._
import gql.preparation._
import cats.effect.std.Supervisor

trait QueryInterpreter[F[_]] {
  import QueryInterpreter._

  def interpretOne[A, B](input: Input[F, A, B], batching: BatchAccumulator[F]): F[Result[F]]

  def interpretAll(inputs: NonEmptyList[Input[F, ?, ?]]): F[Results[F]]
}

object QueryInterpreter {
  final case class Input[F[_], A, B](
      data: IndexedData[F, A],
      cont: StepCont[F, A, B]
  )

  object Input {
    def root[F[_], A](data: A, cont: Prepared[F, A], scope: Scope[F]): Input[F, A, Json] =
      Input(IndexedData(0, EvalNode.empty(data, scope)), StepCont.Done(cont))
  }

  final case class Results[F[_]](errors: Chain[EvalFailure], roots: NonEmptyList[EvalNode[F, Json]])

  final case class Result[F[_]](errors: Chain[EvalFailure], rootData: EvalNode[F, Json])

  def apply[F[_]: Async: Statistics](
      schemaState: SchemaState[F],
      ss: SignalScopes[F, StreamingData[F, ?, ?]]
  )(implicit planner: Planner[F]) =
    new QueryInterpreter[F] {
      def interpretOne[A, B](input: Input[F, A, B], batching: BatchAccumulator[F]): F[Result[F]] =
        Supervisor[F].use { sup =>
          SubqueryInterpreter[F](ss, batching, sup)
            .runEdgeCont(Chain(input.data), input.cont)
            .run
            .map { case (fails, succs) =>
              val (_, j) = succs.headOption.get
              Result(fails, input.data.node.setValue(j))
            }
        }

      def interpretAll(inputs: NonEmptyList[Input[F, ?, ?]]): F[Results[F]] =
        for {
          costTree <- analyzeCost[F](inputs)
          planned <- planner.plan(costTree)
          accumulator <- BatchAccumulator[F](schemaState, planned)
          results <- inputs.parTraverse(interpretOne(_, accumulator))
          batchErrors <- accumulator.getErrors
          allErrors = Chain.fromSeq(results.toList).flatMap(_.errors) ++ Chain.fromSeq(batchErrors)
        } yield Results(allErrors, results.map(_.rootData))
    }

  def analyzeCost[F[_]: Monad: Statistics](inputs: NonEmptyList[Input[F, ?, ?]]): F[NodeTree] =
    Analyzer.analyzeWith[F, Unit] { analyzer =>
      inputs.toList.traverse_ { ri =>
        def contCost(step: StepCont[F, ?, ?]): Analyzer.H[F, Unit] =
          step match {
            case d: StepCont.Done[F, i]           => analyzer.analyzePrepared(d.prep)
            case c: StepCont.Continue[F, ?, ?, ?] => analyzer.analyzeStep(c.step) *> contCost(c.next)
            case StepCont.Join(_, next)           => contCost(next)
            case StepCont.TupleWith(_, next)      => contCost(next)
          }
        contCost(ri.cont)
      }
    }
}
