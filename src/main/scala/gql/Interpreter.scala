package gql

import cats.data._
import PreparedQuery._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import io.circe._
import io.circe.syntax._
import cats.Eval
import cats.Monad
import cats.effect.std.Supervisor
import scala.collection.immutable.SortedMap
import gql.Output.Fields.DeferredResolution
import gql.Output.Fields.PureResolution

object Interpreter {
  object Naive {
    def interpretPrep[F[_]](input: Any, prep: Prepared[F, Any])(implicit F: Async[F]): F[Json] = {
      prep match {
        case Selection(fields) => interpret[F](input, fields).map(_.reduceLeft(_ deepMerge _).asJson)
        case PreparedLeaf(_, encode) =>
          encode(input) match {
            case Left(value)  => F.raiseError(new Exception(value))
            case Right(value) => F.pure(value)
          }
        case PreparedList(of) =>
          val inputLst = input.asInstanceOf[Vector[Any]]
          inputLst.traverse(i => interpretPrep[F](i, of)).map(_.reduceLeft(_ deepMerge _).asJson)
      }
    }

    def interpretField[F[_]](input: Any, pf: PreparedField[F, Any])(implicit F: Async[F]): F[JsonObject] = {
      pf match {
        case PreparedDataField(_, name, resolve, selection, _) =>
          val fa = resolve(input) match {
            case Output.Fields.PureResolution(value)  => F.pure(value)
            case Output.Fields.DeferredResolution(fa) => fa
          }

          fa
            .flatMap(i => interpretPrep[F](i, selection))
            .map(x => JsonObject(name -> x))
        case PreparedFragField(specify, Selection(fields)) =>
          specify(input) match {
            case None    => F.pure(JsonObject.empty)
            case Some(i) => interpret(i, fields).map(_.reduceLeft(_ deepMerge _))
          }
      }
    }

    def interpret[F[_]](input: Any, s: NonEmptyList[PreparedField[F, Any]])(implicit F: Async[F]) =
      s.traverse(pf => interpretField[F](input, pf))
  }

  object Planned {
    /*
     * Following a plan is not entirely trivial compared to the naive interpreter.
     *
     * query Q:
     *            A   B
     *            |  / \
     *            C D   E
     *            |     |
     *            D     F
     *            |     |
     *            G     G
     *
     * Let the following digraph be the plan of Q, where [N] is a batch of nodes of type N:
     *
     *            A   B
     *            |   |\
     *            C   | |
     *             \ /  \
     *             [D]   E
     *              |    |
     *              |   _F
     *              | /
     *             [G]
     *
     * Now consider the following different strategies.
     * 1. Consumer based joining.
     * In this strategy the consumer (child) is responsible for joining the results together.
     * One could define a cursor such that the resulting data could be re-associated in a future emission phase.
     * The inital strategy is relatively straightforward:
     *  Give all nodes a set of ids they must await.
     *  Begin running from the root nodes.
     *  When a node is finished, save the result with the cursor and begin the next node:
     *    * If the child not a batch node, start it immidiately in the background (spawn a fiber).
     *
     *    * If the child is a batch node,
     *      is has an atomic reference allocated to it to keep track of what parent cursors to await.
     *      Modify the reference to add the cursor result,
     *      if the added cursor result is the final one, start the child in a new fiber.
     *
     * This algorithm does, however, not handle lists or options well, that is, nodes where the actual number of
     * values is either 0 or larger than 1.
     * If we only let a cursor be "done" when all of it's results are done (that is, it has produced an output for every input).
     * Furthermore we must also handle the task of re-associating the results.
     * For instance, maybe the first three inputs and results of [D], come from C, but the remaining four come from B.
     * The three inputs from C need to be associated with their results and cursors (A -> C).
     *
     * When C is done:
     * DState.submit(dInputFromC.map(i => (i, CCursor)))
     * When B is done:
     * DState.submit(dInputFromB.map(i => (i, BCursor)))
     * Since both C and B are the parent cursors, and they are done, we can start D.
     *
     * Unfortunately some abmiguity occurs when having nodes that emit lists under nodes that emit lists,
     * since reassociation becames impossible; in the sublist, what element is associated with what parent list element?
     *
     * Let an example cursor be:
     * (A -> C -> D -> G)
     * Now let there be three children D to C and two children G to D.
     * Which of the three children D should the two G's be allocated to?
     * We must track what list indices causes what children to occur.
     *
     * Let cursors be fields (edges) in practice.
     * We now track the traversed fields.
     * On lists, cursors will be list indices.
     *
     */
    final case class NodeInput(
        cursor: List[String],
        input: Any
    )

    final case class NodeState(
        remainingParents: Int,
        accumulatedInput: List[NodeInput]
    )

    final case class PlanNode[F[_]](
        children: List[PlanNode[F]],
        state: Ref[F, NodeState],
        eval: Any => F[Any]
    )

    final case class A(
        children: List[A]
    )

    // no children, has c and i as parents
    val d = A(List(A(Nil)))

    // d as child, has i as parents
    val c = A(List(A(Nil), d))

    // branch out to c and d, has no parents
    val i = A(List(A(Nil), c, d))

    /*
     *               A__
     *              / \ \
     *             A  A  A
     *                 \/ \
     *                [A]  A
     */

    final case class Converted(batchId: String, idsContained: Map[Int, List[Converted]])

    final case class BatchExecutionState(
        remainingInputs: Set[Int],
        inputMap: Map[Int, List[Any]]
    )

    sealed trait StateSubmissionOutcome
    final case class FinalSubmission(accumulatedInputs: Map[Int, List[Any]]) extends StateSubmissionOutcome
    case object NoState extends StateSubmissionOutcome
    case object NotFinalSubmission extends StateSubmissionOutcome

    def run[F[_]](
        rootInput: Any,
        rootSel: NonEmptyList[PreparedField[F, Any]],
        plan: NonEmptyList[Optimizer.Node]
    )(implicit F: Concurrent[F]) = {
      val flat = Optimizer.flattenNodeTree(plan)

      def unpackPrep(prep: Prepared[F, Any]): List[(Int, PreparedDataField[F, Any, Any])] =
        prep match {
          case PreparedLeaf(_, _) => Nil
          case PreparedList(of)   => unpackPrep(of)
          case Selection(fields)  => flattenDataFieldMap(fields).toList
        }

      def flattenDataFieldMap(sel: NonEmptyList[PreparedField[F, Any]]): NonEmptyList[(Int, PreparedDataField[F, Any, Any])] =
        sel.flatMap { pf =>
          pf match {
            case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
              val hd = id -> df
              val tl = unpackPrep(selection)
              NonEmptyList(hd, tl)
            case PreparedFragField(_, selection) => flattenDataFieldMap(selection.fields)
          }
        }

      val dataFieldMap = flattenDataFieldMap(rootSel).toList.toMap

      val batches: Map[Int, (String, NonEmptyList[Optimizer.Node])] =
        flat
          .groupBy(_.end)
          .toList
          .sortBy { case (k, _) => k }
          .zipWithIndex
          .flatMap { case ((_, group), idx) =>
            group
              .groupBy(_.name)
              .filter { case (_, nodes) => nodes.size > 1 }
              .toList
              .flatMap { case (nodeType, nodes) =>
                val thisBatch = (s"$nodeType-$idx", nodes)
                nodes.map(n => n.id -> thisBatch).toList
              }
          }
          .toMap

      val batchStateMapF: F[Map[String, Ref[F, BatchExecutionState]]] =
        batches.values.toList
          .distinctBy { case (k, _) => k }
          .traverse { case (k, nodes) =>
            F.ref(
              BatchExecutionState(
                nodes.map(_.id).toList.toSet,
                Map.empty[Int, List[Any]]
              )
            ).map(k -> _)
          }
          .map(_.toMap)

      batchStateMapF.flatMap { batchStateMap =>
        def getBatchState(nodeId: Int): Option[Ref[F, BatchExecutionState]] =
          batches.get(nodeId).flatMap { case (k, _) => batchStateMap.get(k) }

        def submitAndMaybeStart(nodeId: Int, input: List[Any]): F[StateSubmissionOutcome] =
          getBatchState(nodeId)
            .traverse(_.modify { s =>
              val newSet = s.remainingInputs - nodeId
              val newMap = s.inputMap + (nodeId -> input)
              val newState = BatchExecutionState(newSet, newMap)
              (newState, if (newSet.isEmpty) FinalSubmission(newMap) else NotFinalSubmission)
            })
            .map(_.getOrElse(NoState))

        def collapseInputs(df: PreparedDataField[F, Any, Any], inputs: List[Any]): F[List[Any]] =
          inputs
            .map(df.resolve)
            .traverse {
              case DeferredResolution(fa) => fa
              case PureResolution(value)  => F.pure(value)
            }

        def startNext(df: PreparedDataField[F, Any, Any], outputs: List[Any]): F[Unit] = {
          def evalSel(s: Prepared[F, Any], in: List[Any]): F[Unit] =
            s match {
              case PreparedLeaf(_, _) => F.unit
              case Selection(fields)  => go(fields, in)
              case PreparedList(of) =>
                evalSel(of, in.flatMap(_.asInstanceOf[Vector[Any]].toList))
            }

          evalSel(df.selection, outputs)
        }

        def go(sel: NonEmptyList[PreparedField[F, Any]], input: List[Any]): F[Unit] =
          // fork each field
          sel.parTraverse { pf =>
            pf match {
              case PreparedFragField(specify, selection) =>
                go(selection.fields, input.flatMap(x => specify(x).toList))
              case df @ PreparedDataField(id, name, resolve, selection, batchName) =>
                // maybe join
                submitAndMaybeStart(id, input).flatMap {
                  // No batching state, so just start
                  // TODO return the results with a cursor
                  case NoState => collapseInputs(df, input).flatMap(out => startNext(df, out))
                  // join
                  // There is a batch state, but we didn't add the final input
                  // Stop here, someone else will start the batch
                  case NotFinalSubmission => F.unit
                  // join
                  // We are the final submitter, start the computation
                  case FinalSubmission(inputs) =>
                    // outer list are seperate node ids, inner is is the list of results for that node
                    val outputsF: F[List[(PreparedDataField[F, Any, Any], List[Any])]] =
                      inputs.toList
                        .map { case (k, v) => dataFieldMap(k) -> v }
                        .traverse { case (df, v) => collapseInputs(df, v).map(df -> _) }

                    // fork each node's continuation
                    // in parallel, start ever node's computation
                    outputsF.flatMap(_.parTraverse { case (df, outputs) => startNext(df, outputs) }).void
                }
            }
          }.void

        go(rootSel, List(rootInput))
      }
    }
  }
}
