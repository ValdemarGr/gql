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
        case pl if pl.isInstanceOf[PreparedList[F, Any]] =>
          val pl2 = pl.asInstanceOf[PreparedList[F, Any]]
          input.asInstanceOf[Vector[Any]].traverse(i => interpretPrep[F](i, pl2.of)).map(_.reduceLeft(_ deepMerge _).asJson)
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
        case PreparedFragField(_, specify, Selection(fields)) =>
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

    final case class Converted(idsContained: Map[Int, List[Converted]])

    def runWithPlan[F[_]](rootInput: Any, rootSel: NonEmptyList[PreparedField[F, Any]], plan: NonEmptyList[Optimizer.Node])(implicit
        F: Concurrent[F]
    ): F[Map[Int, Any]] = {
      Supervisor[F].use { sup =>
        val flat = Optimizer.flattenNodeTree(plan)

        val nodeMap = flat.map(n => n.id -> n).toList.toMap

        def unpackPrep(p: Prepared[F, Any]): List[(Int, PreparedField[F, Any])] = {
          p match {
            case PreparedLeaf(_, _) => Nil
            case Selection(fields)  => unpackSel(fields).toList
            case pl if pl.isInstanceOf[PreparedList[F, Any]] =>
              unpackPrep(pl.asInstanceOf[PreparedList[F, Any]].of)
          }
        }

        def unpackSel(sel: NonEmptyList[PreparedField[F, Any]]): NonEmptyList[(Int, PreparedField[F, Any])] =
          sel.flatMap { s =>
            NonEmptyList(
              s.id -> s,
              s match {
                case PreparedDataField(_, _, _, selection, _) => unpackPrep(selection)
                case PreparedFragField(_, _, selection)       => unpackSel(selection.fields).toList
              }
            )
          }

        val executionPlanMapping: Map[Int, PreparedField[F, Any]] = unpackSel(rootSel).toList.toMap

        // the algorithm starts from the bottom of the tree and works it way up merging adjacent nodes
        // of same batch name
        //
        // the child batch is saved as a mapping from id to converted child
        //
        // the parent can then use this id to re-associate itself with it's child implementations

        // we start in the bottom of the tree and work our way up to construct the batch tree
        // first we group by same end, and sort by end decreasing such that every group of same end can
        // be batched, given that they have the same batch name also
        val batchMap: Map[Int, Converted] =
          flat
            .groupBy(_.end)
            .toList
            .sortBy { case (k, _) => k }
            .reverse
            // now we fold over the ordered tree, accumulationg a mapping from node id to a converted node structure
            // the converted node structure will only contain a mapping from batch id to children
            .foldLeft(Map.empty[Int, Converted]) { case (accum, (_, grp)) =>
              accum ++
                grp
                  .groupBy(_.name)
                  .flatMap { case (nodeType, nodes) =>
                    val batch = Converted(nodes.map(n => n.id -> n.children.map(child => accum(child.id))).toList.toMap)
                    nodes.map(n => n.id -> batch).toList.toMap
                  }

            }

        val out =
          flat
            .traverse(n => F.deferred[Any].map(n.id -> _))
            .map(_.toList.toMap)
            .flatMap { inputs =>
              def completeNodeResult(node: Optimizer.Node, result: Any): F[Unit] =
                node.children.traverse(child => inputs(child.id).complete(result)).void

              def run(id: Int, input: Any): F[Any] =
                executionPlanMapping(id) match {
                  case PreparedDataField(_, _, resolve, selection, _) =>
                    val fa = resolve(input) match {
                      case Output.Fields.PureResolution(value)  => F.pure(value)
                      case Output.Fields.DeferredResolution(fb) => fb
                    }

                    selection match {
                      case PreparedLeaf(_, _)                          => fa
                      case Selection(_)                                => fa
                      case pl if pl.isInstanceOf[PreparedList[F, Any]] =>
                        // do list stuff
                        fa.map(_.asInstanceOf[Vector[Any]])
                    }
                  case PreparedFragField(_, specify, selection) => F.pure(specify(input))
                }

              def awaitBatch(c: Converted): F[Unit] =
                c.idsContained.keySet.toList
                  .parTraverse(id => inputs(id).get.flatMap(input => run(id, input)).map(result => id -> result))
                  .flatMap(_.parTraverse { case (id, result) => completeNodeResult(nodeMap(id), result) })
                  .void

              val awaitGraphDone = inputs.values.toList.traverse_(_.get)

              val executeAllNodes = batchMap.values.toList.traverse_(awaitBatch)

              val startRoot = plan.parTraverse_(n => inputs(n.id).complete(rootInput)).void

              (awaitGraphDone, executeAllNodes, startRoot).parTupled.void >>
                inputs.toList.parTraverse { case (k, v) => v.get.map(k -> _) }.map(_.toMap)
            }

        out
      }
    }
  }
}
