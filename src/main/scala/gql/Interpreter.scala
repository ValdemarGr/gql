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

    def run[F[_]](rootInput: Any, rootSel: NonEmptyList[PreparedField[F, Any]], plan: NonEmptyList[Optimizer.Node])(implicit
        F: Concurrent[F]
    ) = {
      Supervisor[F].use { sup =>
        val flat = Optimizer.flattenNodeTree(plan)

        // val nodeMap = flat.map(n => n.id -> n).toList.toMap

        /*
         * the algorithm starts from the bottom of the tree and works it way up merging adjacent nodes
         * of same batch name
         *
         * the child batch is saved as a mapping from id to converted child
         *
         * the parent can then use this id to re-associate itself with it's children
         *
         * Let the following plan (tree) be an exmaple:
         *               A__
         *              / \ \
         *             B  C  D
         *               /  / \
         *              E1 E2  F
         *
         * We start from the bottom:
         * Round 1:
         *  since end(E1) == end(E2), let assoc(id(E1)) = { id(E1), id(E2) }
         *  F has no adjacent nodes with similar end, so assoc(id(F)) = { id(F) }
         *
         * Round 2:
         *  B has no adjacent nodes with similar end, so assoc(id(B)) = { id(B) }
         *  D has no adjacent nodes with similar end, so assoc(id(D)) = { id(D) }, but
         *  D has a two children E2 and F, so children(D) = assoc(id(E2)) \cup assoc(id(F)) =
         *
         */

        // we start in the bottom of the tree and work our way up to construct the batch tree
        // first we group by same end, and sort by end decreasing such that every group of same end can
        // be batched, given that they have the same batch-name also
        // val batchMap: Map[Int, Converted] =
        //   flat
        //     .groupBy(_.end)
        //     .toList
        //     .sortBy { case (k, _) => k }
        //     .zipWithIndex
        //     // now we fold over the in-order grouping, accumulationg a mapping from node id to a converted node structure
        //     // the converted node structure will only contain a mapping from batch id to children
        //     .foldLeft(Map.empty[Int, Converted]) { case (accum, ((_, grp), idx)) =>
        //       // idx + name uniquely identifies a batch
        //       accum ++
        //         grp
        //           .groupBy(_.name)
        //           .flatMap { case (nodeType, nodes) =>
        //             val batch = Converted(s"$nodeType-$idx", nodes.map(n => n.id -> n.children.map(child => accum(child.id))).toList.toMap)
        //             nodes.map(n => n.id -> batch).toList.toMap
        //           }
        //     }

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

          def submitAndMaybeStart(nodeId: Int, input: List[Any]): F[Option[Option[Map[Int, List[Any]]]]] =
            getBatchState(nodeId)
              .traverse(_.modify { s =>
                val newSet = s.remainingInputs - nodeId
                val newMap = s.inputMap + (nodeId -> input)
                val newState = BatchExecutionState(newSet, newMap)
                (newState, if (newSet.isEmpty) Some(newMap) else None)
              })

          def go(sel: NonEmptyList[PreparedField[F, Any]], input: List[Any]): F[Unit] =
            sel.parTraverse { pf =>
              pf match {
                case PreparedFragField(specify, selection) =>
                  go(selection.fields, input.flatMap(x => specify(x).toList))
                case PreparedDataField(id, name, resolve, selection, batchName) =>
                  submitAndMaybeStart(id, input).flatMap {
                    // No batching state, so just start
                    case None =>
                      // TODO return the results with a cursor
                      val results =
                        input
                          .map(resolve)
                          .traverse {
                            case DeferredResolution(fa) => fa
                            case PureResolution(value)  => F.pure(value)
                          }

                      results.flatMap { xs =>
                        def evalSel(s: Prepared[F, Any]): F[Unit] =
                          s match {
                            case PreparedLeaf(_, _) => F.unit
                            case Selection(fields)  => go(fields, xs)
                            case PreparedList(of)   => evalSel(of)
                          }

                        evalSel(selection)
                      }
                    // There is a batch state, but we didn't add the final input
                    // Stop here, someone else will start the batch
                    case Some(None) => F.unit
                    // We are the final submitter, start the computation
                    case Some(Some(inputs)) =>
                      // TODO get all input resolvers and do more or less the same as the no batching state,
                      // except with an extra list layer
                      F.unit
                  }
              }
            }.void

          go(rootSel, List(rootInput))
        }

      // def unpackPrep(p: Prepared[F, Any]): List[(Int, PreparedField[F, Any])] =
      //   p match {
      //     case PreparedLeaf(_, _) => Nil
      //     case Selection(fields)  => unpackSel(fields).toList
      //     case PreparedList(of)   => unpackPrep(of)
      //   }

      // def unpackSel(sel: NonEmptyList[PreparedField[F, Any]]): NonEmptyList[(Int, PreparedField[F, Any])] =
      //   sel.flatMap {
      //     case p @ PreparedDataField(id, _, _, sel, _) => NonEmptyList(id -> p, unpackPrep(sel))
      //     case PreparedFragField(_, sel)               => unpackSel(sel.fields)
      //   }

      // val executionPlanMapping: Map[Int, PreparedDataField[F, Any, Any]] =
      //   unpackSel(rootSel).toList.collect { case (k, v: PreparedDataField[F, Any, Any]) => (k, v) }.toMap

      // // rootSel
      // //   .parTraverse { x =>
      // //     x match {
      // //       case PreparedDataField(id, name, resolve, selection, batchName) => ???
      // //       case PreparedFragField(specify, selection)                      => ???
      // //     }
      // //   }

      // val out =
      //   executionPlanMapping.keySet.toList
      //     .traverse(id => F.deferred[List[Any]].map(id -> _))
      //     .map(_.toList.toMap)
      //     .flatMap { inputs =>
      //       def completeNodeResult(node: Optimizer.Node, result: List[Any]): F[Unit] =
      //         node.children.traverse(child => inputs(child.id).complete(result)).void

      //       def run(id: Int, input: Any): F[List[Any]] =
      //         executionPlanMapping(id) match {
      //           case PreparedDataField(_, _, resolve, selection, _) =>
      //             val fa = resolve(input) match {
      //               case Output.Fields.PureResolution(value)  => F.pure(value)
      //               case Output.Fields.DeferredResolution(fb) => fb
      //             }

      //             selection match {
      //               case PreparedLeaf(_, _)                          => fa.map(List(_))
      //               case Selection(_)                                => fa.map(List(_))
      //               case pl if pl.isInstanceOf[PreparedList[F, Any]] =>
      //                 // do list stuff
      //                 fa.map(_.asInstanceOf[Vector[Any]]).map(_.toList)
      //             }
      //         }

      //       def awaitBatch(c: Converted): F[Unit] =
      //         c.idsContained.keySet.toList
      //           .parTraverse(id => inputs(id).get.flatMap(input => input.parFlatTraverse(i => run(id, i))).map(result => id -> result))
      //           .flatMap(_.parTraverse { case (id, result) => completeNodeResult(nodeMap(id), result) })
      //           .void

      //       val awaitGraphDone = inputs.toList.parTraverse_ { case (id, x) =>
      //         println(s"awaiting node $id")
      //         x.get.map(_ => println(s"node done $id"))
      //       }

      //       val executeAllNodes = batchMap.values.toList.parTraverse_ { c =>
      //         println("executing batch")
      //         awaitBatch(c).map(_ => println("batch done"))
      //       }

      //       val startRoot = plan.parTraverse_ { n =>
      //         println(s"starting node ${n.id}")
      //         inputs(n.id).complete(List(rootInput)).map(_ => println(s"node ${n.id} done"))
      //       }.void

      //       (awaitGraphDone, executeAllNodes, startRoot).parTupled.void >>
      //         inputs.toList
      //           .parTraverse { case (k, v) =>
      //             println(s"awaiting result for $k")
      //             v.get
      //               .map(res => { println(s"result for $k: $res"); res })
      //               .map(k -> _)
      //           }
      //           .map(_.toMap)
      //     }

      // out
      }
    }
  }
}
