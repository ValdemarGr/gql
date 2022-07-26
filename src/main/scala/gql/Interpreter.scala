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
        case PreparedDataField(name, resolve, selection, _) =>
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
     * Let the following digraph be the plan where [N] is a batch of nodes of type N:
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
     */
    def runWithPlan[F[_]](input: Any, s: NonEmptyList[PreparedField[F, Any]], plan: NonEmptyList[Optimizer.Node])(implicit F: Concurrent[F]) = {
      Supervisor[F].use{ sup =>
        ???
      }
    }
  }
}
