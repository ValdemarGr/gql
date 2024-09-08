/*
 * Copyright 2024 Valdemar Grange
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
package gql.server.interpreter

import cats._
import io.circe.syntax._
import cats.effect._
import cats.implicits._
import cats.data._
import io.circe._
import gql.server.planner._
import gql._
import gql.preparation._
import fs2.Stream
import scala.concurrent.duration.FiniteDuration
import cats.arrow.FunctionK

/** The [[StreamInterpreter]] is resposible for:
  *   - Wireing together results for a query.
  *   - Handling incoming asynchronous events.
  *   - Handling resource lifetimes.
  *
  * For actual query excution, take a look at [[QueryInterpreter]].
  */
trait StreamInterpreter[F[_]] {
  import StreamInterpreter._

  def interpretStream[A](
      root: A,
      selection: Selection[F, A],
      takeOne: Boolean = false,
      throttle: F ~> F = FunctionK.id[F]
  ): Stream[F, Result]

  def interpretSync[A](
      root: A,
      selection: Selection[F, A],
      throttle: F ~> F = FunctionK.id[F]
  ): F[Result]
}

object StreamInterpreter {
  final case class Result(
      errors: Chain[EvalFailure],
      data: JsonObject
  ) {
    val asQueryResult = QueryResult(data, errors.flatMap(_.asResult))
  }

  def apply[F[_]: Statistics: Planner](
      schemaState: SchemaState[F],
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  )(implicit F: Async[F]): StreamInterpreter[F] = new StreamInterpreter[F] {
    override def interpretSync[A](root: A, selection: Selection[F, A], throttle: F ~> F): F[Result] =
      interpretStream(root, selection, takeOne = true, throttle).take(1).compile.lastOrError

    override def interpretStream[A](
        root: A,
        selection: Selection[F, A],
        takeOne: Boolean = false,
        throttle: F ~> F = FunctionK.id[F]
    ): Stream[F, Result] = Stream.eval(EvalState.init[F]).flatMap { state =>
      def doRound: F[(Resource[F, List[EvalState.Entry[F, ?]]], F[Unit])] = {
        state.ps.produced >>
          accumulate.traverse_(F.sleep(_)) >>
          EvalState.ProduceConsume.make[F].flatMap { ps2 =>
            state.ref.modify { s =>
              val allValues = s.values.getOrElse(Nil)
              val updatedKeys = allValues.map(_.token).toSet
              val relevant = allValues.filter(_.a.parentLeases.intersect(updatedKeys).isEmpty)
              val ts: Resource[F, List[EvalState.Entry[F, _]]] =
                relevant.traverse(e => e.a.parentLease.map(_.as(e))).map(_.flatten)
              (EvalState[F](None, ps2), (ts, s.ps.notifyConsumed))
            }
          }
      }

      val interpreter = QueryInterpreter[F](schemaState, state.ref, throttle)

      val initial = QueryInterpreter.Input.root(root, selection)

      val changelog: Stream[F, (List[(Cursor, Json)], Chain[EvalFailure])] = Stream.repeatEval {
        doRound.flatMap { case (values, notifyConsumed) =>
          values.use { xs =>
            val inputs = xs.map { case (entr: EvalState.Entry[F, a]) =>
              QueryInterpreter.Input[F, a](entr.cont, entr.a)
            }

            val evalled: F[(List[(Cursor, Json)], Chain[EvalFailure])] =
              debug(s"interpreting for ${inputs.size} inputs") >>
                inputs.toNel
                  .traverse(interpreter.interpretAll)
                  .map {
                    // Okay there were no inputs (toNel), just emit what we have
                    case None => (Nil, Chain.empty)
                    // Okay so an evaluation happened
                    case Some(res) => (res.data.toList, res.errors)
                  } <* debug("done interpreting")

            evalled <* notifyConsumed
          }
        }
      }

      Stream.eval(interpreter.interpretAll(NonEmptyList.one(initial))).flatMap { res =>
        val jo: JsonObject = res.data.map { case (_, j) => j }.reduceLeft(_ deepMerge _).asObject.get

        Stream.emit(Result(res.errors, jo)) ++
          changelog
            .evalMapAccumulate(jo) { case (prevJo, (jsons, errs)) =>
              // Patch the previously emitted json data
              val stitched = jsons.foldLeftM(prevJo) { case (accum, (pos, patch)) =>
                Temporal[F]
                  .fromEither {
                    stitchInto(accum.asJson, patch, pos, Cursor.empty).value.value
                  }
                  .map(_.asObject.get)
              }

              stitched.map(o => (o, Result(errs, o)))
            }
            .map { case (_, x) => x }
      }
    }
  }

  final case class StitchFailure(
      currentPath: Cursor,
      remainingPath: Cursor,
      msg: String
  ) extends Exception(msg) {
    override def toString = s"StitchFailure($currentPath, $remainingPath, $msg)"

    override def getMessage(): String = toString
  }
  def stitchInto(oldTree: Json, subTree: Json, remainingPath: Cursor, currentPath: Cursor): EitherT[Eval, StitchFailure, Json] = {
    remainingPath.uncons match {
      case None => EitherT.pure(subTree)
      case Some((p, tl)) =>
        val newCurrent = currentPath.add(p)
        def err(msg: String) = StitchFailure(newCurrent, tl, msg)
        p match {
          case GraphArc.Field(name) =>
            for {
              oldObj <-
                EitherT.fromOption[Eval](oldTree.asObject, err(s"Expected object at $name, but got ${oldTree.name}"))
              oldValue <-
                EitherT.fromOption[Eval](oldObj(name), err(s"Expected field $name in object, but found nothing"))
              newSubTree <- stitchInto(oldValue, subTree, tl, newCurrent)
            } yield oldObj.add(name, newSubTree).asJson
          case GraphArc.Index(index) =>
            for {
              oldArr <-
                EitherT.fromOption[Eval](oldTree.asArray, err(s"expected array at, but found ${oldTree.name}"))
              atIndex <-
                EitherT.fromOption[Eval](oldArr.get(index.toLong), err(s"expected array at $index, but found nothing"))
              newSubTree <- stitchInto(atIndex, subTree, tl, newCurrent)
            } yield oldArr.updated(index, newSubTree).asJson
        }
    }
  }
}
