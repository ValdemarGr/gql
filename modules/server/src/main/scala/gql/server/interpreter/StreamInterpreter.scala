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
package gql.server.interpreter

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

/** The [[StreamInterpreter]] is resposible for:
  *   - Wireing together results for a query.
  *   - Handling incoming asynchronous events.
  *   - Handling resource lifetimes.
  *
  * For actual query excution, take a look at [[QueryInterpreter]].
  */
trait StreamInterpreter[F[_]] {
  import StreamInterpreter._

  def interpretStream[A](root: A, selections: List[PreparedField[F, A]], takeOne: Boolean = false): Stream[F, Result]

  def interpretSync[A](root: A, selections: List[PreparedField[F, A]]): F[Result]
}

object StreamInterpreter {
  final case class Result(
      errors: Chain[EvalFailure],
      data: JsonObject
  ) {
    val asQueryResult = QueryResult(data, errors.flatMap(_.asResult))
  }

  def apply[F[_]: Async: Statistics: Planner](
      schemaState: SchemaState[F],
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  ): StreamInterpreter[F] = fromMakeInterpreter[F](ss => QueryInterpreter[F](schemaState, ss), debug, accumulate)

  def fromMakeInterpreter[F[_]: Temporal](
      makeInterpreter: SignalScopes[F, StreamingData[F, ?, ?]] => QueryInterpreter[F],
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  ): StreamInterpreter[F] = new StreamInterpreter[F] {
    override def interpretSync[A](root: A, selections: List[PreparedField[F, A]]): F[Result] =
      interpretStream(root, selections, takeOne = true).take(1).compile.lastOrError

    override def interpretStream[A](root: A, selections: List[PreparedField[F, A]], takeOne: Boolean): Stream[F, Result] =
      Stream.resource(Scope[F](None)).flatMap { rootScope =>
        Stream
          .eval(SignalScopes[F, StreamingData[F, ?, ?]](takeOne = takeOne, debug, accumulate, rootScope))
          .flatMap { ss =>
            val interpreter = makeInterpreter(ss)

            val changeStream = Stream.repeatEval(ss.unconsRelevantEvents)

            val initial = QueryInterpreter.Input.root(root, Selection(selections), rootScope)

            Stream.eval(interpreter.interpretAll(NonEmptyList.one(initial))).flatMap { res =>
              val jo: JsonObject = res.roots.map(_.value).reduceLeft(_ deepMerge _).asObject.get

              Stream.emit(Result(res.errors, jo)) ++
                changeStream
                  .evalMapAccumulate(jo) { case (prevJo, changes) =>
                    // Now we have prepared the input for this next iteration
                    val preparedRoots =
                      changes.map { case SignalScopes.ResourceInfo(sd: StreamingData[F, a, b], p, s, _) =>
                        val c = p.cursor
                        sd.value match {
                          case Left(ex) =>
                            (Chain(EvalFailure.StreamTailResolution(c, Left(ex))), None, c)
                          case Right(a) =>
                            val sc = sd.edges
                            (
                              Chain.empty,
                              Some(QueryInterpreter.Input[F, a, b](IndexedData(sd.originIndex, EvalNode(c, a, s)), sc)),
                              c
                            )
                        }
                      }

                    // Some of the streams may have emitted errors, we have to insert nulls into the result at those positions
                    val paddedErrors = preparedRoots.toList.mapFilter {
                      case (_, None, c)    => Some((Json.Null, c))
                      case (_, Some(_), _) => None
                    }

                    // These are the inputs that are ready to be evaluated
                    val defined = preparedRoots.collect { case (_, Some(x), c) => (x, c) }

                    val evalled: F[(List[EvalNode[F, Json]], Chain[EvalFailure])] =
                      debug(s"interpreting for ${defined.size} inputs") >>
                        defined
                          .map { case (x, _) => x }
                          .toNel
                          .traverse(interpreter.interpretAll)
                          .map {
                            // Okay there were no inputs (toNel), just emit what we have
                            case None => (Nil, Chain.empty)
                            // Okay so an evaluation happened
                            case Some(res) => (res.roots.toList, res.errors)
                          } <* debug("done interpreting")

                    // Patch the previously emitted json data
                    val o: F[(JsonObject, Chain[EvalFailure])] = evalled.map { case (jsons, errs) =>
                      val allJsons = jsons.map(en => (en.value, en.cursor)) ++ paddedErrors
                      val allErrs = errs ++ Chain.fromSeq(preparedRoots.toList).flatMap { case (es, _, _) => es }

                      val stitched = allJsons.foldLeft(prevJo) { case (accum, (patch, pos)) =>
                        stitchInto(accum.asJson, patch, pos).asObject.get
                      }

                      (stitched, allErrs)
                    }

                    o.map { case (o, errs) => (o, Result(errs, o).some) }
                  }
                  .map { case (_, x) => x }
                  .unNone
            }
          }
      }
  }

  def stitchInto(oldTree: Json, subTree: Json, path: Cursor): Json =
    path.uncons match {
      case None => subTree
      case Some((p, tl)) =>
        p match {
          case GraphArc.Field(name) =>
            val oldObj = oldTree.asObject.get
            val oldValue = oldObj(name).get
            val newSubTree = stitchInto(oldValue, subTree, tl)
            oldObj.add(name, newSubTree).asJson
          case GraphArc.Index(index) =>
            val oldArr = oldTree.asArray.get
            oldArr.updated(index, stitchInto(oldArr(index), subTree, tl)).asJson
        }
    }
}
