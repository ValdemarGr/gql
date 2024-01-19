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

  def interpretSync[A](root: A, selection: Selection[F, A], throttle: F ~> F): F[Result]
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
  ): StreamInterpreter[F] = fromMakeInterpreter[F](QueryInterpreter[F](schemaState, _, _), debug, accumulate)

  def fromMakeInterpreter[F[_]: Temporal](
      makeInterpreter: (SignalScopes[F, StreamData[F, ?]], F ~> F) => QueryInterpreter[F],
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  ): StreamInterpreter[F] = new StreamInterpreter[F] {
    override def interpretSync[A](root: A, selection: Selection[F, A], throttle: F ~> F): F[Result] =
      interpretStream(root, selection, takeOne = true, throttle).take(1).compile.lastOrError

    override def interpretStream[A](
        root: A,
        selection: Selection[F, A],
        takeOne: Boolean = false,
        throttle: F ~> F = FunctionK.id[F]
    ): Stream[F, Result] =
      Stream.resource(Scope[F](None)).flatMap { rootScope =>
        Stream
          .eval(SignalScopes[F, StreamData[F, ?]](takeOne = takeOne, debug, accumulate, rootScope))
          .flatMap { ss =>
            val interpreter = makeInterpreter(ss, throttle)

            val changeStream = Stream.repeatEval(ss.unconsRelevantEvents)

            val initial = QueryInterpreter.Input.root(root, selection, rootScope)

            Stream.eval(interpreter.interpretAll(NonEmptyList.one(initial))).flatMap { res =>
              val jo: JsonObject = res.data.map { case (_, j) => j }.reduceLeft(_ deepMerge _).asObject.get

              Stream.emit(Result(res.errors, jo)) ++
                changeStream
                  .evalMapAccumulate(jo) { case (prevJo, changes) =>
                    // Now we have prepared the input for this next iteration
                    val preparedRoots =
                      changes.map { case SignalScopes.ResourceInfo(sd: StreamData[F, a], p, s, _) =>
                        val c = p.cursor
                        sd.value match {
                          case Left(ex) =>
                            (Chain(EvalFailure.StreamTailResolution(c, Left(ex))), None, c)
                          case Right(a) =>
                            val sc = sd.cont
                            (
                              Chain.empty,
                              Some(QueryInterpreter.Input[F, a](sc, EvalNode(c, a, s))),
                              c
                            )
                        }
                      }

                    // Some of the streams may have emitted errors, we have to insert nulls into the result at those positions
                    val paddedErrors = preparedRoots.toList.mapFilter {
                      case (_, None, c)    => Some((c, Json.Null))
                      case (_, Some(_), _) => None
                    }

                    // These are the inputs that are ready to be evaluated
                    val defined = preparedRoots.collect { case (_, Some(x), c) => (x, c) }

                    val evalled: F[(List[(Cursor, Json)], Chain[EvalFailure])] =
                      debug(s"interpreting for ${defined.size} inputs") >>
                        defined
                          .map { case (x, _) => x }
                          .toNel
                          .traverse(interpreter.interpretAll)
                          .map {
                            // Okay there were no inputs (toNel), just emit what we have
                            case None => (Nil, Chain.empty)
                            // Okay so an evaluation happened
                            case Some(res) => (res.data.toList, res.errors)
                          } <* debug("done interpreting")

                    // Patch the previously emitted json data
                    val o: F[(JsonObject, Chain[EvalFailure])] = evalled.flatMap { case (jsons, errs) =>
                      val allJsons = jsons ++ paddedErrors
                      val allErrs = errs ++ Chain.fromSeq(preparedRoots.toList).flatMap { case (es, _, _) => es }

                      val stitched = allJsons.foldLeftM(prevJo) { case (accum, (pos, patch)) =>
                        Temporal[F]
                          .fromEither {
                            stitchInto(accum.asJson, patch, pos, Cursor.empty).value.value
                          }
                          .map(_.asObject.get)
                      }

                      stitched.tupleRight(allErrs)
                    }

                    o.map { case (o, errs) => (o, Result(errs, o).some) }
                  }
                  .map { case (_, x) => x }
                  .unNone
            }
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
