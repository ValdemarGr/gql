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
import cats.effect.implicits._
import cats.implicits._
import cats.effect._
import cats.data._
import io.circe._
import gql.server.planner._
import gql.preparation._
import fs2.Stream
import gql._
import scala.concurrent.duration.FiniteDuration
import cats.effect.std.Supervisor

trait QueryInterpreter[F[_]] {
  def queryRunner(
      ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]],
      batchAccumulator: BatchAccumulator[F],
      sup: Supervisor[F]
  ): QueryRunner[F]

  def evalOne[A, B](
      input: QueryInterpreter.RunInput[F, A, B],
      background: Supervisor[F],
      batchAccum: BatchAccumulator[F],
      ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]]
  ): F[(Chain[EvalFailure], EvalNode[F, Json])]

  def analyzeCost(metas: NonEmptyList[QueryInterpreter.RunInput[F, ?, ?]]): F[NodeTree]

  def evalAll(
      metas: NonEmptyList[QueryInterpreter.RunInput[F, ?, ?]],
      schemaState: SchemaState[F],
      background: Supervisor[F],
      ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]]
  )(implicit planner: Planner[F]): F[(Chain[EvalFailure], NonEmptyList[EvalNode[F, Json]])]

  def compileStream[A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]],
      openTails: Boolean
  )(implicit planner: Planner[F]): Stream[F, (Chain[EvalFailure], JsonObject)]

  def runStreamed[A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]]
  )(implicit planner: Planner[F]): Stream[F, (Chain[EvalFailure], JsonObject)]

  def runSync[A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]]
  )(implicit planner: Planner[F]): F[(Chain[EvalFailure], JsonObject)]
}

object QueryInterpreter {
  final case class RunInput[F[_], A, B](
      data: IndexedData[F, A],
      cps: StepCont[F, A, B]
  )
  object RunInput {
    def root[F[_], A](data: A, cont: Prepared[F, A], scope: Scope[F]): RunInput[F, A, Json] =
      RunInput(IndexedData(0, EvalNode.empty(data, scope)), StepCont.Done(cont))
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

  class DefaultImpl[F[_]: Statistics](
      schemaState: SchemaState[F],
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  )(implicit F: Async[F])
      extends QueryInterpreter[F] {
    override def queryRunner(
        ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]],
        batchAccumulator: BatchAccumulator[F],
        sup: Supervisor[F]
    ): QueryRunner[F] = new QueryRunner.DefaultImpl[F](ss, batchAccumulator, sup)

    override def evalOne[A, B](
        input: QueryInterpreter.RunInput[F, A, B],
        background: Supervisor[F],
        batchAccum: BatchAccumulator[F],
        ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]]
    ): F[(Chain[EvalFailure], EvalNode[F, Json])] = {
      val interpreter = queryRunner(ss, batchAccum, background)

      interpreter
        .runEdgeCont(Chain(input.data), input.cps)
        .run
        .map { case (fail, succs) =>
          val (_, j) = succs.headOption.get
          (fail, j)
        }
        .map { case (fail, j) => (fail, input.data.node.setValue(j)) }
    }

    override def analyzeCost(metas: NonEmptyList[RunInput[F, ?, ?]]): F[NodeTree] =
      Analyzer.analyzeWith[F, Unit] { analyzer =>
        metas.toList.traverse_ { ri =>
          def contCost(step: StepCont[F, ?, ?]): Analyzer.H[F, Unit] =
            step match {
              case d: StepCont.Done[F, i]           => analyzer.analyzePrepared(d.prep)
              case c: StepCont.Continue[F, ?, ?, ?] => analyzer.analyzeStep(c.step) *> contCost(c.next)
              case StepCont.Join(_, next)           => contCost(next)
              case StepCont.TupleWith(_, next)      => contCost(next)
            }
          contCost(ri.cps)
        }
      }

    override def evalAll(
        metas: NonEmptyList[QueryInterpreter.RunInput[F, ?, ?]],
        schemaState: SchemaState[F],
        background: Supervisor[F],
        ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]]
    )(implicit planner: Planner[F]): F[(Chain[EvalFailure], NonEmptyList[EvalNode[F, Json]])] =
      for {
        costTree <- analyzeCost(metas)
        planned <- planner.plan(costTree)
        accumulator <- BatchAccumulator[F](schemaState, planned)
        res <- metas.parTraverse(evalOne(_, background, accumulator, ss))
        bes <- accumulator.getErrors
        allErrors = Chain.fromSeq(res.toList).flatMap { case (errs, _) => errs } ++ Chain.fromSeq(bes)
      } yield (allErrors, res.map { case (_, en) => en })

    override def runStreamed[A](
        rootInput: A,
        rootSel: List[PreparedField[F, A]]
    )(implicit planner: Planner[F]): Stream[F, (Chain[EvalFailure], JsonObject)] =
      compileStream[A](rootInput, rootSel, true)

    override def runSync[A](
        rootInput: A,
        rootSel: List[PreparedField[F, A]]
    )(implicit planner: Planner[F]): F[(Chain[EvalFailure], JsonObject)] =
      compileStream[A](rootInput, rootSel, false).take(1).compile.lastOrError

    override def compileStream[A](rootInput: A, rootSel: List[PreparedField[F, A]], openTails: Boolean)(implicit
        planner: Planner[F]
    ): Stream[F, (Chain[EvalFailure], JsonObject)] =
      Stream.resource(Scope[F](None)).flatMap { rootScope =>
        Stream
          .eval(SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]](takeOne = !openTails, debug, accumulate, rootScope))
          .flatMap { ss =>
            Stream.resource(Supervisor[F]).flatMap { sup =>
              val changeStream = Stream.repeatEval {
                debug("waiting for changes") >>
                  ss.unconsRelevantEvents <*
                  debug("got changes")
              }

              val inital = RunInput.root(rootInput, Selection(rootSel), rootScope)

              Stream
                .eval(evalAll(NonEmptyList.one(inital), schemaState, sup, ss))
                .flatMap { case (initialFails, initialSuccs) =>
                  val jo: JsonObject = initialSuccs.map(_.value).reduceLeft(_ deepMerge _).asObject.get

                  Stream.emit((initialFails, jo)) ++
                    changeStream
                      .evalMapAccumulate(jo) { case (prevOutput, changes) =>
                        // Now we have prepared the input for this next iteration
                        val preparedRoots =
                          changes.map { case SignalScopes.ResourceInfo(sd: QueryRunner.StreamingData[F, a, b], p, s, _) =>
                            val c = p.cursor
                            sd.value match {
                              case Left(ex) =>
                                (Chain(EvalFailure.StreamTailResolution(c, Left(ex))), None, c)
                              case Right(a) =>
                                val sc = sd.edges
                                (
                                  Chain.empty,
                                  Some(
                                    RunInput[F, a, b](
                                      IndexedData(sd.originIndex, EvalNode(c, a, s)),
                                      sc
                                    )
                                  ),
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
                              .traverse(evalAll(_, schemaState, sup, ss))
                              .map {
                                // Okay there were no inputs (toNel), just emit what we have
                                case None => (Nil, Chain.empty)
                                // Okay so an evaluation happened
                                case Some((newFails, newOutputs)) =>
                                  (newOutputs.toList, newFails)
                              } <* debug("done interpreting")

                        // Patch the previously emitted json data
                        val o: F[(JsonObject, Chain[EvalFailure])] = evalled.map { case (jsons, errs) =>
                          val allJsons = jsons.map(en => (en.value, en.cursor)) ++ paddedErrors
                          val allErrs = errs ++ Chain.fromSeq(preparedRoots.toList).flatMap { case (es, _, _) => es }

                          val stitched = allJsons.foldLeft(prevOutput) { case (accum, (patch, pos)) =>
                            stitchInto(accum.asJson, patch, pos).asObject.get
                          }

                          (stitched, allErrs)
                        }

                        o.map { case (o, errs) => (o, (errs, o).some) }
                      }
                      .map { case (_, x) => x }
                      .unNone
                }
            }
          }
      }

  }
}

abstract class QueryInterpreterProxy[F[_]](private val delegate: QueryInterpreter[F]) {
  def queryRunner(
      ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]],
      batchAccumulator: BatchAccumulator[F],
      sup: Supervisor[F]
  ): QueryRunner[F] = delegate.queryRunner(ss, batchAccumulator, sup)

  def evalOne[A, B](
      input: QueryInterpreter.RunInput[F, A, B],
      background: Supervisor[F],
      batchAccum: BatchAccumulator[F],
      ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]]
  ): F[(Chain[EvalFailure], EvalNode[F, Json])] =
    delegate.evalOne(input, background, batchAccum, ss)

  def analyzeCost(metas: NonEmptyList[QueryInterpreter.RunInput[F, ?, ?]]): F[NodeTree] =
    delegate.analyzeCost(metas)

  def evalAll(
      metas: NonEmptyList[QueryInterpreter.RunInput[F, ?, ?]],
      schemaState: SchemaState[F],
      background: Supervisor[F],
      ss: SignalScopes[F, QueryRunner.StreamingData[F, ?, ?]]
  )(implicit planner: Planner[F]): F[(Chain[EvalFailure], NonEmptyList[EvalNode[F, Json]])] =
    delegate.evalAll(metas, schemaState, background, ss)

  def compileStream[A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]],
      openTails: Boolean
  )(implicit planner: Planner[F]): Stream[F, (Chain[EvalFailure], JsonObject)] =
    delegate.compileStream(rootInput, rootSel, openTails)

  def runStreamed[A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]]
  )(implicit planner: Planner[F]): Stream[F, (Chain[EvalFailure], JsonObject)] =
    delegate.runStreamed(rootInput, rootSel)

  def runSync[A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]]
  )(implicit planner: Planner[F]): F[(Chain[EvalFailure], JsonObject)] =
    delegate.runSync(rootInput, rootSel)
}
