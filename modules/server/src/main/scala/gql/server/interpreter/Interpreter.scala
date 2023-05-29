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
package gql.interpreter

import gql.resolver._
import cats.data._
import gql.server.planner._
import gql._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import io.circe._
import io.circe.syntax._
import cats.effect.std.Supervisor
import scala.concurrent.duration._
import cats._
import gql.preparation._

trait Interpreter[F[_]] {
  type W[A] = WriterT[F, Chain[EvalFailure], A]

  def runStep[I, C, O](
      inputs: Chain[IndexedData[F, I]],
      step: PreparedStep[F, I, C],
      cont: StepCont[F, C, O]
  ): W[Chain[(Int, Json)]]

  def runEdgeCont[I, O](
      cs: Chain[IndexedData[F, I]],
      cont: StepCont[F, I, O]
  ): W[Chain[(Int, Json)]]

  def runFields[I](dfs: List[PreparedField[F, I]], in: Chain[EvalNode[F, I]]): W[Chain[Map[String, Json]]]

  def startNext[I](s: Prepared[F, I], in: Chain[EvalNode[F, I]]): W[Chain[Json]]

  def runDataField[I](df: PreparedDataField[F, I], input: Chain[EvalNode[F, I]]): W[Chain[Json]]
}

object Interpreter {
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

  final case class StreamingData[F[_], A, B](
      originIndex: Int,
      edges: StepCont[F, A, B],
      value: Either[Throwable, A]
  )

  object StreamingData {
    implicit def docedForStreamingData[F[_]]: Doced[StreamingData[F, ?, ?]] =
      DebugPrinter.Printer.streamingDataDoced[F]
  }

  final case class RunInput[F[_], A, B](
      data: IndexedData[F, A],
      cps: StepCont[F, A, B]
  )

  object RunInput {
    def root[F[_], A](data: A, cont: Prepared[F, A], scope: Scope[F]): RunInput[F, A, Json] =
      RunInput(IndexedData(0, EvalNode.empty(data, scope)), StepCont.Done(cont))
  }

  def evalOne[F[_]: Async: Statistics, A, B](
      input: RunInput[F, A, B],
      background: Supervisor[F],
      batchAccum: BatchAccumulator[F],
      ss: SignalScopes[F, StreamingData[F, ?, ?]]
  ): F[(Chain[EvalFailure], EvalNode[F, Json])] = {
    val interpreter = new InterpreterImpl[F](ss, batchAccum, background)

    interpreter
      .runEdgeCont(Chain(input.data), input.cps)
      .run
      .map { case (fail, succs) =>
        val (_, j) = succs.headOption.get
        (fail, j)
      }
      .map { case (fail, j) => (fail, input.data.node.setValue(j)) }
  }

  def evalAll[F[_]: Async: Statistics](
      metas: NonEmptyList[RunInput[F, ?, ?]],
      schemaState: SchemaState[F],
      background: Supervisor[F],
      ss: SignalScopes[F, StreamingData[F, ?, ?]]
  )(implicit planner: Planner[F]): F[(Chain[EvalFailure], NonEmptyList[EvalNode[F, Json]])] =
    for {
      costTree <- Analyzer.analyzeWith[F, Unit] { analyzer =>
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
      planned <- planner.plan(costTree)
      accumulator <- BatchAccumulator[F](schemaState, planned)
      res <- metas.parTraverse(evalOne(_, background, accumulator, ss))
      bes <- accumulator.getErrors
      allErrors = Chain.fromSeq(res.toList).flatMap { case (errs, _) => errs } ++ Chain.fromSeq(bes)
    } yield (allErrors, res.map { case (_, en) => en })

  def constructStream[F[_]: Statistics, A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]],
      schemaState: SchemaState[F],
      openTails: Boolean,
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  )(implicit F: Async[F], planner: Planner[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] = {
    fs2.Stream.resource(Scope[F](None)).flatMap { rootScope =>
      fs2.Stream
        .eval(SignalScopes[F, StreamingData[F, ?, ?]](takeOne = !openTails, debug, accumulate, rootScope))
        .flatMap { ss =>
          fs2.Stream.resource(Supervisor[F]).flatMap { sup =>
            val changeStream = fs2.Stream.repeatEval {
              debug("waiting for changes") >>
                ss.unconsRelevantEvents <*
                debug("got changes")
            }

            val inital = RunInput.root(rootInput, Selection(rootSel), rootScope)

            fs2.Stream
              .eval(evalAll[F](NonEmptyList.one(inital), schemaState, sup, ss))
              .flatMap { case (initialFails, initialSuccs) =>
                val jo: JsonObject = initialSuccs.map(_.value).reduceLeft(_ deepMerge _).asObject.get

                fs2.Stream.emit((initialFails, jo)) ++
                  changeStream
                    .evalMapAccumulate(jo) { case (prevOutput, changes) =>
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
                            .traverse(evalAll[F](_, schemaState, sup, ss))
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

  def runStreamed[F[_]: Statistics: Planner, A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]],
      schemaState: SchemaState[F],
      debug: DebugPrinter[F],
      accumulate: Option[FiniteDuration]
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    constructStream[F, A](rootInput, rootSel, schemaState, true, debug, accumulate)

  def runSync[F[_]: Async: Statistics: Planner, A](
      rootInput: A,
      rootSel: List[PreparedField[F, A]],
      schemaState: SchemaState[F],
      debug: DebugPrinter[F]
  ): F[(Chain[EvalFailure], JsonObject)] =
    constructStream[F, A](rootInput, rootSel, schemaState, false, debug, None).take(1).compile.lastOrError
}

final case class EvalNode[F[_], +A](cursor: Cursor, value: A, scope: Scope[F]) {
  def setValue[B](value: B): EvalNode[F, B] = copy(value = value)

  def modify(f: Cursor => Cursor): EvalNode[F, A] = copy(cursor = f(cursor))

  def succeed[B](value: B, f: Cursor => Cursor): EvalNode[F, B] =
    EvalNode(f(cursor), value, scope)

  def succeed[B](value: B): EvalNode[F, B] = succeed(value, identity)

  def setScope(scope: Scope[F]) = copy(scope = scope)
}

object EvalNode {
  def empty[F[_], A](value: A, scope: Scope[F]) = EvalNode[F, A](Cursor.empty, value, scope)
}

final case class IndexedData[F[_], +A](
    index: Int,
    node: EvalNode[F, A]
)
object IndexedData {
  implicit def traverseForIndexedData[F[_]]: Traverse[IndexedData[F, *]] = new Traverse[IndexedData[F, *]] {
    override def foldLeft[A, B](fa: IndexedData[F, A], b: B)(f: (B, A) => B): B =
      f(b, fa.node.value)
    override def foldRight[A, B](fa: IndexedData[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.node.value, lb)
    override def traverse[G[_]: Applicative, A, B](fa: IndexedData[F, A])(f: A => G[B]): G[IndexedData[F, B]] =
      f(fa.node.value).map(b => IndexedData(fa.index, fa.node.setValue(b)))
  }
}

sealed trait StepCont[F[_], -I, +O]
object StepCont {
  final case class Done[F[_], I](prep: Prepared[F, I]) extends StepCont[F, I, Json]
  final case class Continue[F[_], I, C, O](
      step: PreparedStep[F, I, C],
      next: StepCont[F, C, O]
  ) extends StepCont[F, I, O]
  final case class Join[F[_], I, O](
      submit: Chain[IndexedData[F, I]] => F[Option[Chain[IndexedData[F, I]]]],
      next: StepCont[F, I, O]
  ) extends StepCont[F, I, O]
  final case class TupleWith[F[_], I, C, O](
      m: Map[Int, C],
      next: StepCont[F, (I, C), O]
  ) extends StepCont[F, I, O]

  trait Visitor[F[_]] {
    def visitContinue[I, C, O](step: Continue[F, I, C, O]): StepCont[F, I, O] = step
    def visitTupleWith[I, C, O](m: TupleWith[F, I, C, O]): StepCont[F, I, O] = m
    def visitJoin[I, O](p: Join[F, I, O]): StepCont[F, I, O] = p
  }

  def visit[F[_], I, O](cont: StepCont[F, I, O])(visitor: Visitor[F]): StepCont[F, I, O] =
    cont match {
      case x: Continue[F, i, c, o]  => visitor.visitContinue(x.copy(next = visit(x.next)(visitor)))
      case x: TupleWith[F, i, c, o] => visitor.visitTupleWith(x.copy(next = visit(x.next)(visitor)))
      case x: Join[F, ?, ?]         => visitor.visitJoin(x.copy(next = visit(x.next)(visitor)))
      case _: Done[?, ?]            => cont
    }
}

class InterpreterImpl[F[_]](
    ss: SignalScopes[F, Interpreter.StreamingData[F, ?, ?]],
    batchAccumulator: BatchAccumulator[F],
    sup: Supervisor[F]
)(implicit F: Async[F], stats: Statistics[F])
    extends Interpreter[F] {
  val W = Async[W]
  val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]

  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def runEdgeCont[I, O](
      cs: Chain[IndexedData[F, I]],
      cont: StepCont[F, I, O]
  ): W[Chain[(Int, Json)]] = {
    cont match {
      case c: StepCont.Continue[F, ?, c, ?] => runStep[I, c, O](cs, c.step, c.next)
      case t: StepCont.TupleWith[F, i, c, ?] =>
        runEdgeCont[(i, c), O](cs.map(id => id.map(i => (i, t.m(id.index)))), t.next)
      case StepCont.Join(c, next) =>
        lift(c(cs)).flatMap {
          case None     => W.pure(Chain.empty)
          case Some(cs) => runEdgeCont(cs, next)
        }
      case StepCont.Done(prep) =>
        startNext(prep, cs.map(_.node))
          .map(_.zipWith(cs) { case (j, IndexedData(i, _)) => (i, j) })
    }
  }

  def runStep[I, C, O](
      inputs: Chain[IndexedData[F, I]],
      step: PreparedStep[F, I, C],
      cont: StepCont[F, C, O]
  ): W[Chain[(Int, Json)]] = {
    import PreparedStep._

    def runNext(cs: Chain[IndexedData[F, C]]): W[Chain[(Int, Json)]] =
      runEdgeCont(cs, cont)

    def liftError[A](a: A, e: Throwable, constructor: Throwable => EvalFailure): W[A] =
      WriterT.put(a)(Chain(constructor(e)))

    def attemptEffect[A](constructor: Throwable => EvalFailure)(fo: F[A]): W[Option[A]] =
      lift(fo.attempt).flatMap {
        case Left(ex) => liftError[Option[A]](None, ex, constructor)
        case Right(o) => W.pure(Some(o))
      }

    def attemptTimed[A](cursor: UniqueEdgeCursor, constructor: Throwable => EvalFailure)(fo: F[A]): W[Option[A]] =
      attemptEffect(constructor) {
        fo.timed.flatMap { case (dur, x) =>
          submit(cursor.asString, dur, 1) as x
        }
      }

    step match {
      case Lift(f) => runNext(inputs.map(_.map(f)))
      case alg: EmbedEffect[F @unchecked, i] =>
        val cursor = alg.stableUniqueEdgeName
        inputs
          .parFlatTraverse { id =>
            val runF = attemptTimed(cursor, e => EvalFailure.EffectResolution(id.node.cursor, Left(e))) {
              id.sequence[F, i]
            }

            runF.map(Chain.fromOption(_))
          }
          .flatMap(runEdgeCont(_, cont))
      case EmbedError() =>
        (inputs: Chain[IndexedData[F, Ior[String, C]]])
          .flatTraverse { id =>
            val ior = id.sequence
            WriterT.put[F, Chain[EvalFailure], Chain[IndexedData[F, C]]](
              Chain.fromOption(ior.right)
            )(
              Chain.fromOption(ior.left.map(e => EvalFailure.Raised(id.node.cursor, e)))
            )
          }
          .flatMap(runEdgeCont(_, cont))
      case alg: Compose[F, ?, a, ?] =>
        val contR: StepCont[F, a, O] = StepCont.Continue[F, a, C, O](alg.right, cont)
        runStep[I, a, O](inputs, alg.left, contR)
      case alg: EmbedStream[F @unchecked, i] =>
        inputs.parFlatTraverse { id =>
          // We modify the cont for the next stream emission
          // We need to get rid of the skips since they are a part of THIS evaluation, not the next
          val ridded = StepCont.visit(cont)(new StepCont.Visitor[F] {
            override def visitJoin[I, O](cont: StepCont.Join[F, I, O]): StepCont[F, I, O] =
              cont.next
          })

          val runF =
            attemptTimed(alg.stableUniqueEdgeName, e => EvalFailure.StreamHeadResolution(id.node.cursor, Left(e))) {
              id
                .traverse { (i: fs2.Stream[F, i]) =>
                  ss
                    .acquireAwait(
                      i.attempt.map { i =>
                        Interpreter.StreamingData(
                          id.index,
                          ridded,
                          i
                        )
                      },
                      id.node.scope,
                      signal = alg.signal,
                      cursor = id.node.cursor
                    )
                    .map { case (s, sd) => sd.value.map { case i: i @unchecked => (i, s) } }
                    .rethrow
                }
            }
          runF
            .map(Chain.fromOption(_))
            .map(_.map { id =>
              val (i, s) = id.node.value
              id.copy(node = id.node.setScope(s).setValue(i))
            })
        } >>= runNext
      case alg: Choose[F @unchecked, a, b, c, d] =>
        val (lefts, rights) = (inputs: Chain[IndexedData[F, Either[a, b]]]).partitionEither { in =>
          in.node.value match {
            case Left(a)  => Left(in as a)
            case Right(b) => Right(in as b)
          }
        }

        lift(F.deferred[Chain[IndexedData[F, C]]]).flatMap { d =>
          // For any side, either it completes or it lets the other side complete
          def complete(xs: Chain[IndexedData[F, C]]): F[Option[Chain[IndexedData[F, C]]]] =
            d.complete(xs).flatMap {
              case true => F.pure(None)
              case false =>
                d.get.map { other =>
                  Some(xs ++ other)
                }
            }

          val leftAlg = Compose(alg.fac, Lift[F, c, C](Left(_)))
          val rightAlg = Compose(alg.fbc, Lift[F, d, C](Right(_)))

          val leftF = runStep(lefts, leftAlg, StepCont.Join[F, C, O](complete _, cont))
          val rightF = runStep(rights, rightAlg, StepCont.Join[F, C, O](complete _, cont))

          (leftF, rightF).parMapN(_ ++ _)
        }
      case GetMeta(pm) =>
        runNext(inputs.map(in => in as FieldMeta(QueryMeta(in.node.cursor, pm.variables), pm.args, pm.alias)))
      case alg: First[F @unchecked, i2, o2, c2] =>
        // (o2, c2) <:< C
        // (i2, c2) <:< I
        val inputMap: Map[Int, c2] =
          inputs.map(in => in.index -> (in.map { case (_, c2) => c2 }.node.value)).toIterable.toMap

        val base: StepCont[F, C, O] = cont
        val contR = StepCont.TupleWith[F, o2, c2, O](
          inputMap,
          base
        )
        runStep[i2, o2, O](inputs.map(_.map { case (i2, _) => i2 }), alg.step, contR)
      case alg: Batch[F @unchecked, k, v] =>
        val keys: Chain[(Cursor, Set[k])] = inputs.map(id => id.node.cursor -> id.node.value)

        lift {
          batchAccumulator.submit[k, v](alg.globalEdgeId, keys).map {
            case None => Chain.empty
            case Some(m) =>
              inputs.map { id =>
                id.map { keys =>
                  Chain.fromIterableOnce[k](keys).mapFilter(k => m.get(k) tupleLeft k).iterator.toMap
                }
              }
          }
        }.flatMap(runEdgeCont(_, cont))
    }
  }

  def runEdge[I, O](
      inputs: Chain[EvalNode[F, I]],
      edges: PreparedStep[F, I, O],
      cont: Prepared[F, O]
  ): W[Chain[Json]] = {
    val indexed = inputs.zipWithIndex.map { case (en, i) => IndexedData(i, en) }
    runStep(indexed, edges, StepCont.Done(cont))
      .map { indexedJson =>
        val m = indexedJson.toList.toMap
        indexed.map(_.index).map(i => m.get(i).getOrElse(Json.Null))
      }
  }

  def runDataField[I](df: PreparedDataField[F, I], in: Chain[EvalNode[F, I]]): W[Chain[Json]] = {
    df.cont match {
      case cont: PreparedCont[F, i, a] =>
        runEdge[i, a](
          in.map(_.modify(_.field(df.outputName))),
          cont.edges,
          cont.cont
        )
    }
  }

  // ns is a list of sizes, dat is a list of dat
  // for every n, there will be consumed n of dat
  def unflatten[A](ns: Vector[Int], dat: Vector[A]): Vector[Vector[A]] =
    ns.mapAccumulate(dat)((ds, n) => ds.splitAt(n).swap)._2

  def runFields[I](dfs: List[PreparedField[F, I]], in: Chain[EvalNode[F, I]]): W[Chain[Map[String, Json]]] = {
    Chain
      .fromSeq(dfs.toList)
      .parFlatTraverse {
        case PreparedSpecification(_, specify, selection) =>
          // Partition into what inputs satisfy the fragment and what don't
          // Run the ones that do
          // Then re-build an output, padding every empty output
          val parted = in.map(x => x.setValue(specify(x.value)))
          val somes = parted.collect { case EvalNode(c, Some(x), s) => EvalNode(c, x, s) }
          val fa = selection.parTraverse { df =>
            runDataField(df, somes).map(_.map(j => Map(df.outputName -> j)))
          }
          fa.map(_.toList.map { ys =>
            Chain.fromSeq {
              unflatten(parted.map(_.value.size.toInt).toVector, ys.toVector)
                .map(_.foldLeft(Map.empty[String, Json])(_ ++ _))
            }
          }).map(Chain.fromSeq)
        case df @ PreparedDataField(_, _, _) =>
          runDataField(df, in)
            .map(_.map(j => Map(df.outputName -> j)))
            .map(Chain(_))
      }
      // We have a list (fields) of af list (inputs)
      // We want to transpose this such that inputs are grouped
      // After transposition:
      // Every outer is for every defined input
      // Every inner is for every field for that input
      // For each same input, we then melt all the fields together
      .map(_.toIterable.map(_.toIterable).transpose.map(_.foldLeft(Map.empty[String, Json])(_ ++ _)))
      .map(Chain.fromIterableOnce)
  }

  def startNext[I](s: Prepared[F, I], in: Chain[EvalNode[F, I]]): W[Chain[Json]] = W.defer {
    s match {
      case PreparedLeaf(_, enc) => W.pure(in.map(x => enc(x.value)))
      case Selection(fields)    => runFields(fields, in).map(_.map(JsonObject.fromMap(_).asJson))
      case s: PreparedList[F, a, ?, b] =>
        val of = s.of
        val toSeq = s.toSeq
        val partedInput = in.map(x => Chain.fromSeq(toSeq(x.value)).mapWithIndex((y, i) => x.succeed(y, _.index(i))))
        val flattened = partedInput.flatten
        runEdge(flattened, of.edges, of.cont).map { result =>
          val out = unflatten(partedInput.map(_.size.toInt).toVector, result.toVector)
          Chain.fromSeq(out.map(Json.fromValues))
        }
      case s: PreparedOption[F @unchecked, i, ?] =>
        val of = s.of
        val partedInput: Chain[EvalNode[F, Option[i]]] = in.map(nv => nv.setValue(nv.value))
        runEdge(partedInput.collect { case EvalNode(c, Some(x), s) => EvalNode(c, x, s) }, of.edges, of.cont)
          .map { result =>
            Chain.fromSeq {
              unflatten(partedInput.map(_.value.size.toInt).toVector, result.toVector)
                .map(_.headOption.getOrElse(Json.Null))
            }
          }
    }
  }
}
