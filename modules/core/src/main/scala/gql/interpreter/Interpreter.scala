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
import gql.PreparedQuery._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import io.circe._
import io.circe.syntax._
import cats.effect.std.Supervisor
import scala.concurrent.duration.FiniteDuration
import cats._
import gql._

trait Interpreter[F[_]] {
  type W[A] = WriterT[F, Chain[EvalFailure], A]

  def runStep[I, C, O](
      inputs: Chain[IndexedData[I]],
      step: PreparedStep[F, I, C],
      cont: StepCont[F, C, O]
  ): W[Chain[(Int, Json)]]

  def runEdgeCont[I, O](
      cs: Chain[IndexedData[I]],
      cont: StepCont[F, I, O]
  ): W[Chain[(Int, Json)]]

  def runFields[I](dfs: NonEmptyList[PreparedField[F, I]], in: Chain[EvalNode[I]]): W[Chain[Map[String, Json]]]

  def startNext[I](s: Prepared[F, I], in: Chain[EvalNode[I]]): W[Chain[Json]]

  def runDataField[I](df: PreparedDataField[F, I], input: Chain[EvalNode[I]]): W[Chain[Json]]
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

  final case class StreamMetadata[F[_], A, B](
      originIndex: Int,
      cursor: Cursor,
      edges: StepCont[F, A, B]
  )

  final case class RunInput[F[_], A, B](
      data: IndexedData[A],
      cps: StepCont[F, A, B]
  )

  object RunInput {
    def root[F[_], A](data: A, cont: Prepared[F, A]): RunInput[F, A, Json] =
      RunInput(IndexedData(0, EvalNode.empty(data)), StepCont.Done(cont))
  }

  def evalOne[F[_]: Async: Statistics: StreamSupervisor, A, B](
      input: RunInput[F, A, B],
      background: Supervisor[F],
      batchAccum: BatchAccumulator[F]
  ): F[(Chain[EvalFailure], EvalNode[Json], Map[Unique.Token, StreamMetadata[F, ?, ?]])] = {
    StreamMetadataAccumulator[F, StreamMetadata[F, ?, ?]].flatMap { sma =>
      val interpreter = new InterpreterImpl[F](sma, batchAccum, background)

      interpreter
        .runEdgeCont(Chain(input.data), input.cps)
        .run
        .map { case (fail, succs) =>
          val (_, j) = succs.headOption.get
          (fail, j)
        }
        .flatMap { case (fail, j) => sma.getState.map((fail, input.data.node.setValue(j), _)) }
    }
  }

  def evalAll[F[_]: Async: Statistics: StreamSupervisor](
      metas: NonEmptyList[RunInput[F, ?, ?]],
      schemaState: SchemaState[F],
      background: Supervisor[F]
  )(implicit planner: Planner[F]): F[(Chain[EvalFailure], NonEmptyList[EvalNode[Json]], Map[Unique.Token, StreamMetadata[F, ?, ?]])] =
    for {
      costTree <- metas.toList
        .flatTraverse { ri =>
          Planner.runCostAnalysisFor[F, List[Planner.Node]] { implicit stats2 =>
            def contCost(step: StepCont[F, ?, ?]): Planner.H[F, List[Planner.Node]] =
              step match {
                case d: StepCont.Done[F, i]           => Planner.costForPrepared[Planner.H[F, *], F](d.prep)
                case c: StepCont.Continue[F, ?, ?, ?] => Planner.costForStep[Planner.H[F, *], F](c.step, contCost(c.next))
                case StepCont.AppendClosure(_, next)  => contCost(next)
                case StepCont.TupleWith(_, next)      => contCost(next)
              }
            contCost(ri.cps)
          }
        }
        .map(Planner.NodeTree(_))
      planned <- planner.plan(costTree)
      accumulator <- BatchAccumulator[F](schemaState, planned)
      res <- metas.parTraverse(evalOne(_, background, accumulator))
      smas = res.foldMapK { case (_, _, sma) => sma }
      bes <- accumulator.getErrors
      allErrors = Chain.fromSeq(res.toList).flatMap { case (errs, _, _) => errs } ++ Chain.fromSeq(bes)
    } yield (allErrors, res.map { case (_, en, _) => en }, smas)

  def constructStream[F[_]: Statistics, A](
      rootInput: A,
      rootSel: NonEmptyList[PreparedField[F, A]],
      schemaState: SchemaState[F],
      openTails: Boolean
  )(implicit F: Async[F], planner: Planner[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    StreamSupervisor[F](openTails).flatMap { implicit streamSup =>
      fs2.Stream.resource(Supervisor[F]).flatMap { sup =>
        val changeStream = streamSup.changes
          .map(_.toList.reverse.distinctBy { case (tok, _, _) => tok }.toNel)
          .unNone

        val inital = RunInput.root(rootInput, PreparedQuery.Selection(rootSel))

        fs2.Stream
          .eval(evalAll[F](NonEmptyList.one(inital), schemaState, sup))
          .flatMap { case (initialFails, initialSuccs, initialSM) =>
            val jo: JsonObject = initialSuccs.map(_.value).reduceLeft(_ deepMerge _).asObject.get

            fs2.Stream.emit((initialFails, jo)) ++
              changeStream
                .evalMapAccumulate((jo, initialSM)) { case ((prevOutput, activeStreams), changes) =>
                  changes
                    .map { case (k, rt, v) => activeStreams.get(k).map((k, rt, v, _)) }
                    .collect { case Some(x) => x }
                    .toNel
                    .flatTraverse { activeChanges =>
                      val s = activeChanges.toList.map { case (k, _, _, _) => k }.toSet

                      val allSigNodes = activeStreams.toList.map { case (k, sm) => (sm.cursor, k) }

                      // Compute the set of signals that have been evicted from the tree
                      val meta = recompute(allSigNodes, s)

                      // The root nodes are the highest common signal ancestors
                      val rootNodes = activeChanges.filter { case (k, _, _, _) => meta.hcsa.contains(k) }

                      // Now we have prepared the input for this next iteration
                      val preparedRoots =
                        rootNodes.map { case (_, _, in, sm) =>
                          in match {
                            case Left(ex) =>
                              (Chain(EvalFailure.StreamTailResolution(sm.cursor, Left(ex))), None, sm.cursor)
                            case Right(nec) =>
                              sm.edges match {
                                case sc: StepCont[F, a, b] =>
                                  (
                                    Chain.empty,
                                    Some(
                                      RunInput(
                                        IndexedData(sm.originIndex, EvalNode(sm.cursor, nec.asInstanceOf[a])),
                                        sc
                                      )
                                    ),
                                    sm.cursor
                                  )
                              }
                          }
                        }

                      preparedRoots.toNel
                        .traverse { xs =>
                          // Some of the streams may have emitted errors, we have to insert nulls into the result at those positions
                          val paddedErrors = xs.toList.mapFilter {
                            case (_, None, c)    => Some((Json.Null, c))
                            case (_, Some(_), _) => None
                          }

                          // These are the inputs that are ready to be evaluated
                          val defined = xs.collect { case (_, Some(x), c) => (x, c) }

                          val evalled: F[(List[EvalNode[Json]], Chain[EvalFailure], Map[Unique.Token, StreamMetadata[F, ?, ?]])] =
                            defined
                              .map { case (x, _) => x }
                              .toNel
                              .traverse(evalAll[F](_, schemaState, sup))
                              .map {
                                // Okay there were no inputs (toNel), just emit what we have
                                case None => (Nil, Chain.empty, activeStreams)
                                // Okay so an evaluation happened
                                case Some((newFails, newOutputs, newStreams)) =>
                                  val newActiveStreams = newStreams ++ activeStreams
                                  (newOutputs.toList, newFails, newActiveStreams)
                              }
                              .flatMap { case (out, errs, actives) =>
                                // Remove evicted nodes from the actives
                                val newActives = actives -- meta.toRemove

                                // Free all the unused streams
                                val gcF = streamSup.release(meta.toRemove)

                                // Free all the old versions of our roots
                                val freeRootsF = rootNodes.traverse_ { case (k, r, _, _) => streamSup.freeUnused(k, r) }

                                sup.supervise(gcF &> freeRootsF) as (out, errs, newActives)
                              }

                          // Patch the previously emitted json data
                          evalled.map { case (jsons, errs, finalStreams) =>
                            val allJsons = jsons.map(en => (en.value, en.cursor)) ++ paddedErrors
                            val allErrs = errs ++ Chain.fromSeq(xs.toList).flatMap { case (es, _, _) => es }

                            val stitched = allJsons.foldLeft(prevOutput) { case (accum, (patch, pos)) =>
                              stitchInto(accum.asJson, patch, pos).asObject.get
                            }

                            ((stitched, finalStreams), Some((allErrs, stitched)))
                          }
                        }
                    }
                    .map(_.getOrElse(((jo, activeStreams), None)))
                }
                .map { case (_, x) => x }
                .unNone
          }
      }
    }

  def runStreamed[F[_]: Statistics: Planner, A](
      rootInput: A,
      rootSel: NonEmptyList[PreparedField[F, A]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    constructStream[F, A](rootInput, rootSel, schemaState, true)

  def runSync[F[_]: Async: Statistics: Planner, A](
      rootInput: A,
      rootSel: NonEmptyList[PreparedField[F, A]],
      schemaState: SchemaState[F]
  ): F[(Chain[EvalFailure], JsonObject)] =
    constructStream[F, A](rootInput, rootSel, schemaState, false).take(1).compile.lastOrError

  def findToRemove[A](nodes: List[(Cursor, A)], s: Set[A]): Set[A] = {
    val (children, nodeHere) = nodes
      .partitionEither {
        case (xs, y) if xs.path.isEmpty => Right(y)
        case (xs, y)                    => Left((xs, y))
      }

    lazy val msg =
      s"something went terribly wrong s=$s, nodeHere:${nodeHere}\niterates:\n${children.mkString("\n")}\nall nodes:\n${nodes.mkString("\n")}"

    nodeHere match {
      case x :: Nil if s.contains(x) => children.map { case (_, v) => v }.toSet
      case _ :: Nil | Nil            => groupNodeValues(children).flatMap { case (_, v) => findToRemove(v, s) }.toSet
      case _                         => throw new Exception(msg)
    }
  }

  final case class StreamRecompute[A](
      toRemove: Set[A],
      hcsa: Set[A]
  )

  def recompute[A](nodes: List[(Cursor, A)], s: Set[A]): StreamRecompute[A] = {
    val tr = findToRemove(nodes, s)
    val hcsa = s -- tr
    StreamRecompute(tr, hcsa)
  }

  def groupNodeValues[A](nvs: List[(Cursor, A)]): Map[GraphArc, List[(Cursor, A)]] =
    nvs.groupMap { case (c, _) => c.head } { case (c, v) => (c.tail, v) }
}

final case class IndexedData[+A](
    index: Int,
    node: EvalNode[A]
)
object IndexedData {
  implicit val traverseForIndexedData: Traverse[IndexedData] = new Traverse[IndexedData] {
    override def foldLeft[A, B](fa: IndexedData[A], b: B)(f: (B, A) => B): B =
      f(b, fa.node.value)
    override def foldRight[A, B](fa: IndexedData[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.node.value, lb)
    override def traverse[G[_]: Applicative, A, B](fa: IndexedData[A])(f: A => G[B]): G[IndexedData[B]] =
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
  final case class AppendClosure[F[_], I, O](
      xs: Chain[IndexedData[I]],
      next: StepCont[F, I, O]
  ) extends StepCont[F, I, O]
  final case class TupleWith[F[_], I, C, O](
      m: Map[Int, C],
      next: StepCont[F, (I, C), O]
  ) extends StepCont[F, I, O]

  trait Visitor[F[_]] {
    def visitContinue[I, C, O](step: Continue[F, I, C, O]): StepCont[F, I, O] = step
    def visitAppendClosure[I, O](xs: AppendClosure[F, I, O]): StepCont[F, I, O] = xs
    def visitTupleWith[I, C, O](m: TupleWith[F, I, C, O]): StepCont[F, I, O] = m
  }

  def visit[F[_], I, O](cont: StepCont[F, I, O])(visitor: Visitor[F]): StepCont[F, I, O] =
    cont match {
      case x: Continue[F, i, c, o]   => visitor.visitContinue(x.copy(next = visit(x.next)(visitor)))
      case x: AppendClosure[F, i, o] => visitor.visitAppendClosure(x.copy(next = visit(x.next)(visitor)))
      case x: TupleWith[F, i, c, o]  => visitor.visitTupleWith(x.copy(next = visit(x.next)(visitor)))
      case _: Done[?, ?]             => cont
    }
}

class InterpreterImpl[F[_]](
    streamAccumulator: StreamMetadataAccumulator[F, Interpreter.StreamMetadata[F, ?, ?]],
    batchAccumulator: BatchAccumulator[F],
    sup: Supervisor[F]
)(implicit F: Async[F], stats: Statistics[F])
    extends Interpreter[F] {
  val W = Async[W]
  val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]

  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def runEdgeCont[I, O](
      cs: Chain[IndexedData[I]],
      cont: StepCont[F, I, O]
  ): W[Chain[(Int, Json)]] =
    cont match {
      case c: StepCont.Continue[F, ?, c, ?] => runStep[I, c, O](cs, c.step, c.next)
      case StepCont.AppendClosure(xs, next) => runEdgeCont(cs ++ xs, next)
      case t: StepCont.TupleWith[F, i, c, ?] =>
        runEdgeCont[(i, c), O](cs.map(id => id.map(i => (i, t.m(id.index)))), t.next)
      case StepCont.Done(prep) =>
        startNext(prep, cs.map(_.node))
          .map(_.zipWith(cs) { case (j, IndexedData(i, _)) => (i, j) })
    }

  def runStep[I, C, O](
      inputs: Chain[IndexedData[I]],
      step: PreparedStep[F, I, C],
      cont: StepCont[F, C, O]
  ): W[Chain[(Int, Json)]] = {
    import PreparedStep._

    def runNext(cs: Chain[IndexedData[C]]): W[Chain[(Int, Json)]] =
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
      case alg: Effect[F, i, c] =>
        val f = alg.f
        val cursor = alg.stableUniqueEdgeName
        inputs
          .parFlatTraverse { id =>
            val runF = attemptTimed(cursor, e => EvalFailure.EffectResolution(id.node.cursor, Left(e))) {
              id.traverse(f)
            }

            runF.map(Chain.fromOption(_))
          }
          .flatMap(runEdgeCont(_, cont))
      case Rethrow() =>
        (inputs: Chain[IndexedData[Ior[String, C]]])
          .flatTraverse { id =>
            val ior = id.sequence
            WriterT.put[F, Chain[EvalFailure], Chain[IndexedData[C]]](
              Chain.fromOption(ior.right)
            )(
              Chain.fromOption(ior.left.map(e => EvalFailure.Raised(id.node.cursor, e)))
            )
          }
          .flatMap(runEdgeCont(_, cont))
      case alg: Compose[F, ?, a, ?] =>
        val contR: StepCont[F, a, O] = StepCont.Continue[F, a, C, O](alg.right, cont)
        runStep[I, a, O](inputs, alg.left, contR)
      case Stream(f, cursor) =>
        inputs.parFlatTraverse { id =>
          // We modify the cont for the next stream emission
          // We need to get rid of the skips since they are a part of THIS evaluation, not the next
          val ridded = StepCont.visit(cont)(new StepCont.Visitor[F] {
            override def visitAppendClosure[I, O](cont: StepCont.AppendClosure[F, I, O]): StepCont[F, I, O] =
              cont.copy(xs = Chain.empty)
          })

          val runF =
            attemptTimed(cursor, e => EvalFailure.StreamHeadResolution(id.node.cursor, Left(e))) {
              id
                .traverse { i =>
                  streamAccumulator
                    .add(
                      Interpreter.StreamMetadata(
                        id.index,
                        id.node.cursor,
                        ridded
                      ),
                      f(i)
                    )
                    .map { case (_, e) => e }
                    .rethrow
                }
            }
          runF.map(Chain.fromOption(_))
        } >>= runNext
      case alg: Skip[F @unchecked, i, ?] => // scala 2 unused type variable bug?
        val (force0, skip) = (inputs: Chain[IndexedData[Either[i, C]]]).partitionEither { in =>
          in.node.value match {
            case Left(i2) => Left(in as i2)
            case Right(c) => Right(in as c)
          }
        }
        val force: Chain[IndexedData[i]] = force0
        val contR = StepCont.AppendClosure[F, C, O](skip, cont)
        runStep[i, C, O](force, alg.compute, contR)
      case GetMeta(pm) =>
        runNext(inputs.map(in => in as Meta(in.node.cursor, pm.alias, pm.args, pm.variables)))
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
      inputs: Chain[EvalNode[I]],
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

  def runDataField[I](df: PreparedDataField[F, I], in: Chain[EvalNode[I]]): W[Chain[Json]] =
    df.cont match {
      case cont: PreparedCont[F, i, a] =>
        runEdge[i, a](
          in.map(_.modify(_.field(df.outputName))),
          cont.edges,
          cont.cont
        )
    }

  // ns is a list of sizes, dat is a list of dat
  // for every n, there will be consumed n of dat
  def unflatten[A](ns: Vector[Int], dat: Vector[A]): Vector[Vector[A]] =
    ns.mapAccumulate(dat)((ds, n) => ds.splitAt(n).swap)._2

  def runFields[I](dfs: NonEmptyList[PreparedField[F, I]], in: Chain[EvalNode[I]]): W[Chain[Map[String, Json]]] =
    Chain
      .fromSeq(dfs.toList)
      .parFlatTraverse {
        case PreparedSpecification(_, specify, selection) =>
          // Partition into what inputs satisfy the fragment and what don't
          // Run the ones that do
          // Then re-build an output, padding every empty output
          val parted = in.map(x => x.setValue(specify(x.value)))
          val somes = parted.collect { case EvalNode(c, Some(x)) => EvalNode(c, x) }
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

  def startNext[I](s: Prepared[F, I], in: Chain[EvalNode[I]]): W[Chain[Json]] = W.defer {
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
        val partedInput: Chain[EvalNode[Option[i]]] = in.map(nv => nv.setValue(nv.value))
        runEdge(partedInput.collect { case EvalNode(c, Some(x)) => EvalNode(c, x) }, of.edges, of.cont)
          .map { result =>
            Chain.fromSeq {
              unflatten(partedInput.map(_.value.size.toInt).toVector, result.toVector)
                .map(_.headOption.getOrElse(Json.Null))
            }
          }
    }
  }
}
