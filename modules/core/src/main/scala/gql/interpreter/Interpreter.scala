/*
 * Copyright 2022 Valdemar Grange
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

sealed trait Interpreter[F[_]] {
  type W[A] = WriterT[F, Chain[EvalFailure], A]

  final case class IndexedData[A](
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

  def runEdge[I, O](
      inputs: Chain[IndexedData[I]],
      step: StepWithInfo[F, I, O],
      cont: Prepared2[F, O]
  ): W[Chain[Json]]

  def runFields[I](dfs: NonEmptyList[PreparedField2[F, I]], in: Chain[EvalNode[I]]): W[Chain[Map[String, Json]]]

  def startNext[I](s: Prepared2[F, I], in: Chain[EvalNode[I]]): W[Chain[Json]]

  def runDataField[I](df: PreparedDataField2[F, I], input: Chain[EvalNode[I]]): W[Chain[Json]]
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
      cursor: Cursor,
      edges: StepWithInfo[F, A, B],
      cont: Prepared2[F, B]
  )

  def constructStream[F[_]: Statistics, A](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField2[F, A]],
      schemaState: SchemaState[F],
      openTails: Boolean
  )(implicit F: Async[F], planner: Planner[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    StreamSupervisor[F, IorNec[String, Any]](openTails).flatMap { implicit streamSup =>
      fs2.Stream.resource(Supervisor[F]).flatMap { sup =>
        val changeStream = streamSup.changes
          .map(_.toList.reverse.distinctBy { case (tok, _, _) => tok }.toNel)
          .unNone

        final case class RunInput[A, B](
            position: Cursor,
            edges: StepWithInfo[F, A, B],
            cont: Prepared2[F, B],
            inputValue: Any
        )

        def evaluate(metas: NonEmptyList[RunInput]): F[(Chain[EvalFailure], NonEmptyList[Json], Map[Unique.Token, StreamMetadata[F]])] =
          for {
            costTree <- metas.toList
              .flatTraverse(ri => Planner.costForCont[F](ri.edges, ri.cont, 0d))
              .map(Planner.NodeTree(_))
            planned <- planner.plan(costTree)
            accumulator <- BatchAccumulator[F](schemaState, planned)
            (res: NonEmptyList[(Chain[EvalFailure], Json, Map[Unique.Token, StreamMetadata[F]])]) <- metas.parTraverse { ri =>
              StreamMetadataAccumulator[F, StreamMetadata[F], IorNec[String, Any]].flatMap { sma =>
                val interpreter = new InterpreterImpl[F](sma, accumulator, sup)
                interpreter
                  .runEdge(Chain(EvalNode.empty(ri.inputValue)), ri.edges, ri.cont)
                  .run
                  .map { case (fail, succs) =>
                    val j = succs.headOption.get
                    //val comb = combineSplit(fail, succs)
                    //val j = reconstructField[F](ri.cont, comb.toList)
                    (fail, j)
                  }
                  .flatMap { case (fail, j) =>
                    // All the cursors in sma should be prefixed with the start position
                    sma.getState
                      .map(_.fmap(x => x.copy(cursor = ri.position |+| x.cursor)))
                      .map((fail, j, _))
                  }
              }
            }
            smas = res.foldMapK { case (_, _, sma) => sma }
            bes <- accumulator.getErrors
            allErrors = Chain.fromSeq(res.toList).flatMap { case (errs, _, _) => errs } ++ Chain.fromSeq(bes)
          } yield (allErrors, res.map { case (_, j, _) => j }, smas)

        val inital = RunInput(Cursor.empty, Chain.empty, PreparedQuery.Selection(rootSel), rootInput)

        fs2.Stream
          .eval(evaluate(NonEmptyList.one(inital)))
          .flatMap { case (initialFails, initialSuccs, initialSM) =>
            val jo: JsonObject = initialSuccs.reduceLeft(_ deepMerge _).asObject.get

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

                      val meta = recompute(allSigNodes, s)

                      // The root nodes are the highest common signal ancestors
                      val rootNodes = activeChanges.filter { case (k, _, _, _) => meta.hcsa.contains(k) }

                      val preparedRoots =
                        rootNodes.map { case (_, _, in, sm) =>
                          in match {
                            case Left(ex) =>
                              (Chain(EvalFailure.StreamTailResolution(sm.cursor, Left(ex))), None, sm.cursor)
                            case Right(nec) =>
                              (
                                Chain.fromOption(nec.left).flatMap(_.toChain).map { msg =>
                                  EvalFailure.StreamTailResolution(sm.cursor, Right(msg))
                                },
                                nec.right.map(RunInput(sm.cursor, sm.edges, sm.cont, _)),
                                sm.cursor
                              )
                          }
                        }

                      preparedRoots.toNel
                        .traverse { xs =>
                          val paddedErrors = xs.toList.mapFilter {
                            case (_, None, c)    => Some((Json.Null, c))
                            case (_, Some(_), _) => None
                          }

                          val defined = xs.collect { case (_, Some(x), c) => (x, c) }

                          val evalled =
                            defined
                              .collect { case (x, _) => x }
                              .toNel
                              .traverse(evaluate)
                              .flatMap[(List[(Json, Cursor)], Chain[EvalFailure], Map[Unique.Token, StreamMetadata[F]])] {
                                case None => F.pure((Nil, Chain.empty, activeStreams))
                                case Some((newFails, newOutputs, newStreams)) =>
                                  val succWithInfo = newOutputs.toList zip defined.map { case (_, c) => c }

                                  val withNew = newStreams ++ activeStreams
                                  // All nodes that occured in the tree but are not in the HCSA are dead
                                  val garbageCollected = withNew -- meta.toRemove

                                  // Also remove the subscriptions and dead resources
                                  val releasesF = streamSup.release(meta.toRemove.toSet)

                                  val freeF = activeChanges
                                    .collect { case (k, rt, _, _) if meta.hcsa.contains(k) => (k, rt) }
                                    .traverse_ { case (k, rt) => streamSup.freeUnused(k, rt) }

                                  sup.supervise(releasesF >> freeF) as (succWithInfo, newFails, garbageCollected)
                              }

                          evalled.map { case (jsons, errs, finalStreams) =>
                            val allJsons = jsons ++ paddedErrors
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

  def runStreamed[F[_]: Statistics: Planner](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    constructStream[F](rootInput, rootSel, schemaState, true)

  def runSync[F[_]: Async: Statistics: Planner](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F]],
      schemaState: SchemaState[F]
  ): F[(Chain[EvalFailure], JsonObject)] =
    constructStream[F](rootInput, rootSel, schemaState, false).take(1).compile.lastOrError

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

class InterpreterImpl[F[_]](
    streamAccumulator: StreamMetadataAccumulator[F, Interpreter.StreamMetadata[F], IorNec[String, Any]],
    batchAccumulator: BatchAccumulator[F],
    sup: Supervisor[F]
)(implicit F: Async[F], stats: Statistics[F])
    extends Interpreter[F] {
  val W = Async[W]
  type E = Chain[EvalNode[Any]]
  val lift: F ~> W = WriterT.liftK[F, Chain[EvalFailure]]

  def failM[A](e: EvalFailure)(implicit M: Monoid[A]): W[A] = WriterT.put(M.empty)(Chain.one(e))

  def attemptUser[A](fa: F[IorNec[String, A]], constructor: Either[Throwable, String] => EvalFailure)(implicit
      M: Monoid[A]
  ): W[A] =
    lift(fa).attempt.flatMap {
      case Left(ex)              => WriterT.put(M.empty)(Chain(constructor(Left(ex))))
      case Right(Ior.Both(l, r)) => WriterT.put(r)(l.toChain.map(x => constructor(Right(x))))
      case Right(Ior.Left(l))    => WriterT.put(M.empty)(l.toChain.map(x => constructor(Right(x))))
      case Right(Ior.Right(r))   => WriterT.put(r)(Chain.empty)
    }

  def attemptUserE(fa: F[IorNec[String, E]], constructor: Either[Throwable, String] => EvalFailure): W[E] =
    attemptUser[E](fa, constructor)

  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def runEdge_[I, C, O](
      inputs: Chain[IndexedData[I]],
      step: StepWithInfo[F, I, C],
      cont: Chain[IndexedData[C]] => W[Chain[IndexedData[O]]]
  ): W[Chain[IndexedData[O]]] = {
    import PreparedStep._
    def liftError[A](a: A, e: Throwable, constructor: Throwable => EvalFailure): W[A] =
      WriterT.put(a)(Chain(constructor(e)))

    def attemptEffect[A](constructor: Throwable => EvalFailure)(fo: F[A]): W[Option[A]] =
      lift(fo.attempt).flatMap {
        case Left(ex) => liftError[Option[A]](None, ex, constructor)
        case Right(o) => W.pure(Some(o))
      }

    def attemptTimed[A](constructor: Throwable => EvalFailure)(fo: F[A]): W[Option[A]] =
      attemptEffect(constructor) {
        fo.timed.flatMap { case (dur, x) =>
          submit(step.stableUniqueEdgeName.asString, dur, 1) as x
        }
      }

    step.step match {
      case Pure(f) => cont(inputs.map(_.map(f)))
      case Effect(f) =>
        inputs.parFlatTraverse { id =>
          val runF = attemptTimed(e => EvalFailure.EffectResolution(id.node.cursor, Left(e), id.node.value)) {
            id.traverse(f)
          }

          runF.map(Chain.fromOption(_))
        } >>= cont
      case Raise(f) =>
        inputs.flatTraverse { id =>
          val ior = id.traverse(f)
          WriterT.put[F, Chain[EvalFailure], Chain[IndexedData[C]]](
            Chain.fromOption(ior.right)
          )(
            Chain.fromOption(ior.left.map(e => EvalFailure.Raised(id.node.cursor, e)))
          )
        } >>= cont
      case alg: Compose[F, ?, a, ?] =>
        val contR: Chain[IndexedData[a]] => W[Chain[IndexedData[O]]] = runEdge_[a, C, O](_, alg.right, cont)
        runEdge_[I, a, O](inputs, alg.left, contR)
      case Stream(f) =>
        inputs.flatTraverse { id =>
          val runF = attemptTimed(e => EvalFailure.StreamHeadResolution(id.node.cursor, Left(e), id.node.value)) {
            id
              .traverse { i =>
                streamAccumulator
                  .add2(f(i))
                  .map { case (_, e) => e }
                  .rethrow
              }
          }
          runF.map(Chain.fromOption(_))
        } >>= cont
      case alg: Skip[F, ?, i2, ?] =>
        val contR: Chain[IndexedData[Either[i2, C]]] => W[Chain[IndexedData[O]]] = { xs =>
          val (force, skip) = xs.partitionEither(in =>
            in.node.value match {
              case Left(i2) => Left(in as i2)
              case Right(c) => Right(in as c)
            }
          )
          val contR2: Chain[IndexedData[C]] => W[Chain[IndexedData[O]]] = forced => cont(forced ++ skip)

          runEdge_[i2, C, O](force, alg.step, contR2)
        }

        runEdge_[I, Either[i2, C], O](inputs, alg.check, contR)
      case GetMeta(pm) =>
        cont(inputs.map(in => in as Meta(in.node.cursor, pm.alias, pm.args, pm.variables)))
      case alg: First[F, i2, o2, c2] =>
        // (o2, c2) <:< C
        // (i2, c2) <:< I
        val inputMap: Map[Int, c2] =
          inputs.map(in => in.index -> (in.map { case (_, c2) => c2 }.node.value)).toIterable.toMap
        val contR: Chain[IndexedData[o2]] => W[Chain[IndexedData[O]]] = { xs =>
          val zs: Chain[IndexedData[C]] = xs.map(id => id tupleRight inputMap(id.index))
          cont(zs)
        }
        runEdge_[i2, o2, O](inputs.map(_.map { case (i2, _) => i2 }), alg.step, contR)
      case alg: Batch[F, k, v] =>
        val keys: Chain[(Cursor, Set[k])] = inputs.map(id => id.node.cursor -> id.node.value)

        lift {
          batchAccumulator.submit2[k, v](alg.id, keys).map {
            case None => Chain.empty
            case Some(m) =>
              inputs.map { id =>
                id.map { keys =>
                  Chain.fromIterableOnce(keys).mapFilter(k => m.get(k) tupleLeft k).iterator.toMap
                }
              }
          }
        } >>= cont
    }
  }

  def runEdge[I, O](
      inputs: Chain[EvalNode[I]],
      edges: StepWithInfo[F, I, O],
      cont: Prepared2[F, O]
  ): W[Chain[Json]] = {
    val indexed = inputs.zipWithIndex.map { case (en, i) => IndexedData(i, en) }
    runEdge_[I, O, O](indexed, edges, xs => W.pure(xs))
      .flatMap { remaining =>
        startNext(cont, remaining.map { case IndexedData(_, x) => x })
          .map(_.zipWith(remaining) { case (j, IndexedData(i, _)) => (i, j) })
          .map { indexedJson =>
            val m = indexedJson.toList.toMap
            indexed.map { case IndexedData(i, _) => m.get(i).getOrElse(Json.Null) }
          }
      }
  }

  def runDataField[I](df: PreparedDataField2[F, I], in: Chain[EvalNode[I]]): W[Chain[Json]] =
    df.cont match {
      case cont: PreparedCont2[F, i, a] =>
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

  def runFields[I](dfs: NonEmptyList[PreparedField2[F, I]], in: Chain[EvalNode[I]]): W[Chain[Map[String, Json]]] =
    Chain
      .fromSeq(dfs.toList)
      .parFlatTraverse {
        case PreparedSpecification2(_, specify, selection) =>
          // Partition into what inputs satisfy the fragment and what don't
          // Run the ones that do
          // Then re-build an output, padding every empty output
          val parted = in.map(x => x.setValue(specify(x.value)))
          val somes = parted.collect { case EvalNode(c, Some(x)) => EvalNode(c, x) }
          val fa = selection.traverse { df =>
            runDataField(df, somes).map(_.map(j => Map(df.outputName -> j)))
          }
          fa.map(_.toList.map { ys =>
            Chain.fromSeq {
              unflatten(parted.map(_.value.size.toInt).toVector, ys.toVector)
                .map(_.foldLeft(Map.empty[String, Json])(_ ++ _))
            }
          }).map(Chain.fromSeq)
        case df @ PreparedDataField2(_, _, _) =>
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

  def startNext[I](s: Prepared2[F, I], in: Chain[EvalNode[I]]): W[Chain[Json]] = W.defer {
    s match {
      case PreparedLeaf2(_, enc) => W.pure(in.map(x => enc(x.value)))
      case Selection2(fields)    => runFields(fields, in).map(_.map(JsonObject.fromMap(_).asJson))
      case s: PreparedList2[F, a, ?, b] =>
        val of = s.of
        val toSeq = s.toSeq
        val partedInput = in.map(x => Chain.fromSeq(toSeq(x.value)).mapWithIndex((y, i) => x.succeed(y, _.index(i))))
        val flattened = partedInput.flatten
        runEdge(flattened, of.edges, of.cont).map { result =>
          val out = unflatten(partedInput.map(_.size.toInt).toVector, result.toVector)
          Chain.fromSeq(out.map(Json.fromValues))
        }
      case s: PreparedOption2[F, i, o] =>
        val of = s.of
        val partedInput = in.map(nv => nv.setValue(nv.value))
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
