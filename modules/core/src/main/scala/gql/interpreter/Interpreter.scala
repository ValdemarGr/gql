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

  def runEdge(
    inputs: Chain[EvalNode[Any]],
    edges: List[PreparedEdge[F]],
    cont: Prepared[F, Any]
  ): W[Chain[Json]]

  def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[Map[String, Json]]]

  def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[Json]]

  def runDataField(df: PreparedDataField[F, ?, ?], input: Chain[EvalNode[Any]]): W[Chain[Json]]
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

  def combineSplit(fails: Chain[EvalFailure], succs: Chain[EvalNode[Json]]): Chain[(Cursor, Json)] =
    fails.flatMap(_.paths).map(m => (m, Json.Null)) ++ succs.map(n => (n.cursor, n.value))

  final case class StreamMetadata[F[_]](
      cursor: Cursor,
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  )

  def constructStream[F[_]: Statistics](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F],
      openTails: Boolean
  )(implicit F: Async[F], planner: Planner[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    StreamSupervisor[F, IorNec[String, Any]](openTails).flatMap { implicit streamSup =>
      fs2.Stream.resource(Supervisor[F]).flatMap { sup =>
        val changeStream = streamSup.changes
          .map(_.toList.reverse.distinctBy { case (tok, _, _) => tok }.toNel)
          .unNone

        final case class RunInput(
            position: Cursor,
            edges: List[PreparedEdge[F]],
            cont: Prepared[F, Any],
            inputValue: Any
        )

        def evaluate(metas: NonEmptyList[RunInput]): F[(Chain[EvalFailure], NonEmptyList[Json], Map[Unique.Token, StreamMetadata[F]])] =
          for {
            costTree <- metas.toList
              .flatTraverse(ri => Planner.costForCont[F](ri.edges, ri.cont, 0d))
              .map(Planner.NodeTree(_))
            planned <- planner.plan(costTree)
            accumulator <- BatchAccumulator[F](schemaState, planned)
            (res: NonEmptyList[(Chain[EvalFailure], Json, Map[Unique.Token,StreamMetadata[F]])]) <- metas.parTraverse { ri =>
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

        val inital = RunInput(Cursor.empty, Nil, PreparedQuery.Selection(rootSel), rootInput)

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
      rootSel: NonEmptyList[PreparedField[F, Any]],
      schemaState: SchemaState[F]
  )(implicit F: Async[F]): fs2.Stream[F, (Chain[EvalFailure], JsonObject)] =
    constructStream[F](rootInput, rootSel, schemaState, true)

  def runSync[F[_]: Async: Statistics: Planner](
      rootInput: Any,
      rootSel: NonEmptyList[PreparedField[F, Any]],
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

  type IndexedValue = (Int, EvalNode[Any])
  def runEdge_(
      inputs: Chain[IndexedValue],
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  ): W[Chain[IndexedValue]] =
    edges match {
      case Nil                                                          => W.pure(inputs)
      case PreparedQuery.PreparedEdge.Skip(specify, relativeJump) :: xs =>
        // Partition into skipping and non-skipping
        // Run the non-skipping first so they are in sync
        // Then continue with tl
        lift(inputs.traverse { case (i, en) => specify(en.value).map(e => (i, en.setValue(e))) })
          .flatMap { parted =>
            val (force, skip) = parted.partitionEither { case (i, x) =>
              x.value match {
                case Left(v)  => Left((i, x.setValue(v)))
                case Right(v) => Right((i, x.setValue(v)))
              }
            }
            val (ls, rs) = xs.splitAt(relativeJump)
            runEdge_(force, ls, cont).flatMap { forced =>
              val combined = (forced ++ skip).sortBy{ case (i, _) => i }
              runEdge_(combined, rs, cont)
            }
          }
      case PreparedQuery.PreparedEdge.Edge(id, resolver, statisticsName) :: xs =>
        def evalEffect(resolve: Any => F[Ior[String, Any]]): W[Chain[IndexedValue]] =
          inputs.parFlatTraverse { case (i, in) =>
            attemptUser(
              IorT(resolve(in.value)).timed
                .semiflatMap { case (dur, v) =>
                  val out = Chain(in.setValue(v))
                  submit(statisticsName, dur, 1) as out
                }
                .value
                .map(_.leftMap(NonEmptyChain.one)),
              EvalFailure.EffectResolution(in.cursor, _, in.value)
            ).map(_ tupleLeft i)
          }

        import PreparedQuery.PreparedResolver._
        val edgeRes: W[Chain[IndexedValue]] = resolver match {
          case Pure(r)     => W.pure(inputs.map { case (i, en) => i -> en.setValue(r.resolve(en.value)) })
          case Effect(r)   => evalEffect(a => r.resolve(a).map(_.rightIor))
          case Fallible(r) => evalEffect(r.resolve)
          case Stream(r) =>
            inputs.parFlatTraverse { case (i, in) =>
              attemptUserE(
                streamAccumulator
                  .add(Interpreter.StreamMetadata(in.cursor, xs, cont), r.stream(in.value))
                  .timed
                  .map { case (dur, (_, fb)) => fb.tupleLeft(dur) }
                  .rethrow
                  .flatMap { case (dur, xs) =>
                    submit(statisticsName, dur, 1) as xs.map(hd => Chain(in.setValue(hd)))
                  },
                EvalFailure.StreamHeadResolution(in.cursor, _, in.value)
              ).map(_ tupleLeft i)
            }
          case Batch(BatchResolver(_, run)) =>
            val xs = inputs.map { case (i, en) => i -> (run(en.value), en.cursor) }

            val allKeys = xs.map { case (_, ((keys, _), cg)) => (cg, keys) }

            lift(batchAccumulator.submit(id, allKeys)).map {
              case None => Chain.empty[IndexedValue]
              case Some(resultMap) =>
                xs.map { case (i, ((keys, reassoc), cg)) =>
                  i -> EvalNode(cg, reassoc(resultMap.view.filterKeys(keys.contains).toMap))
                }
            }
        }

        edgeRes.flatMap(runEdge_(_, xs, cont))
    }

  def runEdge(
      inputs: Chain[EvalNode[Any]],
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  ): W[Chain[Json]] = {
    val indexed = inputs.zipWithIndex.map(_.swap)
    runEdge_(indexed, edges, cont)
      .flatMap { remaining =>
        startNext(cont, remaining.map { case (_, x) => x })
          .map(_.zipWith(remaining) { case (j, (i, _)) => (i, j) })
          .map { indexedJson =>
            val m = indexedJson.toList.toMap
            indexed.map{ case (i, _) => m.get(i).getOrElse(Json.Null) }
          }
      }
  }

  def runDataField(df: PreparedDataField[F, ?, ?], in: Chain[EvalNode[Any]]): W[Chain[Json]] =
    runEdge(in.map(_.modify(_.field(df.outputName))), df.cont.edges.toList, df.cont.cont)

  // ns is a list of sizes, dat is a list of dat
  // for every n, there will be consumed n of dat
  def unflatten[A](ns: Vector[Int], dat: Vector[A]): Vector[Vector[A]] =
    ns.mapAccumulate(dat)((ds, n) => ds.splitAt(n).swap)._2

  def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[Map[String, Json]]] =
    Chain
      .fromSeq(dfs.toList)
      .parFlatTraverse {
        case PreparedSpecification(_, _, specify, selection) =>
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
        case df @ PreparedDataField(_, _, _, _) =>
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

  def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[Json]] = W.defer {
    s match {
      case PreparedLeaf(_, enc) => W.pure(in.map(x => enc(x.value)))
      case Selection(fields)    => runFields(fields, in).map(_.map(JsonObject.fromMap(_).asJson))
      case PreparedList(of, toSeq) =>
        val partedInput = in.map(x => Chain.fromSeq(toSeq(x.value)).mapWithIndex((y, i) => x.succeed(y, _.index(i))))
        val flattened = partedInput.flatten
        runEdge(flattened, of.edges.toList, of.cont).map { result =>
          val out = unflatten(partedInput.map(_.size.toInt).toVector, result.toVector)
          Chain.fromSeq(out.map(Json.fromValues))
        }
      case PreparedOption(of) =>
        val partedInput = in.map(nv => nv.setValue(nv.value.asInstanceOf[Option[Any]]))
        runEdge(partedInput.collect { case EvalNode(c, Some(x)) => EvalNode(c, x) }, of.edges.toList, of.cont).map { result =>
          Chain.fromSeq {
            unflatten(partedInput.map(_.value.size.toInt).toVector, result.toVector)
              .map(_.headOption.getOrElse(Json.Null))
          }
        }
    }
  }
}
