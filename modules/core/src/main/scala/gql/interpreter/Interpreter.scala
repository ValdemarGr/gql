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
  ): W[Chain[EvalNode[Json]]]

  def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]]

  def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]]

  def runDataField(df: PreparedDataField[F, ?, ?], input: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]]
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
            res <- metas.parTraverse { ri =>
              StreamMetadataAccumulator[F, StreamMetadata[F], IorNec[String, Any]].flatMap { sma =>
                val interpreter = new InterpreterImpl[F](sma, accumulator, sup)
                interpreter
                  .runEdge(Chain(EvalNode.empty(ri.inputValue)), ri.edges, ri.cont)
                  .run
                  .map { case (fail, succs) =>
                    val comb = combineSplit(fail, succs)
                    val j = reconstructField[F](ri.cont, comb.toList)
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

  def groupNodeValues2[A](nvs: List[(Cursor, A)]): Map[Option[GraphArc], List[(Cursor, A)]] =
    nvs.groupMap { case (c, _) => c.headOption } { case (c, v) => (c.tail, v) }

  def reconstructField[F[_]](p: Prepared[F, Any], cursors: List[(Cursor, Json)]): Json = {
    val m = groupNodeValues2(cursors)

    m.get(None) match {
      case Some(t) if m.size == 1 && t.forall { case (_, o) => o.isNull } => Json.Null
      case _ =>
        p match {
          case PreparedLeaf(_, _) =>
            cursors.collectFirst { case (_, x) if !x.isNull => x }.get
          case PreparedList(of, _) =>
            m.toVector
              .collect { case (Some(GraphArc.Index(i)), tl) => i -> tl }
              .map { case (i, tl) => i -> reconstructField[F](of.cont, tl) }
              .sortBy { case (i, _) => i }
              .map { case (_, v) => v }
              .asJson
          case PreparedOption(of) => reconstructField[F](of.cont, cursors)
          case Selection(fields)  => _reconstructSelection(fields, m).asJson
        }
    }
  }

  def _reconstructSelection[F[_]](
      sel: NonEmptyList[PreparedField[F, Any]],
      m: Map[Option[GraphArc], List[(Cursor, Json)]]
  ): JsonObject = {
    sel
      .map {
        case PreparedSpecification(_, _, _, selection) => _reconstructSelection(selection, m)
        case PreparedDataField(_, name, alias, cont) =>
          val n = alias.getOrElse(name)
          JsonObject(
            n -> reconstructField(cont.cont, m.get(Some(GraphArc.Field(n))).toList.flatten)
          )
      }
      .reduceLeft(_ deepMerge _)
  }

  def reconstructSelection[F[_]](
      levelCursors: List[(Cursor, Json)],
      sel: NonEmptyList[PreparedField[F, Any]]
  ): JsonObject =
    _reconstructSelection(sel, groupNodeValues2(levelCursors))
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

  // def runEdge2_(
  //     inputs: Chain[EvalNode[Any]],
  //     edges: List[PreparedEdge[F]],
  //     cont: Prepared[F, Any]
  // ): W[Chain[Option[EvalNode[Any]]]] =
  //   edges match {
  //     case Nil                                                          => W.pure(inputs)
  //     case PreparedQuery.PreparedEdge.Skip(specify, relativeJump) :: xs =>
  //       // Partition into skipping and non-skipping
  //       // Run the non-skipping first so they are in sync
  //       // Then continue with tl
  //       val (force, skip) = inputs.partitionEither { x =>
  //         specify(x.value) match {
  //           case Left(y)  => Left(x.setValue(y))
  //           case Right(y) => Right(x.setValue(y))
  //         }
  //       }
  //       val (ls, rs) = xs.splitAt(relativeJump)
  //       runEdge2_(force, ls, cont).flatMap { forced =>
  //         // val reassoc = force.zipWith(forced)((en, y) => en.setValue(y))
  //         runEdge2_(skip ++ forced, rs, cont)
  //       }
  //     case PreparedQuery.PreparedEdge.Edge(id, resolver, statisticsName) :: xs =>
  //       def evalEffect(resolve: Any => F[Ior[String, Any]]): W[Chain[Option[EvalNode[Any]]]] =
  //         inputs.parTraverse { in =>
  //           lift {
  //             IorT(resolve(in.value)).timed.semiflatMap { case (dur, v) =>
  //               val out = in.setValue(v)
  //               submit(statisticsName, dur, 1) as out
  //             }.value
  //           }.attempt
  //             .flatMap[Option[EvalNode[Any]]] {
  //               case Left(ex) =>
  //                 WriterT.put(Option.empty[EvalNode[Any]])(Chain(EvalFailure.EffectResolution(in.cursor, Left(ex), in.value)))
  //               case Right(Ior.Both(l, r)) =>
  //                 WriterT.put(Option[EvalNode[Any]](r))(Chain(EvalFailure.EffectResolution(in.cursor, Right(l), in.value)))
  //               case Right(Ior.Left(l)) =>
  //                 WriterT.put(Option.empty[EvalNode[Any]])(Chain(EvalFailure.EffectResolution(in.cursor, Right(l), in.value)))
  //               case Right(Ior.Right(r)) => WriterT.put(Option[EvalNode[Any]](r))(Chain.empty)
  //             }
  //         }

  //       import PreparedQuery.PreparedResolver._
  //       val edgeRes = resolver match {
  //         case Pure(r)     => W.pure(inputs.map(en => en.setValue(r.resolve(en.value))))
  //         case Effect(r)   => evalEffect(a => r.resolve(a).map(_.rightIor))
  //         case Fallible(r) => evalEffect(r.resolve)
  //         case Stream(r) =>
  //           inputs.parFlatTraverse { in =>
  //             attemptUserE(
  //               streamAccumulator
  //                 .add(Interpreter.StreamMetadata(in.cursor, xs, cont), r.stream(in.value))
  //                 .timed
  //                 .map { case (dur, (_, fb)) => fb.tupleLeft(dur) }
  //                 .rethrow
  //                 .flatMap { case (dur, xs) =>
  //                   submit(statisticsName, dur, 1) as xs.map(hd => Chain(in.setValue(hd)))
  //                 },
  //               EvalFailure.StreamHeadResolution(in.cursor, _, in.value)
  //             )
  //           }
  //         case Batch(BatchResolver(_, run)) =>
  //           val xs = inputs.map(en => (run(en.value), en.cursor))

  //           val allKeys = xs.map { case ((keys, _), cg) => (cg, keys) }

  //           lift(batchAccumulator.submit(id, allKeys)).map {
  //             case None => Chain.empty[EvalNode[Any]]
  //             case Some(resultMap) =>
  //               xs.map { case ((keys, reassoc), cg) =>
  //                 EvalNode(cg, reassoc(resultMap.view.filterKeys(keys.contains).toMap))
  //               }
  //           }
  //       }

  //       edgeRes.flatMap(runEdge2_(_, xs, cont))
  //   }

  // def runEdge2(
  //     inputs: Chain[EvalNode[Any]],
  //     edges: List[PreparedEdge[F]],
  //     cont: Prepared[F, Any]
  // ): W[Chain[Json]] =
  //   runEdge2_(inputs, edges, cont).flatMap(startNext2(cont, _))

  // def runDataField2(df: PreparedDataField[F, Any, ?], in: Chain[EvalNode[Any]]): W[Chain[Json]] =
  //   runEdge2(in.map(_.modify(_.field(df.id, df.alias.getOrElse(df.name)))), df.cont.edges.toList, df.cont.cont)

  // def runFields2(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[Map[String, Json]]] =
  //   Chain
  //     .fromSeq(dfs.toList)
  //     .parFlatTraverse {
  //       case PreparedSpecification(_, _, specify, selection) =>
  //         // Partition into what inputs satisfy the fragment and what don't
  //         // Run the ones that do
  //         // Then re-build an output, padding every empty output
  //         val parted = in.map(x => x.setValue(specify(x.value)))
  //         val somes = parted.collect { case EvalNode(c, Some(x)) => EvalNode(c, x) }
  //         val fa = selection.traverse { df =>
  //           runDataField2(df, somes).map(_.map(j => Map(df.name -> j)))
  //         }
  //         fa.map(_.toList.map { ys =>
  //           val (_, b) = parted.foldLeft((ys.toList, Chain.empty[Map[String, Json]])) {
  //             case ((xs, builder), EvalNode(_, None))         => (xs, builder.append(Map.empty))
  //             case ((x :: xs, builder), EvalNode(_, Some(_))) => (xs, builder.append(x))
  //             case _                                          => ???
  //           }
  //           b
  //         }).map(Chain.fromSeq)
  //       case df @ PreparedDataField(_, _, _, _) =>
  //         runDataField2(df, in)
  //           .map(_.map(j => Map(df.name -> j)))
  //           .map(Chain(_))
  //     }
  //     // We have a list (fields) of af list (inputs)
  //     // We want to transpose this such that inputs are grouped
  //     // After transposition:
  //     // Every outer is for every defined input
  //     // Every inner is for every field for that input
  //     // For each same input, we then melt all the fields together
  //     .map(_.toIterable.map(_.toIterable).transpose.map(_.foldLeft(Map.empty[String, Json])(_ ++ _)))
  //     .map(Chain.fromIterableOnce)

  // def startNext2(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[Json]] = W.defer {
  //   s match {
  //     case PreparedLeaf(_, enc) => W.pure(in.map(x => enc(x.value)))
  //     case Selection(fields)    => runFields2(fields, in).map(_.map(JsonObject.fromMap(_).asJson))
  //     case PreparedList(of, toSeq) =>
  //       val partedInput = in.map(x => Chain.fromSeq(toSeq(x.value)).mapWithIndex((y, i) => x.succeed(y, _.index(i))))
  //       val flattened = partedInput.flatten
  //       // Input size is output size, we consume the output by folding
  //       runEdge2(flattened, of.edges.toList, of.cont).map { result =>
  //         val (_, b) = partedInput.foldLeft((result.toList, Chain.empty[Json])) { case ((dataStack, topBuilder), thisArr) =>
  //           val (remainingDataStack, b2) = thisArr.foldLeft((dataStack, Chain.empty[Json])) { case ((remaining, builder), _) =>
  //             (remaining.tail, builder.append(remaining.head))
  //           }
  //           (remainingDataStack, topBuilder.append(Json.fromValues(b2.toVector)))
  //         }
  //         b
  //       }
  //     case PreparedOption(of) =>
  //       val partedInput = in.map(nv => nv.setValue(nv.value.asInstanceOf[Option[Any]]))
  //       runEdge2(partedInput.collect { case EvalNode(c, Some(x)) => EvalNode(c, x) }, of.edges.toList, of.cont).map { result =>
  //         val (_, b) = partedInput.foldLeft((result.toList, Chain.empty[Json])) {
  //           case ((dataStack, topBuilder), EvalNode(_, None))    => (dataStack, topBuilder.append(Json.Null))
  //           case ((dataStack, topBuilder), EvalNode(_, Some(_))) => (dataStack.tail, topBuilder.append(dataStack.head))
  //         }
  //         b
  //       }
  //   }
  // }

  def runEdge_(
      inputs: Chain[EvalNode[Any]],
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  ): W[Chain[EvalNode[Any]]] =
    edges match {
      case Nil                                                          => W.pure(inputs)
      case PreparedQuery.PreparedEdge.Skip(specify, relativeJump) :: xs =>
        // Partition into skipping and non-skipping
        // Run the non-skipping first so they are in sync
        // Then continue with tl
        val (force, skip) = inputs.partitionEither { x =>
          specify(x.value) match {
            case Left(y)  => Left(x.setValue(y))
            case Right(y) => Right(x.setValue(y))
          }
        }
        val (ls, rs) = xs.splitAt(relativeJump)
        runEdge_(force, ls, cont).flatMap { forced =>
          runEdge_(skip ++ forced, rs, cont)
        }
      case PreparedQuery.PreparedEdge.Edge(id, resolver, statisticsName) :: xs =>
        def evalEffect2(resolve: Any => F[Ior[String, Any]]): W[Chain[EvalNode[Option[Any]]]] =
          inputs.parTraverse { in =>
            lift {
              IorT(resolve(in.value)).timed.semiflatMap { case (dur, v) =>
                val out = in.setValue(v)
                submit(statisticsName, dur, 1) as out
              }.value
            }.attempt
              .flatMap[EvalNode[Option[Any]]] {
                case Left(ex) =>
                  WriterT.put(in.setValue(Option.empty[Any]))(Chain(EvalFailure.EffectResolution(in.cursor, Left(ex), in.value)))
                case Right(Ior.Both(l, r)) =>
                  WriterT.put(r.setValue(Option[Any](r.value)))(Chain(EvalFailure.EffectResolution(in.cursor, Right(l), in.value)))
                case Right(Ior.Left(l)) =>
                  WriterT.put(in.setValue(Option.empty[Any]))(Chain(EvalFailure.EffectResolution(in.cursor, Right(l), in.value)))
                case Right(Ior.Right(r)) => WriterT.put(r.setValue(Option[Any](r.value)))(Chain.empty)
              }
          }
        def evalEffect(resolve: Any => F[Ior[String, Any]]): W[Chain[EvalNode[Any]]] =
          inputs.parFlatTraverse { in =>
            attemptUser(
              IorT(resolve(in.value)).timed
                .semiflatMap { case (dur, v) =>
                  val out = Chain(in.setValue(v))
                  submit(statisticsName, dur, 1) as out
                }
                .value
                .map(_.leftMap(NonEmptyChain.one)),
              EvalFailure.EffectResolution(in.cursor, _, in.value)
            )
          }

        import PreparedQuery.PreparedResolver._
        val edgeRes = resolver match {
          case Pure(r)     => W.pure(inputs.map(en => en.setValue(r.resolve(en.value))))
          case Effect(r)   => evalEffect(a => r.resolve(a).map(_.rightIor))
          case Fallible(r) => evalEffect(r.resolve)
          case Stream(r) =>
            inputs.parFlatTraverse { in =>
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
              )
            }
          case Batch(BatchResolver(_, run)) =>
            val xs = inputs.map(en => (run(en.value), en.cursor))

            val allKeys = xs.map { case ((keys, _), cg) => (cg, keys) }

            lift(batchAccumulator.submit(id, allKeys)).map {
              case None => Chain.empty[EvalNode[Any]]
              case Some(resultMap) =>
                xs.map { case ((keys, reassoc), cg) =>
                  EvalNode(cg, reassoc(resultMap.view.filterKeys(keys.contains).toMap))
                }
            }
        }

        edgeRes.flatMap(runEdge_(_, xs, cont))
    }

  def runEdge(
      inputs: Chain[EvalNode[Any]],
      edges: List[PreparedEdge[F]],
      cont: Prepared[F, Any]
  ): W[Chain[EvalNode[Json]]] =
    runEdge_(inputs, edges, cont).flatMap(startNext(cont, _))

  def runFields(dfs: NonEmptyList[PreparedField[F, Any]], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] =
    Chain.fromSeq(dfs.toList).parFlatTraverse {
      case PreparedSpecification(_, _, specify, selection) =>
        runFields(selection, in.flatMap(x => Chain.fromOption(specify(x.value)).map(x.setValue)))
      case df @ PreparedDataField(_, _, _, _) => runDataField(df, in)
    }

  def startNext(s: Prepared[F, Any], in: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] = W.defer {
    s match {
      case PreparedLeaf(_, enc) => W.pure(in.map(en => en.setValue(enc(en.value))))
      case Selection(fields)    => runFields(fields, in)
      case PreparedList(of, toSeq) =>
        val (empties, continuations) =
          in.partitionEither { nv =>
            NonEmptyChain.fromChain(Chain.fromSeq(toSeq(nv.value))) match {
              case None      => Left(nv.setValue(Json.arr()))
              case Some(nec) => Right(nec.mapWithIndex { case (v, i) => nv.succeed(v, _.index(i)) })
            }
          }

        runEdge(continuations.flatMap(_.toChain), of.edges.toList, of.cont).map(_ ++ empties)
      case PreparedOption(of) =>
        val (nulls, continuations) =
          in.partitionEither { nv =>
            val inner = nv.value.asInstanceOf[Option[Any]]

            inner match {
              case None    => Left(nv.setValue(Json.Null))
              case Some(v) => Right(nv.setValue(v))
            }
          }

        runEdge(continuations, of.edges.toList, of.cont).map(_ ++ nulls)
    }
  }

  def runDataField(df: PreparedDataField[F, ?, ?], input: Chain[EvalNode[Any]]): W[Chain[EvalNode[Json]]] =
    runEdge(input.map(_.modify(_.field(df.alias.getOrElse(df.name)))), df.cont.edges.toList, df.cont.cont)
}
