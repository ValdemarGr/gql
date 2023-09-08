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
import cats.data._
import gql.preparation._
import io.circe._
import cats.effect.std._
import cats.effect._
import gql.Statistics
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import cats.effect.implicits._
import gql.resolver._
import io.circe.syntax._
import gql._

/** The [[SubqueryInterpreter]] recursively runs through the AST and performs a multitude of tasks:
  *   - Runs the [[gql.resolver.Resolver]]/[[gql.resolver.Step]]s defined in the query.
  *   - Accumulates errors that occur during the evaluation of the query.
  *   - Logs streams that have been subscribed to.
  *   - Batches computations that have been marked as batchable.
  */
trait SubqueryInterpreter[F[_]] {
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

  def runDataField[I](df: PreparedDataField[F, I, ?], input: Chain[EvalNode[F, I]]): W[Chain[Json]]
}

object SubqueryInterpreter {
  def apply[F[_]](
      ss: SignalScopes[F, StreamingData[F, ?, ?]],
      batchAccumulator: BatchAccumulator[F],
      sup: Supervisor[F]
  )(implicit F: Async[F], stats: Statistics[F]) =
    new SubqueryInterpreter[F] {
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
          case d: StepCont.Done[f, ?] =>
            startNext(d.prep, cs.map(_.node))
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

        def attemptTimed[A](cursor: UniqueEdgeCursor, constructor: Throwable => EvalFailure, n: Int = 1)(fo: F[A]): W[Option[A]] =
          attemptEffect(constructor) {
            fo.timed.flatMap { case (dur, x) =>
              submit(cursor.asString, dur, n) as x
            }
          }

        step match {
          case Lift(f) => runNext(inputs.map(_.map(f)))
          case alg: EmbedEffect[?, i] =>
            val cursor = alg.stableUniqueEdgeName
            inputs
              .flatTraverse { id =>
                val runF = attemptTimed(cursor, e => EvalFailure.EffectResolution(id.node.cursor, Left(e))) {
                  id.sequence[F, i]
                }

                runF.map(Chain.fromOption(_))
              }
              .flatMap(runEdgeCont(_, cont))
          case alg: InlineBatch[F, k, o] =>
            val cursor = alg.stableUniqueEdgeName
            val keys: Set[k] = inputs.toList.flatMap(_.node.value.toList).toSet

            attemptTimed(cursor, e => EvalFailure.BatchResolution(inputs.map(_.node.cursor), e), keys.size) {
              alg.run(keys).map { m =>
                inputs.map { id =>
                  id.map { keys =>
                    Chain.fromIterableOnce[k](keys).mapFilter(k => m.get(k) tupleLeft k).iterator.toMap
                  }
                }
              }
            }
              .map(xs => Chain.fromOption(xs).flatten)
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
            inputs.flatTraverse { id =>
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
                          i.attempt.map(StreamingData(id.index, ridded, _)),
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
          case GetMeta(pm0) =>
            val pm = pm0.value
            runNext {
              inputs.map { in =>
                in as FieldMeta(QueryMeta(in.node.cursor, pm.variables), pm.args, pm.pdf)
              }
            }
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

      def runDataField[I](df: PreparedDataField[F, I, ?], in: Chain[EvalNode[F, I]]): W[Chain[Json]] = {
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
            case PreparedSpecification(_, _, Nil)             => W.pure(Chain(in.as(Map.empty[String, Json])))
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
            case df @ PreparedDataField(_, _, _, _, _) =>
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
          case Selection(Nil)       => W.pure(in.as(Json.obj()))
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
}
