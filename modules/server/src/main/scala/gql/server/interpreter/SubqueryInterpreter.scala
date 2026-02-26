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

import fs2.Stream
import cats._
import cats.data._
import gql.preparation._
import io.circe._
import cats.effect.std._
import cats.effect._
import gql.Statistics
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import gql.resolver._
import io.circe.syntax._
import org.typelevel.scalaccompat.annotation._
import cats.effect.implicits._
import scala.util.Success
import scala.util.Failure
import fs2.concurrent.SignallingRef
import scala.collection.immutable.ArraySeq

class SubqueryInterpreter[F[_]](
    sup: Supervisor[F],
    stats: Statistics[F],
    throttle: F ~> F,
    errors: Ref[F, Chain[EvalFailure]],
    subgraphBatches: SubgraphBatches[F],
    api: StreamingApi[F],
    counter: SignallingRef[F, Int]
)(implicit F: Async[F]) {
  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def rethrow[A](
      nodeId: NodeId,
      cont: Continuation[F, A]
  ): Continuation[F, Option[Ior[EvalFailure, A]]] =
    Continuation.Rethrow[F, A](nodeId, cont)

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def interpretSelection[I](
      fields: List[PreparedField[F, I]],
      en: EvalNode[F, I]
  ): F[Chain[(String, Json)]] =
    Chain.fromSeq(fields).parFlatTraverse {
      case fa: PreparedSpecification[F, I, a] =>
        val x = fa.specialization.specify(en.value)
        val v = x.right.flatten
        val sub = subgraphBatches.multiplicityNode(fa.nodeId, v.size.toInt)
        sub *>
          x.left.traverse(x => errors.update(EvalFailure.Raised(en.cursor, x) +: _)) *>
          x.right.flatten
            .parTraverse(a => interpretSelection[a](fa.selection, en.setValue(a)))
            .map(_.getOrElse(Chain.empty))
      case df: PreparedDataField[F, I, a] =>
        interpretEffect(df.cont.edges, df.cont.cont, en.modify(_.field(df.outputName)))
          .map(j => Chain(df.outputName -> j))
    }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def interpretPrepared[I](
      s: Prepared[F, I],
      en: EvalNode[F, I]
  ): F[Json] =
    s match {
      case PreparedLeaf(_, _, enc) => F.pure(enc(en.value))
      case Selection(_, xs, _)     => interpretSelection(xs, en).map(ys => JsonObject(ys.toList: _*).asJson)
      case lst: PreparedList[F, a, I, b] =>
        val sq = lst.toSeq(en.value)
        subgraphBatches.multiplicityNode(lst.id, sq.size) *>
          sq.zipWithIndex
            .parTraverse { case (a, i) =>
              interpretEffect[a, b](lst.of.edges, lst.of.cont, en.modify(_.index(i)).setValue(a))
            }
            .map(Json.arr(_: _*))
      case opt: PreparedOption[F, i, a] =>
        val en2: Option[i] = en.value
        subgraphBatches.multiplicityNode(opt.id, en2.size.toInt) *>
          en2
            .traverse(i => interpretEffect[i, a](opt.of.edges, opt.of.cont, en.setValue(i)))
            .map(_.getOrElse(Json.Null))
    }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def goCont[I](
      c: Continuation[F, I],
      en: EvalNode[F, I]
  ): F[Json] =
    c match {
      case Continuation.Done(prep)             => interpretPrepared(prep, en)
      case fa: Continuation.Continue[F, I, c]  => goStep[I, c](fa.step, fa.next, en)
      case fa: Continuation.Contramap[F, i, I] => goCont(fa.next, en.map(fa.f))
      case fa: Continuation.Rethrow[F, i] =>
        val x = en.value: Option[Ior[EvalFailure, i]]
        val value = x.flatMap(_.right).traverse(x => goCont(fa.inner, en.setValue(x))).map(_.getOrElse(Json.Null))
        val err = x.traverse_(_.left.traverse_(x => errors.update(x +: _)))
        val doesntContinue = F.whenA(x.forall(_.right.isEmpty))(subgraphBatches.multiplicityNode(fa.nodeId, 0))
        doesntContinue *> err *> value
    }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def goStep[I, O](
      ps: PreparedStep[F, I, O],
      cont: Continuation[F, O],
      en: EvalNode[F, I]
  ): F[Json] = {
    import PreparedStep._
    def rt = rethrow[O](ps.nodeId, cont)
    ps match {
      case Lift(_, f) =>
        val res = en.map[Ior[EvalFailure, O]] { x =>
          scala.util.Try(f(x)) match {
            case Success(value) =>
              value.rightIor[EvalFailure]
            case Failure(exception) =>
              EvalFailure.EffectResolution(en.cursor, Left(exception)).leftIor[O]
          }
        }
        goCont(rt, res.map(_.some))
      case fa: EmbedEffect[F, i] =>
        val fb = en.value: F[i]
        runEffect(fb, fa.sei.edgeId, 1, e => EvalFailure.EffectResolution(en.cursor, Left(e)))
          .flatMap(x => goCont(rt, en.setValue(Some(x))))
      case alg: Batch[F, k, v] =>
        val keys = en.value: Set[k]
        subgraphBatches
          .batch(alg.ubi, keys, en.cursor)
          .map(_.map(_.filter { case (k, _) => keys.contains(k) }))
          .flatMap(y => goCont(rt, en.setValue(y.map(_.rightIor))))
      case alg: InlineBatch[F, k, o] =>
        val keys = en.value: Set[k]
        subgraphBatches
          .inlineBatch(alg, keys, en.cursor)
          .map(_.map(_.filter { case (k, _) => keys.contains(k) }))
          .flatMap(y => goCont(rt, en.setValue(y.map(_.rightIor))))
      case EmbedError(_) =>
        goCont(rt, en.setValue(en.value.leftMap(EvalFailure.Raised(en.cursor, _)).some))
      case GetMeta(_, pm0) =>
        val pm = pm0.value
        goCont(cont, en.map(_ => FieldMeta(QueryMeta(en.cursor, pm.variables), pm.args, pm.pdf)))
      case alg: Compose[F, I, a, O] =>
        goStep(alg.left, Continuation.Continue(alg.right, cont), en)
      case alg: Choose[F, a, b, c, d] =>
        val e = en.value: Either[a, b]
        val record =
          if (en.value.isLeft) subgraphBatches.multiplicityNode(alg.fbd.nodeId, 0)
          else subgraphBatches.multiplicityNode(alg.fac.nodeId, 0)
        record *> {
          e match {
            case Left(a)  => goStep(alg.fac, cont.contramap[c](Left(_)), en.setValue(a))
            case Right(b) => goStep(alg.fbd, cont.contramap[d](Right(_)), en.setValue(b))
          }
        }
      case alg: First[F, i2, o2, c2] =>
        val (i2, c2) = en.value: (i2, c2)
        goStep(alg.step, cont.contramap[o2](o2 => (o2, c2)), en.setValue(i2))
      case _: EmbedStream[F, i] =>
        val stream = en.value: fs2.Stream[F, i]
        val s2 = stream.attempt.zipWithIndex
          .map {
            case (Left(t), 0)  => EvalFailure.StreamTailResolution(en.cursor, Left(t)).leftIor[i]
            case (Left(t), _)  => EvalFailure.StreamHeadResolution(en.cursor, Left(t)).leftIor[i]
            case (Right(a), _) => a.rightIor[EvalFailure]
          }
          .map(_.some)
        subscribeStream[Option[Ior[EvalFailure, i]]](s2, rt, en.map(_.some))
    }
  }

  def runEffect[A](fa: F[A], id: UniqueEdgeCursor, n: Int, makeError: Throwable => EvalFailure): F[Ior[EvalFailure, A]] =
    throttle(fa.timed.attempt).flatMap {
      case Right((t, a)) => submit(id.asString, t, n).as(a.rightIor[EvalFailure])
      case Left(err)     => makeError(err).leftIor[A].pure[F]
    }

  def interpretEffect[I0, O0](
      ps: PreparedStep[F, I0, O0],
      cont: Prepared[F, O0],
      en: EvalNode[F, I0]
  ): F[Json] =
    goStep[I0, O0](ps, Continuation.Done(cont), en)

  def subscribeStream[A](
      stream: Stream[F, A],
      cont: Continuation[F, A],
      en: EvalNode[F, ?]
  ): F[Json] = for {
    initialObject <- F.deferred[Option[EvalNode[F, A]]]

    // historically had some issues with fs2 streams not closing, signalling internal interruption seems to help
    interruptStream <- F.deferred[Unit]
    _ <- en.active.leaseNow {
      Resource.onFinalize[F](interruptStream.complete(()).void)
    }
    b <- en.active.leaseNow {
      val stream2 =
        Stream.bracket(counter.update(_ + 1))(_ => counter.update(_ - 1)) >> stream

      stream2
        .flatMap { a =>
          Stream.resource(Res.make[F]).evalMap { streamRes =>
            val en2 = en.setValue(a).setActive(streamRes)
            // both cases must block for the next element until execution has been completed
            initialObject.tryGet.flatMap {
              // previous bug DON'T RECURSE into goCont here. This causes a memory leak since
              // the GC can't know that once initalObject is none, the other case is never reachable
              case None =>
                // order is important, we must get the current execution blocker
                // since submitting initial object can theoretically cause execution to be executed immideately
                api.awaitExecution {
                  initialObject.complete(Some(en2)).void
                }
              case Some(_) => api.submitAndAwaitExecution(cont, en2)
            }
          }
        }
        .interruptWhen(interruptStream.get.attempt)
        .compile
        .drain
        .guarantee(initialObject.complete(None).void)
        .background
    }
    _ = assert(b, "StreamInterpreter: parent resource must be alive to start stream subscription")
    res <- initialObject.get.flatMap {
      case None      => F.canceled >> F.pure(Json.Null)
      case Some(en2) => goCont(cont, en2)
    }
  } yield res
}

final case class StepEvalNode[F[_], +I, S](
    en: EvalNode[F, I],
    state: S
) {
  def cursor = en.cursor
  def value = en.value
  def active = en.active
  def setActive(active: Res[F]) = copy(en = en.setActive(active))
  def setValue[A](a: A): StepEvalNode[F, A, S] = copy(en = en.setValue(a))
  def map[B](f: I => B): StepEvalNode[F, B, S] = copy(en = en.map(f))
}

final case class StreamingAdditions[F[_]](
    streamingAdditions: collection.Map[NodeId, ArraySeq[StepEvalNode[F, Either[Throwable, ?], ?]]]
)

class NewImpl[F[_]](
    sup: Supervisor[F],
    stats: Statistics[F],
    throttle: F ~> F,
    errors: Ref[F, Chain[EvalFailure]],
    batches: QueryPlanBatches[F],
    api: StreamingApi2[F],
    counter: SignallingRef[F, Int],
    streamingAdditions: StreamingAdditions[F]
)(implicit F: Async[F]) {
  type StepEN[I, S] = StepEvalNode[F, I, S]
  def stepEN[I, S](en: EvalNode[F, I], state: S): StepEN[I, S] = StepEvalNode(en, state)

  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def rethrow[A](
      nodeId: NodeId,
      cont: Continuation[F, A]
  ): Continuation[F, Option[Ior[EvalFailure, A]]] =
    Continuation.Rethrow[F, A](nodeId, cont)

  def interpretSelection[I](
      fields: ArraySeq[PreparedField[F, I]],
      xs: ArraySeq[EvalNode[F, I]]
  ): F[ArraySeq[EvalNode[F, Json]]] = {
    fields.parFlatTraverse {
      case fa: PreparedSpecification[F, I, a] =>
        val ys = xs.map(en => (en, fa.specialization.specify(en.value)))
        val errs = ys.collect { case (en, Ior.Left(err)) => EvalFailure.Raised(en.cursor, err) }
        val matches = ys.collect { case (en, Ior.Right(Some(a))) => en.setValue(a) }
        errors.update(Chain.fromSeq(errs) ++ _) *>
          interpretSelection(ArraySeq.from(fa.selection), matches)
      case df: PreparedDataField[F, I, a] =>
        interpretCont(df.cont, xs.map(_.modify(_.field(df.outputName))))
    }
  }

  def interpretCont[I, O](
      pc: PreparedCont[F, I, O],
      xs: ArraySeq[EvalNode[F, I]]
  ): F[ArraySeq[EvalNode[F, Json]]] =
    interpretEffect(pc.edges, xs).flatMap(interpretPrepared(pc.cont, _))

  def interpretPrepared[I](
      s: Prepared[F, I],
      xs: ArraySeq[EvalNode[F, I]]
  ): F[ArraySeq[EvalNode[F, Json]]] =
    s match {
      case PreparedLeaf(_, _, enc) => F.pure(xs.map(en => en.setValue(enc(en.value))))
      case Selection(_, ys, _)     => interpretSelection(ArraySeq.from(ys), xs)
      case lst: PreparedList[F, a, I, b] =>
        val all = xs.flatMap { en =>
          val ys = lst.toSeq(en.value)
          ys.zipWithIndex.map { case (a, i) =>
            en.setValue(a).modify(_.index(i))
          }
        }
        interpretCont(lst.of, all)
      case opt: PreparedOption[F, i, a] =>
        // must do explicit null, spec
        val zs: ArraySeq[EvalNode[F, Option[i]]] = xs
        val (nones, somes) = zs.partitionEither { z =>
          z.value match {
            case None    => Left(z.setValue(Json.Null))
            case Some(i) => Right(z.setValue(i))
          }
        }
        interpretCont(opt.of, somes).map(_.appendedAll(nones))
    }

  def interpretEffect[I0, O0](
      ps: PreparedStep[F, I0, O0],
      xs: ArraySeq[EvalNode[F, I0]]
  ): F[ArraySeq[EvalNode[F, O0]]] =
    goStep(ps, xs.map(en => stepEN(en, ()))).map(_.map(_.en))

  def goStep[I, O, S](
      ps: PreparedStep[F, I, O],
      xs: ArraySeq[StepEN[I, S]]
  ): F[ArraySeq[StepEN[O, S]]] = {
    import PreparedStep._
    ps match {
      case Lift(_, f) =>
        xs.flatTraverse { en =>
          scala.util.Try(f(en.value)) match {
            case Success(value) => F.pure(ArraySeq(en.setValue(value)))
            case Failure(exception) =>
              errors
                .update(Chain.one(EvalFailure.EffectResolution(en.cursor, Left(exception))) ++ _)
                .as(ArraySeq.empty[StepEN[O, S]])
          }
        }
      case alg: First[F, i2, o2, c2] =>
        val ys: ArraySeq[StepEN[(i2, c2), S]] = xs
        val zs = ys.map { en =>
          val (i2, c2) = en.value
          stepEN[i2, (c2, S)](en.en.setValue(i2), (c2, en.state))
        }
        goStep(alg.step, zs).map(_.map { en =>
          val o2 = en.value
          val (c2, s) = en.state
          stepEN[O, S](en.en.setValue((o2, c2)), s)
        })
      case fa: EmbedEffect[F, i] =>
        xs.parFlatTraverse { en =>
          val fb = en.value: F[i]
          throttle(fb.timed.attempt).flatMap {
            case Left(err) =>
              errors
                .update(_.append(EvalFailure.EffectResolution(en.cursor, Left(err))))
                .as(ArraySeq.empty[StepEN[O, S]])
            case Right((t, x)) =>
              submit(fa.sei.edgeId.asString, t, 1).as(ArraySeq(en.setValue(x)))
          }
        }
      case alg: Batch[F, k, v] =>
        val keys: ArraySeq[EvalNode[F, Set[k]]] = xs.map(_.en)
        batches.submitBatch(alg, keys).map {
          case None => ArraySeq.empty[StepEN[O, S]]
          case Some(res) =>
            xs.map(en => en.map(current => res.filter { case (k, _) => current.contains(k) }))
        }
      case alg: InlineBatch[F, k, o] =>
        val keys: ArraySeq[EvalNode[F, Set[k]]] = xs.map(_.en)
        val keySet = keys.iterator.flatMap(_.value).toSet
        throttle(alg.run(keySet).attempt).flatMap {
          case Left(err) =>
            errors
              .update(_.append(EvalFailure.BatchResolution(Chain.fromSeq(keys.map(_.cursor)), err)))
              .as(ArraySeq.empty[StepEN[O, S]])
          case Right(res) =>
            F.pure {
              xs.map(en => en.map(current => res.filter { case (k, _) => current.contains(k) }))
            }
        }
      case EmbedError(_) =>
        val ens = xs: ArraySeq[StepEN[Ior[String, O], S]]
        val errs = ens.flatMap(en => en.value.left.map(EvalFailure.Raised(en.cursor, _)))
        val nonErrs = ens.flatMap(en => en.value.toOption.map(en.setValue))
        errors.update(Chain.fromSeq(errs) ++ _).as(nonErrs)
      case GetMeta(_, pm0) =>
        val pm = pm0.value
        F.pure(xs.map(en => en.setValue(FieldMeta(QueryMeta(en.cursor, pm.variables), pm.args, pm.pdf))))
      case alg: Compose[F, I, a, O] =>
        goStep(alg.left, xs).flatMap(goStep(alg.right, _))
      case alg: Choose[F, a, b, c, d] =>
        val (lefts, rights) = xs.partitionEither { en =>
          (en.value: Either[a, b]) match {
            case Left(a)  => Left(en.setValue(a))
            case Right(b) => Right(en.setValue(b))
          }
        }
        (goStep(alg.fac, lefts), goStep(alg.fbd, rights))
          .parMapN((l, r) => l.map(_.map(_.asLeft[d])) appendedAll r.map(_.map(_.asRight[c])))
      case _: EmbedStream[F, i] =>
        xs.parTraverse { en =>
          val stream = en.value: fs2.Stream[F, i]
          val s2 = stream.attempt
          subscribeStream(ps.nodeId, s2, en)
        }.map { subbed =>
          val delta = streamingAdditions.streamingAdditions
            .getOrElse(ps.nodeId, ArraySeq.empty)
          subbed.appendedAll(delta.asInstanceOf[subbed.type])
        }.flatMap { all =>
          val errs = all
            .flatMap(en => en.value.left.toOption.map(e => EvalFailure.StreamHeadResolution(en.cursor, Left(e))))
          val values = all.flatMap(en => en.value.toOption.map(en.setValue))
          errors.update(Chain.fromSeq(errs) ++ _).as(values)
        }
    }
  }

  def subscribeStream[A, X, S](
      nodeId: NodeId,
      stream: Stream[F, A],
      en: StepEN[X, S]
  ): F[StepEN[A, S]] = for {
    initialObject <- F.deferred[Option[StepEN[A, S]]]

    // historically had some issues with fs2 streams not closing, signalling internal interruption seems to help
    interruptStream <- F.deferred[Unit]
    _ <- en.active.leaseNow {
      Resource.onFinalize[F](interruptStream.complete(()).void)
    }
    b <- en.active.leaseNow {
      val stream2 =
        Stream.bracket(counter.update(_ + 1))(_ => counter.update(_ - 1)) >> stream

      stream2
        .flatMap { a =>
          Stream.resource(Res.make[F]).evalMap { streamRes =>
            val en2 = en.setValue(a).setActive(streamRes)
            // both cases must block for the next element until execution has been completed
            initialObject.tryGet.flatMap {
              case None =>
                // order is important, we must get the current execution blocker
                // since submitting initial object can theoretically cause execution to be executed immideately
                api.awaitExecution {
                  initialObject.complete(Some(en2)).void
                }
              case Some(_) => api.submitAndAwaitExecution(nodeId, en2)
            }
          }
        }
        .interruptWhen(interruptStream.get.attempt)
        .compile
        .drain
        .guarantee(initialObject.complete(None).void)
        .background
    }
    _ = assert(b, "StreamInterpreter: parent resource must be alive to start stream subscription")
    res <- initialObject.get.flatMap[StepEN[A, S]] {
      case Some(en2) => F.pure(en2)
      case None      => F.canceled >> F.raiseError(new RuntimeException("cloudn't cancel"))
    }
  } yield res
}
