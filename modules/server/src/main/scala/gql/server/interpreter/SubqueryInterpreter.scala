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
import cats.effect.implicits._
import scala.util.Success
import scala.util.Failure
import fs2.concurrent.SignallingRef
import scala.collection.immutable.ArraySeq

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
    streamingAdditions: collection.Map[NodeId, List[StepEvalNode[F, Either[Throwable, ?], ?]]]
)

class SubqueryInterpreter[F[_]](
    sup: Supervisor[F],
    stats: Statistics[F],
    throttle: F ~> F,
    errors: Ref[F, Chain[EvalFailure]],
    batches: QueryPlanBatches[F],
    api: StreamingApi[F],
    counter: SignallingRef[F, Int],
    streamingAdditions: StreamingAdditions[F]
)(implicit F: Async[F]) {
  type StepEN[I, S] = StepEvalNode[F, I, S]
  def stepEN[I, S](en: EvalNode[F, I], state: S): StepEN[I, S] = StepEvalNode(en, state)

  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  def interpretSelection[I](
      fields: ArraySeq[PreparedField[F, I]],
      xs: ArraySeq[EvalNode[F, I]]
  ): F[ArraySeq[EvalNode[F, PatchOp]]] = {
    fields.parFlatTraverse {
      case fa: PreparedSpecification[F, I, a] =>
        val ys = xs.map(en => (en, fa.specialization.specify(en.value)))
        val errs = ys.flatMap { case (en, ior) => ior.left.map(EvalFailure.Raised(en.cursor, _)) }
        val matches = ys.flatMap { case (en, ior) => ior.right.flatten.map(a => en.setValue(a)) }
        errors.update(Chain.fromSeq(errs) ++ _) *>
          interpretSelection(ArraySeq.from(fa.selection), matches)
      case df: PreparedDataField[F, I, a] =>
        interpretCont(df.cont, xs.map(_.modify(_.field(df.outputName))))
    }
  }

  def interpretCont[I, O](
      pc: PreparedCont[F, I, O],
      xs: ArraySeq[EvalNode[F, I]]
  ): F[ArraySeq[EvalNode[F, PatchOp]]] =
    interpretEffect(pc.edges, xs).flatMap(interpretPrepared(pc.cont, _))

  def interpretPrepared[I](
      s: Prepared[F, I],
      xs: ArraySeq[EvalNode[F, I]]
  ): F[ArraySeq[EvalNode[F, PatchOp]]] =
    s match {
      case PreparedLeaf(_, _, enc) => F.pure(xs.map(en => en.setValue(PatchOp.Set(enc(en.value)))))
      case Selection(_, ys, _) =>
        interpretSelection(ArraySeq.from(ys), xs).map { res =>
          // all paths to selections must exist, spec
          // we do if not exists here since an error might have set the path already
          val empties = xs.map(en => en.setValue(PatchOp.IfNotExists(Json.obj())))
          // empties come first
          empties.appendedAll(res)
        }
      case lst: PreparedList[F, a, I, b] =>
        val prefixes = xs.map(en => en.setValue(PatchOp.Set(Json.arr())))
        val values = xs.flatMap { en =>
          lst.toSeq(en.value).zipWithIndex.map { case (a, i) => en.setValue(a).modify(_.index(i)) }
        }
        interpretCont(lst.of, values).map(prefixes.appendedAll(_))
      case opt: PreparedOption[F, i, a] =>
        // must do explicit null, spec
        val zs: ArraySeq[EvalNode[F, Option[i]]] = xs
        val (nones, somes) = zs.partitionEither { z =>
          z.value match {
            case None    => Left(z.setValue(PatchOp.Set(Json.Null)))
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
            .getOrElse(ps.nodeId, List.empty)
          subbed.appendedAll(ArraySeq.from(delta).asInstanceOf[subbed.type])
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
      stream: Stream[F, Either[Throwable, A]],
      en: StepEN[X, S]
  ): F[StepEN[Either[Throwable, A], S]] = for {
    initialObject <- F.deferred[Option[StepEN[Either[Throwable, A], S]]]

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
    res <- initialObject.get.flatMap[StepEN[Either[Throwable, A], S]] {
      case Some(en2) => F.pure(en2)
      case None      => F.canceled >> F.raiseError(new RuntimeException("couldn't cancel"))
    }
  } yield res
}
