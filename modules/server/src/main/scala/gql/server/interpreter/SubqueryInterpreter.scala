/*
 * Copyright 2024 Valdemar Grange
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

import fs2.{Stream, Pull}
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
import cats.effect.kernel.Resource

class SubqueryInterpreter[F[_]](
    state: Ref[F, EvalState[F]],
    sup: Supervisor[F],
    stats: Statistics[F],
    throttle: F ~> F,
    errors: Ref[F, Chain[EvalFailure]],
    subgraphBatches: SubgraphBatches[F]
)(implicit F: Async[F]) {
  def submit(name: String, duration: FiniteDuration, size: Int): F[Unit] =
    sup.supervise(stats.updateStats(name, duration, size)).void

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def interpretSelection[I](
      fields: List[PreparedField[F, I]],
      en: EvalNode[F, I]
  ): F[Chain[(String, Json)]] = {
    // println(s"interpretSelection ${en.value}")
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
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def interpretPrepared[I](
      s: Prepared[F, I],
      en: EvalNode[F, I]
  ): F[Json] = {
    // println(s"interpretPrepared ${en.value}")

    s match {
      case PreparedLeaf(_, _, enc) => F.pure(enc(en.value))
      case Selection(_, xs, _)     => interpretSelection(xs, en).map(_.toList.toMap.asJson)
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
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def goCont[I](
      c: Continuation[F, I],
      en: EvalNode[F, I]
  ): F[Json] = {
    // println(s"goCont ${en.value}")
    c match {
      case Continuation.Done(prep)             => interpretPrepared(prep, en)
      case fa: Continuation.Continue[F, I, c]  => goStep[I, c](fa.step, fa.next, en)
      case fa: Continuation.Contramap[F, i, I] => goCont(fa.next, en.map(fa.f))
    }
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def goStep[I, O](
      ps: PreparedStep[F, I, O],
      cont: Continuation[F, O],
      en: EvalNode[F, I]
  ): F[Json] = {
    // println(s"goStep ${en.value}")
    import PreparedStep._
    ps match {
      case Lift(_, f) => goCont(cont, en.map(f))
      case fa: EmbedEffect[F, i] =>
        runEffect(en.value: F[i], fa.sei.edgeId, 1, e => EvalFailure.EffectResolution(en.cursor, Left(e)))
          .flatMap {
            case Some(x) => goCont(cont, en.setValue(x))
            case None    => F.pure(Json.Null)
          }
      case alg: InlineBatch[F, k, o] =>
        val keys = en.value: Set[k]
        subgraphBatches
          .inlineBatch(alg, keys, en.cursor)
          .flatMap {
            case Some(x) => goCont(cont, en.setValue(x.filter { case (k, _) => keys.contains(k) }))
            case None    => F.pure(Json.Null)
          }
      case alg: Batch[F, k, v] =>
        val keys = en.value: Set[k]
        subgraphBatches
          .batch(alg.ubi, keys, en.cursor)
          .flatMap {
            case Some(x) => goCont(cont, en.setValue(x.filter { case (k, _) => keys.contains(k) }))
            case None    => F.pure(Json.Null)
          }
      case EmbedError(_) =>
        val v = en.value
        v.left.traverse(x => errors.update(EvalFailure.Raised(en.cursor, x) +: _)) *>
          v.right.traverse(x => goCont(cont, en.setValue(x))).map(_.getOrElse(Json.Null))
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
      case alg: EmbedStream[F, i] =>
        val stream = en.value: fs2.Stream[F, i]
        val effect = subscribeStream[i](stream, cont, en)

        val _ = alg
        effect
    }
  }

  def runEffect[A](fa: F[A], id: UniqueEdgeCursor, n: Int, makeError: Throwable => EvalFailure): F[Option[A]] =
    throttle(fa.timed.attempt).flatMap {
      case Right((t, a)) => submit(id.asString, t, n) *> a.some.pure[F]
      case Left(err)     => errors.update(makeError(err) +: _).as(none[A])
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
    tok <- F.unique
    d <- F.deferred[Json]

    _ <- sup.supervise {
      def mkEn(isKilled: F[Unit], leaseThis: Resource[F, Option[Int]], a: A): EvalNode[F, A] =
        en.addParentPath(tok).setInterruptContext(isKilled).setParentLease(leaseThis).setValue(a)

      def tryUpdate(poll: Poll[F], lease: Resource[F, Option[Int]], c: EvalNode[F, A]): F[Unit] =
        state.modify { s =>
          s.values match {
            // If we are not consuming, we await next and try again
            case None =>
              (s, (poll(s.ps.consumed) >> tryUpdate(poll, lease, c)))
            // If we are consuming, publish the value and await next consumption
            case Some(xs) =>
              (
                s.copy(values = Some(EvalState.Entry(tok, cont, c) :: xs)),
                s.ps.notifyProduced >> poll(s.ps.consumed)
              )
          }
        }.flatten

      def go(kill: F[Unit], a: A, handle: (EvalNode[F, A], Resource[F, Option[Int]]) => F[Unit]): Stream[F, F[Unit]] =
        for {
          leaseScope <- fs2.UnsafeFs2Access.leaseScope[F].flatMap(Pull.output1(_)).streamNoScope
          safeLease <- Stream.resource(SharedResource.make[F](leaseScope))
          _ <- Stream.eval(handle(mkEn(kill, safeLease, a), safeLease))
        } yield kill

      val sout = stream.zipWithIndex
        .evalMap[F, (Option[Deferred[F, Unit]], A)] {
          case (a, 0) => (none[Deferred[F, Unit]], a).pure[F]
          case (a, _) => F.deferred[Unit].map(d => (Some(d), a))

        }
        .zipWithPrevious
        .flatMap {
          case (None, (_, a)) => go(F.unit, a, (en, _) => goCont(cont, en).flatMap(d.complete(_)).void)
          case (Some((killPrev, _)), (killMe, a)) =>
            val kp: F[Unit] = killPrev.traverse_(_.complete(()))
            val km = killMe.traverse_(_.get)
            Stream.eval(kp) >> go(km, a, (en, sl) => F.uncancelable(tryUpdate(_, sl, en)))
        }
        // we may only interrupt after first element
        .interruptWhen(d.get >> en.interruptContext.attempt)

      sout.compile.drain
    }
    o <- d.get
  } yield o
}
