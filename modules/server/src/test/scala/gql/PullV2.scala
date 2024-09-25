package hest

import cats.implicits._
import cats.effect.implicits._
import cats._
import cats.effect._
import munit.CatsEffectSuite
import fs2.Chunk

trait Arc[F[_]] {
  def lease: Resource[F, Int]

  def leases: F[Int]

  def add[A](r: Resource[F, A]): F[Option[A]]

  def leaseAndAdd[A](r: Resource[F, A]): Resource[F, Option[A]]
}

object Arc {
  def make[F[_]]: Resource[F, Arc[F]] = ???
}

sealed trait Pull2[+F[_], +O, +A]
object Pull2 {
  final case class Output[O](value: fs2.Chunk[O]) extends Pull2[Nothing, O, Unit]
  final case class FlatMap[F[_], O, A, B](
      fa: Pull2[F, O, A],
      f: A => Pull2[F, O, B]
  ) extends Pull2[F, O, B]
  final case class Eval[F[_], A](fa: F[A]) extends Pull2[F, Nothing, A]
  final case class Pure[A](a: A) extends Pull2[Nothing, Nothing, A]
  final case class Uncons[F[_], O](
      fa: Pull2[F, O, Unit]
  ) extends Pull2[F, Nothing, Option[(fs2.Chunk[O], Pull2[F, O, Unit])]]
  final case class FlatMapOutput[F[_], O, O2](
      fa: Pull2[F, O, Unit],
      f: O => Pull2[F, O2, Unit]
  ) extends Pull2[F, O2, Unit]
  final case class HandleErrorWith[F[_], O, A](
      fa: Pull2[F, O, A],
      f: Throwable => Pull2[F, O, A]
  ) extends Pull2[F, O, A]
  final case class SubArc[F[_], O, A](
      fa: Pull2[F, O, A]
  ) extends Pull2[F, O, A]
  final case class Acquire[F[_], A](r: Resource[F, A]) extends Pull2[F, Nothing, Option[A]]
  final case class GetArc[F[_]](arc: Arc[F]) extends Pull2[F, Nothing, Arc[F]]

  def output[O](value: fs2.Chunk[O]): Pull2[Nothing, O, Unit] = Output(value)

  def eval[F[_], A](fa: F[A]): Pull2[F, Nothing, A] = Eval(fa)

  val done: Pull2[Nothing, Nothing, Unit] = Pure(())

  def flatMapOutput[F[_], O, O2](
      fa: Pull2[F, O, Unit],
      f: O => Pull2[F, O2, Unit]
  ): Pull2[F, O2, Unit] = FlatMapOutput(fa, f)

  def compile[F[_], O, A, B](
      p: Pull2[F, O, A],
      init: B
  )(
      fold: (B, fs2.Chunk[O]) => B
  )(implicit F: Sync[F]): F[(B, A)] = {
    val emptyChunk = fs2.Chunk.empty

    val leftUnit = ().asLeft[Nothing]

    def fastSeq[O2](fa: Either[Unit, Pull2[F, O2, Unit]]): Pull2[F, O2, Unit] = fa match {
      case Right(p) => p
      case _        => done
    }

    def fastTraversePull[O2, A2, B2](as: List[A2], f: A2 => Pull2[F, O2, B2]): Pull2[F, O2, Unit] =
      as match {
        case Nil     => done
        case x :: xs => f(x) >> fastTraversePull(xs, f)
      }

    final case class Deferral[O2, +A2](
        values: fs2.Chunk[O2],
        getValue: Either[A2, Pull2[F, O2, A2]]
    )

    var arc: Arc[F] = Arc.make[F]

    def go[O2, A2](p: Pull2[F, O2, A2]): F[Deferral[O2, A2]] = F.defer {
      p match {
        case Pure(a)           => F.pure(Deferral(emptyChunk, Left(a)))
        case eval: Eval[F, A2] => eval.fa.map(a => Deferral(emptyChunk, Left(a)))
        case o: Output[O2]     => F.pure(Deferral(o.value, leftUnit))
        case uncons: Uncons[F, o] =>
          go[o, Unit](uncons.fa).map { d =>
            Deferral(
              emptyChunk,
              Left {
                if (d.values.isEmpty) None
                else Some((d.values, fastSeq(d.getValue)))
              }
            )
          }
        case fm: FlatMap[F, O2, a, A2] =>
          go[O2, a](fm.fa).map { out =>
            Deferral[O2, A2](
              out.values,
              Right {
                out.getValue match {
                  case Left(a)  => fm.f(a)
                  case Right(p) => FlatMap(p, fm.f)
                }
              }
            )
          }
        case fmo: FlatMapOutput[F, o, O2] =>
          go[o, Unit](fmo.fa).flatMap { out =>
            go[O2, Unit](fastTraversePull(out.values.toList, fmo.f)).map { d =>
              out.getValue match {
                case Left(())   => d
                case Right((p)) => Deferral(d.values, Right(fastSeq(d.getValue) >> FlatMapOutput(p, fmo.f)))
              }
            }
          }
        case he: HandleErrorWith[F, O2, A2] =>
          go[O2, A2](he.fa).attempt.flatMap {
            case Left(e)  => go(he.f(e))
            case Right(d) => F.pure(Deferral(d.values, d.getValue.map(p => HandleErrorWith(p, he.f))))
          }
        case sa: SubArc[F, O2, A2] =>
          F.uncancelable { poll =>
            poll(arc.leaseAndAdd(Arc.make[F]).allocated).flatMap { case (child, release) =>
              val old = arc
              arc = child.get
              poll {
                go(sa.fa)
                  .map { d =>
                    val next = d.getValue.leftMap(Pure(_)).merge
                    val guaranteed = HandleErrorWith(
                      next,
                      e => {
                        arc = old
                        eval(release) >> eval(F.raiseError(e))
                      }
                    )
                    Deferral(d.values, Right(guaranteed))
                  }
                  .guarantee(release)
              }
            }
          }
      }
    }

    def loop(
        d: Deferral[O, A],
        b: B
    ): F[(B, A)] = {
      val b2 = fold(b, d.values)
      d.getValue match {
        case Left(a)  => F.pure((b2, a))
        case Right(p) => go(p).flatMap(loop(_, b2))
      }
    }

    go(p).flatMap(x => loop(x, init))
  }

  implicit def monad[F[_], O]: Monad[Pull2[F, O, *]] = new Monad[Pull2[F, O, *]] {
    def pure[A](a: A): Pull2[F, O, A] = Pure(a)
    def flatMap[A, B](fa: Pull2[F, O, A])(f: A => Pull2[F, O, B]): Pull2[F, O, B] = FlatMap(fa, f)
    def tailRecM[A, B](a: A)(f: A => Pull2[F, O, Either[A, B]]): Pull2[F, O, B] = ???
  }

  implicit class Pull2Ops[+F[_], +O, +A](private val self: Pull2[F, O, A]) {
    def flatMap[F2[x] >: F[x], O2 >: O, B](f: A => Pull2[F2, O2, B]): Pull2[F2, O2, B] = FlatMap(self, f)

    def >>[F2[x] >: F[x], O2 >: O, B](that: => Pull2[F2, O2, B]): Pull2[F2, O2, B] = flatMap(_ => that)

    def map[B](f: A => B): Pull2[F, O, B] = flatMap(a => Pull2.Pure(f(a)))
  }

  implicit class Pull2UnitOps[F[_], O](private val self: Pull2[F, O, Unit]) {
    def flatMapOutput[O2](f: O => Pull2[F, O2, Unit]): Pull2[F, O2, Unit] =
      Pull2.FlatMapOutput(self, f)

    def uncons: Pull2[F, Nothing, Option[(fs2.Chunk[O], Pull2[F, O, Unit])]] =
      Pull2.Uncons(self)
  }

  class Stream2[F[_], O](val pull: Pull2[F, O, Unit]) {
    def map[O2](f: O => O2): Stream2[F, O2] =
      new Stream2(pull.flatMapOutput(o => Pull2.output(fs2.Chunk.singleton(f(o)))))

    def flatMap[O2](f: O => Stream2[F, O2]): Stream2[F, O2] =
      new Stream2(pull.flatMapOutput(o => f(o).pull))

    def ++(that: => Stream2[F, O]): Stream2[F, O] =
      new Stream2(pull >> that.pull)

    def repeat: Stream2[F, O] =
      this ++ repeat

    def repeatN(n: Int): Stream2[F, O] =
      if (n <= 0) Stream2.empty
      else this ++ repeatN(n - 1)

    def evalMap[A](f: O => F[A]): Stream2[F, A] =
      new Stream2(pull.flatMapOutput(o => Pull2.eval(f(o)).flatMap(o => Pull2.output(fs2.Chunk.singleton(o)))))

    def chunkMin(n: Int): Stream2[F, fs2.Chunk[O]] = {
      def go(
          accum: Chunk[O],
          p: Pull2[F, O, Unit]
      ): Pull2[F, Chunk[O], Unit] =
        p.uncons.flatMap {
          case None => Pull2.output(fs2.Chunk.singleton(accum))
          case Some((hd, tl)) =>
            val newAccum = accum ++ hd
            if (newAccum.size >= n) Pull2.output(fs2.Chunk.singleton(newAccum)) >> go(Chunk.empty, tl)
            else go(newAccum, tl)
        }

      new Stream2(go(Chunk.empty, pull))
    }
  }

  object Stream2 {
    def apply[F[_], O](a: O): Stream2[F, O] = new Stream2(Pull2.output(fs2.Chunk.singleton(a)))

    def chunk[F[_], O](chunk: Chunk[O]): Stream2[F, O] = new Stream2(Pull2.output(chunk))

    def empty[F[_], O]: Stream2[F, O] = new Stream2(Pull2.Pure(()))
  }
}

class PullV2Test extends CatsEffectSuite {
  import Pull2._
  test("yoou") {
    val p = Stream2[IO, Int](1).repeatN(100000).chunkMin(100).evalMap(x => IO(x.size))

    fs2.Stream(1).covary[IO].repeatN(100000).chunkMin(100).evalMap(x => IO(x.size)).compile.drain.timed.flatMap { case (duration, _) =>
      IO(println(duration))
    } >> compile(p.pull, ())((_, _) => ()).void.timed.flatMap { case (duration, _) =>
      IO(println(duration))
    }
  }
}
