package hest

import scala.concurrent.duration._
import cats.implicits._
import cats.effect.implicits._
import cats._
import cats.effect._
import munit.CatsEffectSuite
import fs2.Chunk

trait Arc[F[_]] {
  def lease: Resource[F, Boolean]

  // n.b this resource is safe to not release manually since Arc will release it when the Arc reaches 0 leases
  def attachResource[A](r: Resource[F, A]): Resource[F, Option[A]]
}

object Arc {
  final case class State[F[_]](
      activeLeases: Set[Int],
      finalizers: Map[Int, F[Unit]],
      nextId: Int
  ) {
    def acquireLease: (State[F], Int) =
      copy(activeLeases = activeLeases + nextId, nextId = nextId + 1) -> nextId

    def releaseLease(id: Int): (Option[State[F]], List[F[Unit]]) = {
      val without = activeLeases - id
      if (without.isEmpty) None -> finalizers.values.toList
      else Some(copy(activeLeases = without)) -> Nil
    }

    def attachFinalizer(fin: F[Unit]): (State[F], Int) =
      copy(finalizers = finalizers + (nextId -> fin), nextId = nextId + 1) -> nextId

    def releaseFinalizer(id: Int): (State[F], Option[F[Unit]]) =
      copy(finalizers = finalizers - id) -> finalizers.get(id)
  }

  def make[F[_]](implicit F: Concurrent[F]): Resource[F, Arc[F]] = {
    val init = 0
    val alloc = F.ref[Option[State[F]]](Some(State[F](Set(init), Map.empty, init + 1)))
    Resource.eval(alloc).flatMap { ref =>
      def releaseLease(id: Int) = F.uncancelable { _ =>
        ref
          .modify {
            case None    => (None, Nil)
            case Some(s) => s.releaseLease(id)
          }
          .flatMap(_.traverse(_.attempt).map(_.sequence_).rethrow)
      }

      def acquireLease = ref.modify {
        case None => (None, None)
        case Some(x) =>
          val (s, i) = x.acquireLease
          (Some(s), Some(i))
      }

      val useOne: Resource[F, Boolean] = Resource
        .make(acquireLease)(id => id.traverse_(releaseLease))
        .map(_.isDefined)

      Resource.onFinalize[F](releaseLease(init)).as {
        new Arc[F] {
          override def lease: Resource[F, Boolean] = useOne

          override def attachResource[A](r: Resource[F, A]): Resource[F, Option[A]] =
            Resource.uncancelable { poll =>
              poll(Resource.eval(r.allocated)).flatMap { case (a, release) =>
                val allocate: F[Option[Int]] = ref
                  .modify[F[Option[Int]]] {
                    case None => (None, release.as(None))
                    case Some(s) =>
                      val (s2, id) = s.attachFinalizer(release)
                      (Some(s2), F.pure(Some(id)))
                  }
                  .flatten
                Resource.eval(allocate).flatMap {
                  case None => Resource.eval(F.pure(None))
                  case Some(id) =>
                    Resource
                      .onFinalize(ref.modify {
                        case None => (None, F.unit)
                        case Some(s) =>
                          val (s2, fin) = s.releaseFinalizer(id)
                          (Some(s2), fin.sequence_)
                      }.flatten)
                      .as(Some(a))
                }
              }
            }
        }
      }
    }
  }
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
  final case class HandleErrorWith[F[_], O, A](
      fa: Pull2[F, O, A],
      f: Throwable => Pull2[F, O, A]
  ) extends Pull2[F, O, A]
  final case class SubArc[F[_], O, A](
      fa: Pull2[F, O, A]
  ) extends Pull2[F, O, A]
  final case class Acquire[F[_], A](r: Resource[F, A]) extends Pull2[F, Nothing, Option[A]]
  final case class Unfree[F[_]]() extends Pull2[F, Nothing, Concurrent[F]]
  final case class GetContext[F[_]]() extends Pull2[F, Nothing, Context[F]]
  final case class ReplaceContext[F[_]](ctx: Context[F]) extends Pull2[F, Nothing, Unit]
  // runs the stream until the next output or termination (A)
  // returns the next evaluation as F, such that more sophisticated algorithms can be implemented
  final case class Step[F[_], O, A](
      fa: Pull2[F, O, A]
  ) extends Pull2[F, Nothing, F[Either[A, (fs2.Chunk[O], Pull2[F, O, A])]]]

  def unfree[F[_]]: Pull2[F, Nothing, Concurrent[F]] = Unfree()

  def output[O](value: fs2.Chunk[O]): Pull2[Nothing, O, Unit] = Output(value)

  def output1[O](value: O): Pull2[Nothing, O, Unit] = output(fs2.Chunk.singleton(value))

  def eval[F[_], A](fa: F[A]): Pull2[F, Nothing, A] = Eval(fa)

  def acquire[F[_], A](r: Resource[F, A]): Pull2[F, Nothing, Option[A]] = Acquire(r)

  def step[F[_], O, A](
      fa: Pull2[F, O, A]
  ): Pull2[F, Nothing, F[Either[A, (fs2.Chunk[O], Pull2[F, O, A])]]] = Step(fa)
  // def step[F[_], O, A](fa: Pull2[F, O, A]): Pull2[F, Nothing, F[StepResult[F, O, A]]] = Step(fa)

  def getContext[F[_]]: Pull2[F, Nothing, Context[F]] = GetContext()

  val done: Pull2[Nothing, Nothing, Unit] = Pure(())

  final case class Context[F[_]](
      arc: Arc[F],
      parents: List[Arc[F]]
  )

  def compile[F[_], O, A, B](
      p: Pull2[F, O, A],
      init: B
  )(
      fold: (B, fs2.Chunk[O]) => B
  )(implicit F: Async[F]): F[(B, A)] = Arc.make[F].use { initArc =>
    val leftUnit = ().asLeft[Nothing]

    final case class Unconsed[O2, A2](
        ctx: Context[F],
        cont: Either[A2, (fs2.Chunk[O2], Pull2[F, O2, A2])]
    )

    def go[O2, A2](
        p: Pull2[F, O2, A2],
        ctx: Context[F]
    ): F[Unconsed[O2, A2]] = F.defer {
      p match {
        case Pure(a)           => F.pure(Unconsed(ctx, Left(a)))
        case eval: Eval[F, A2] => eval.fa.map(a => Unconsed(ctx, Left(a)))
        case o: Output[O2]     => F.pure(Unconsed(ctx, Right(o.value -> done)))
        case uncons: Uncons[F, o] =>
          go[o, Unit](uncons.fa, ctx).flatMap { d =>
            d.cont match {
              case Left(_) => F.pure(Unconsed(d.ctx, Left(None)))
              case Right((hd, tl)) => F.pure(Unconsed(d.ctx, Left(Some((hd, tl)))))
            }
          }
        case fm: FlatMap[F, O2, a, A2] =>
          go[O2, a](fm.fa, ctx).flatMap { out =>
            out.cont match {
              case Left(a)         => go(fm.f(a), out.ctx)
              case Right((hd, tl)) => F.pure(Unconsed(out.ctx, Right(hd -> FlatMap(tl, fm.f))))
            }
          }
        case _: Unfree[F] => F.pure(Unconsed(ctx, Left(F)))
        case he: HandleErrorWith[F, O2, A2] =>
          go[O2, A2](he.fa, ctx).attempt.flatMap {
            case Left(e) => go(he.f(e), ctx)
            case Right(d) =>
              d.cont match {
                case Left(_) => F.pure(d)
                case Right((hd, tl)) =>
                  F.pure(Unconsed(d.ctx, Right(hd -> HandleErrorWith(tl, he.f))))
              }
          }
        case _: GetContext[F]      => F.pure(Unconsed(ctx, Left(ctx)))
        case rc: ReplaceContext[F] => F.pure(Unconsed(rc.ctx, leftUnit))
        case sa: SubArc[F, O2, A2] =>
          F.uncancelable { poll =>
            poll(ctx.arc.attachResource(Arc.make[F]).map(_.get).allocated).flatMap { case (child, release) =>
              // the cancellation/exception handling is not adequate
              val ctx2 = ctx.copy(arc = child, parents = ctx.arc :: ctx.parents)
              val fa = go(sa.fa, ctx2).flatMap[Unconsed[O2, A2]] { d =>
                d.cont match {
                  case Left(a) => release.as(Unconsed(d.ctx, Left(a)))
                  case Right((hd, tl)) =>
                    val whenDone = eval(release) <* ReplaceContext(ctx)
                    F.pure(Unconsed(d.ctx, Right(hd -> (tl <* whenDone))))
                }
              }

              poll(fa)
            }
          }
        case ac: Acquire[F, a] =>
          ctx.arc.attachResource(ac.r).allocated.map { case (a, _) => Unconsed(ctx, Left(a)) }
        case s: Step[F, o, a] =>
          val run = go(s.fa, ctx)

          def find[O3, A3](d: Unconsed[O3, A3]): F[Either[A3, (fs2.Chunk[O3], Pull2[F, O3, A3])]] =
            d.cont match {
              case Left(a)         => F.pure(Left(a))
              case Right((hd, tl)) => F.pure(Right((hd, ReplaceContext(d.ctx) >> tl)))
            }

          F.pure(Unconsed(ctx, Left(run.flatMap(find))))
      }
    }

    def loop(
        d: Unconsed[O, A],
        b: B
    ): F[(B, A)] = d.cont match {
      case Left(a) => F.pure((b, a))
      case Right((hd, tl)) =>
        val b2 = fold(b, hd)
        go(tl, d.ctx).flatMap(loop(_, b2))
    }

    go(p, Context(initArc, Nil)).flatMap(x => loop(x, init))
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

    def evalMap[F2[x] >: F[x], A2](f: A => F2[A2]): Pull2[F2, O, A2] =
      flatMap(a => Pull2.eval(f(a)))

    def lease[F2[x] >: F[x]]: Pull2[F2, Nothing, Resource[F2, Arc[F2]]] =
      getContext[F2].flatMap { ctx =>
        unfree[F2].map { implicit F =>
          Arc.make[F2].flatTap(_.attachResource((ctx.arc :: ctx.parents).traverse_(_.lease)))
        }
      }

    def uncons: Pull2[F, Nothing, Option[(fs2.Chunk[O], Pull2[F, O, A])]] =
      step[F, O, A](self).flatMap(eval(_)).map(_.toOption)
  }

  implicit class Pull2UnitOps[F[_], O](private val self: Pull2[F, O, Unit]) {
    // def uncons: Pull2[F, Nothing, Option[(fs2.Chunk[O], Pull2[F, O, Unit])]] =
    //   Pull2.Uncons(self)
  }

  class Stream2[F[_], O](val pull: Pull2[F, O, Unit]) {
    def map[O2](f: O => O2): Stream2[F, O2] =
      new Stream2(
        pull.uncons.flatMap {
          case None => done
          case Some((hd, tl)) =>
            output(hd.map(f)) >> new Stream2(tl).map(f).pull
        }
      )

    def flatMap[O2](f: O => Stream2[F, O2]): Stream2[F, O2] =
      new Stream2(
        pull.uncons.flatMap {
          case None => done
          case Some((hd, tl)) =>
            def go(xs: List[O]): Pull2[F, O2, Unit] =
              xs match {
                case Nil     => new Stream2(tl).flatMap(f).pull
                case x :: xs => f(x).pull >> go(xs)
              }
            go(hd.toList)
        }
      )

    def ++(that: => Stream2[F, O]): Stream2[F, O] =
      new Stream2(pull.flatMap(_ => that.pull))

    def repeat: Stream2[F, O] =
      this ++ repeat

    def repeatN(n: Int): Stream2[F, O] =
      if (n <= 0) Stream2.empty
      else this ++ repeatN(n - 1)

    def evalMap[A](f: O => F[A]): Stream2[F, A] =
      new Stream2(
        unfree[F].flatMap { implicit F =>
          pull.uncons.flatMap {
            case None => done
            case Some((hd, tl)) =>
              eval(hd.traverse(f)).flatMap(output[A](_)) >> new Stream2(tl).evalMap(f).pull
          }
        }
      )

    def chunkMin(n: Int): Stream2[F, fs2.Chunk[O]] = {
      def go(
          accum: Chunk[O],
          p: Pull2[F, O, Unit]
      ): Pull2[F, Chunk[O], Unit] =
        p.uncons.flatMap {
          case None => Pull2.output1(accum)
          case Some((hd, tl)) =>
            val newAccum = accum ++ hd
            if (newAccum.size >= n) Pull2.output1(newAccum) >> go(Chunk.empty, tl)
            else go(newAccum, tl)
        }

      new Stream2(go(Chunk.empty, pull))
    }

    def handleErrorWith(f: Throwable => Stream2[F, O]): Stream2[F, O] =
      new Stream2(Pull2.HandleErrorWith(pull, e => f(e).pull))

    def subArc: Stream2[F, O] =
      new Stream2(Pull2.SubArc(pull))

    def interruptWhen(p: F[Unit])(implicit F: Async[F]): Stream2[F, O] =
      new Stream2({
        step[F, O, Unit](pull).flatMap { fa =>
          eval(p.race(fa).map(_.flatten)).flatMap {
            case Left(_) => done
            case Right((hd, tl)) =>
              output(hd) >> new Stream2(tl).interruptWhen(p).pull
          }
        }
      }).subArc
  }

  object Stream2 {
    def apply[F[_], O](a: O): Stream2[F, O] = new Stream2(output1(a))

    def chunk[F[_], O](chunk: Chunk[O]): Stream2[F, O] = new Stream2(Pull2.output(chunk))

    def empty[F[_], O]: Stream2[F, O] = new Stream2(Pull2.Pure(()))

    def resource[F[_], A](r: Resource[F, A]): Stream2[F, A] =
      new Stream2(
        acquire(r)
          .flatMap { o =>
            unfree[F].flatMap { implicit F =>
              o match {
                case None    => eval[F, Nothing](F.raiseError(new Exception("arc closed")))
                case Some(x) => output1(x)
              }
            }
          }
      )
  }
}

class PullV2Test extends CatsEffectSuite {
  import Pull2._

  def infSeq(n: Int): Stream2[IO, Int] =
    Stream2[IO, Int](n) ++ infSeq(n + 1)

  test("yoou") {
    val p = Stream2[IO, Int](1).repeatN(100000).chunkMin(100).evalMap(x => IO(x.size))

    IO.println("CPS") >>
      fs2.Stream(1).covary[IO].repeatN(100000).chunkMin(100).evalMap(x => IO(x.size)).compile.drain.timed.flatMap { case (duration, _) =>
        IO(println(duration))
      } >> IO.println("closure") >> compile(p.pull, ())((_, _) => ()).void.timed.flatMap { case (duration, _) =>
        IO(println(duration))
      }
  }

  test("err") {
    val p = Stream2[IO, Int](1)
      .repeatN(3)
      .evalMap(i => IO.raiseError[Unit](new Exception("boom")))
      .map(_ => Option.empty[Throwable])
      .handleErrorWith(e => Stream2[IO, Option[Throwable]](Some(e)))

    compile(p.pull, Chunk.empty[Option[Throwable]])((z, c) => z ++ c).map(println)
  }

  test("resource") {
    var n = 0
    val p = Stream2
      .resource(Resource.make(IO {
        println(s"opening $n")
        val x = n
        n = n + 1
        x
      })(x => IO.println(s"closing $x")))
      .subArc
      .repeatN(3)
      .flatMap { x =>
        Stream2
          .resource[IO, Int](
            Resource.make(IO.println(s"sub-resource for $x"))(_ => IO.println(s"sub-closing for $x")).as(x)
          )
          .subArc
      }
      .evalMap(x => IO.println(s"using $x"))

    compile(p.pull, ())((_, _) => ()).void
  }

  test("interruption") {
    IO.deferred[Unit].flatMap { d =>
      val p = infSeq(0).evalMap(x => IO.sleep(100.millis) >> IO.println(x)).interruptWhen(d.get)

      (IO.sleep(500.millis) >> d.complete(()).void).background.surround {
        compile(p.pull, ())((_, _) => ()).void
      }
    }
  }
}
