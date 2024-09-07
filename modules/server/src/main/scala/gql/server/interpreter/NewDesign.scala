package gql.server.interpreter

import cats._
import cats.implicits._
import cats.effect._
import fs2.Stream
import fs2.concurrent.Signal
import fs2.Pull
import cats.effect.std.Queue
import cats.effect
import cats.data.Chain
import fs2.Chunk
import fs2.concurrent.Channel
import fs2.UnsafeFs2Access
import java.lang
import scala.collection.immutable

object NewDesign {
  object Pass1 {
    final case class TimeVaryingRoseTree[F[_], A](
        value: A,
        content: Stream[F, List[TimeVaryingRoseTree[F, A]]]
    )

    def flatten[F[_]: Async, A](tree: TimeVaryingRoseTree[F, A]): Stream[F, List[A]] =
      tree.content.flatMap { children =>
        val signalsF: Stream[F, List[Signal[F, List[A]]]] = children.traverse { child =>
          flatten[F, A](child).hold1
        }

        signalsF.flatMap { signals =>
          Stream
            .emits(signals)
            .covary[F]
            .map(_.discrete)
            .parJoinUnbounded
        }
      }

    trait JsonPath
    trait Json

    trait QueryPlan

    trait SubQuery[F[_]] {
      type A
      def sqi: SubQueryInfo
      def value: A
      def run(a: A, qp: QueryPlan): F[List[(JsonPath, Json)]]
    }
    object SubQuery {
      type Aux[F[_], A0] = SubQuery[F] { type A = A0 }
    }

    type QueryPlanner[F[_]] = SubQuery[F] => F[QueryPlan]

    trait SubQueryInfo

    final case class Task[F[_]](
        eval: QueryPlan => Stream[F, (List[(JsonPath, Json)], Task[F])]
    )

    type MyTree[F[_]] = TimeVaryingRoseTree[F, Task[F]]

    def plan[F[_]](sq: List[SubQueryInfo]): F[QueryPlan] = ???

    def execute[F[_], A](sq: SubQuery[F], qp: QueryPlan): F[List[(JsonPath, Json)]] = ???

    def go = {
      // tree gotten from recursively transforming the query into something executable
      val myTree: MyTree[IO] = ???

      // we then flatten the tree. Any update to any node will produce a new result
      val flattened: Stream[IO, List[Task[IO]]] = flatten[IO, Task[IO]](myTree)
    }

    // def taskifyStream[F[_], A](taskify: A => Task[F], stream: Stream[F, A]): Pull[F, Task[F], Unit] =
    //   stream.pull.uncons1.flatMap {
    //     case None => ???
    //     case Some((head, tail)) =>
    //       val h = taskify(head)
    //       Pull.output1[F, Task[F]](h) >> taskifyStream[F, A](taskify, tail)
    //   }
  }

  object Pass2 {
    trait QueryPlan

    trait JsonPath
    trait Json

    trait NodeInfo

    def plan(nis: List[NodeInfo]): QueryPlan = ???

    final case class PlanEval[F[_], A, B](
        plan: A,
        eval: B => F[PlanEval[F, A, B]]
    )

    type Effect[F[_], A] = Pull[F, Nothing, (Json, List[Fiber[F, Throwable, Unit]])]
    type Collect[F[_]] = PlanEval[Effect[F, *], NodeInfo, QueryPlan]
    type PlanEvalEffect[F[_]] = Effect[F, Collect[F]]

    final case class Hierarchy[A](children: List[(A, Hierarchy[A])])

    // trait Node[F[_], A] {
    //   def eval(a: A, plan: QueryPlan, lease: Resource[F, Unit]): PlanEvalEffect[F]
    // }

    // def evalStreamNode[A](
    //     stream: Stream[IO, A],
    //     child: Node[IO, A],
    //     plan: QueryPlan,
    //     nodeInfo: NodeInfo,
    //     parentLease: Resource[IO, Unit]
    // ): PlanEvalEffect[IO] =
    //   stream.pull.uncons1.flatMap {
    //     case None => Pull.eval(IO.never)
    //     case Some((hd, updates)) =>
    //       val tl = updates.map(a => PlanEval(nodeInfo, child.eval(a, _, parentLease)))
    //       UnsafeFs2Access.leaseScope[IO].flatMap { thislease =>
    //         child.eval(hd, plan, thislease).map { case (jsons, childUpdates) =>
    //           jsons -> ??? // Hierarchy(List(tl -> childUpdates))
    //         }
    //       }
    //   }

    final case class StateEntry[F[_]](
        // id of the node in the tree
        token: Unique.Token,
        parents: Set[Unique.Token],
        // the next evaluation informaiton
        collect: Collect[F],
        // to allow children to lease this node
        lease: Resource[F, Option[Int]]
    )

    final case class State[F[_]](
        // None if we are currently not consuming
        values: Option[List[StateEntry[F]]],
        // The next state, if we cannot publish values, await next state
        consumed: F[State[F]]
    )

    // def registerHierarchy(
    //     path: Set[Unique.Token],
    //     h: Hierarchy[Stream[IO, Collect[IO]]],
    //     state: Ref[IO, State[IO]]
    // ): IO[List[Fiber[IO, Throwable, Unit]]] = {
    //   h.children.map { case (col, child) =>
    //     IO.unique.map { tok =>
    //       val newPath = path + tok
    //       registerHierarchy(newPath, child, state).flatMap { children =>
    //         val cancelChildren: IO[Unit] = children.parTraverse_(_.cancel)

    //         val publishThisNode: IO[Unit] = {
    //           def rec(s: Stream[IO, Collect[IO]]): Pull[IO, Nothing, Unit] =
    //             s.pull.uncons1.flatMap {
    //               case None => Pull.done
    //               case Some((hd, tl)) =>
    //                 UnsafeFs2Access.leaseScope[IO].flatMap { leaseResource =>
    //                   // keep trying to submit hd
    //                   // every attempt we reserve the resource
    //                   def tryUpdate(poll: IO ~> IO): IO[Unit] =
    //                     poll(leaseResource.allocated).flatMap { case (_, release) =>
    //                       state.modify { s =>
    //                         s.values match {
    //                           // If we are not consuming, we await next and try again
    //                           case None =>
    //                             (s, release >> poll((s.consumed >> tryUpdate(poll))))
    //                           // If we are consuming, publish the value and await next consumption
    //                           case Some(xs) =>
    //                             (
    //                               s.copy(values = Some(StateEntry(tok, path, hd, release, leaseResource) :: xs)),
    //                               poll(s.consumed.void)
    //                             )
    //                         }
    //                       }.flatten
    //                     }

    //                   Pull.eval(IO.uncancelable(tryUpdate)) >> rec(tl)
    //                 }
    //             }

    //           rec(col).stream.compile.drain
    //         }
    //         ???
    //       }

    //       registerHierarchy(
    //         newPath,
    //         child,
    //         state
    //       ).map { children =>
    //         val killAll = children.parTraverse(_.cancel)
    //       }
    //     }
    //   }
    //   ???
    // }

    final case class Context[F[_]](
        sup: effect.std.Supervisor[F],
        path: Set[Unique.Token],
        state: Ref[F, State[F]],
        parentLease: Resource[F, Option[Int]],
        plan: QueryPlan,
        interruptContext: F[Unit]
    ) {
      def addPath(tok: Unique.Token): Context[F] = copy(path = path + tok)

      def setLease(lease: Resource[F, Option[Int]]): Context[F] = copy(parentLease = lease)

      def setPlan(qp: QueryPlan): Context[F] = copy(plan = qp)

      def setInterruptContext(interrupt: F[Unit]): Context[F] = copy(interruptContext = interrupt)
    }

    trait Cont[F[_], A] {
      def eval(a: A, ctx: Context[F]): PlanEvalEffect[F]
    }

    def visit[A](
        stream: Stream[IO, A],
        child: Cont[IO, A],
        nodeInfo: NodeInfo,
        ctx: Context[IO]
    ): Pull[IO, Nothing, (Json, List[Fiber[IO, Throwable, Unit]])] = {
      Pull.eval(IO.unique).flatMap { tok =>
        def mkCtx(isKilled: IO[Unit], leaseThis: Resource[IO, Option[Int]]): Context[IO] =
          ctx.addPath(tok).setInterruptContext(isKilled).setLease(leaseThis)

        val leaseTree = UnsafeFs2Access.leaseScope[IO].map[Resource[IO, Resource[IO, Option[Int]]]] { r =>
          ctx.parentLease.flatMap {
            case Some(_) => SharedResource.make[IO](r)
            case None =>
              Resource.raiseError[IO, Resource[IO, Option[Int]], Throwable] {
                new RuntimeException("impossible, no parent lease")
              }
          }
        }

        def resourcePull: Pull[IO, Nothing, Resource[IO, Option[Int]]] =
          leaseTree.flatMap(r => Stream.resource(r).pull.uncons1.map(_.get._1))

        def tryUpdate(lease: Resource[IO, Option[Int]], c: Collect[IO]): IO[Unit] =
          ctx.state.modify { s =>
            s.values match {
              // If we are not consuming, we await next and try again
              case None =>
                (s, (s.consumed >> tryUpdate(lease, c)))
              // If we are consuming, publish the value and await next consumption
              case Some(xs) =>
                (
                  s.copy(values = Some(StateEntry(tok, ctx.path, c, lease) :: xs)),
                  s.consumed.void
                )
            }
          }.flatten

        def repeatUncons(
            stream: Stream[IO, A],
            interruptPrevious: IO[Unit]
        ): Pull[IO, Nothing, Unit] =
          stream.pull.uncons1.flatMap {
            case None => Pull.done
            case Some((hd, tl)) =>
              for {
                _ <- Pull.eval(interruptPrevious)
                killThis <- Pull.eval(IO.deferred[Unit])
                leaseResource <- resourcePull
                c: Collect[IO] = PlanEval[Effect[IO, *], NodeInfo, QueryPlan](
                  plan = nodeInfo,
                  eval = qp => child.eval(hd, mkCtx(killThis.get, leaseResource).setPlan(qp))
                )

                _ <- Pull.eval(tryUpdate(leaseResource, c))

                _ <- repeatUncons(tl, killThis.complete(()).void)
              } yield ()
          }

        stream.pull.uncons1.flatMap {
          case None => Pull.eval(IO.never)
          case Some((hd, tl)) =>
            for {
              killThis <- Pull.eval(IO.deferred[Unit])
              leaseResource <- resourcePull
              (result, fibs) <- child.eval(hd, mkCtx(killThis.get, leaseResource))
              cancelAll = fibs.parTraverse_(_.cancel)
              bigProg = ctx.sup.supervise {
                repeatUncons(tl, cancelAll).stream
                  .interruptWhen(ctx.interruptContext.attempt)
                  .compile
                  .drain
              }
              ensureCleanup = ctx.sup.supervise(ctx.interruptContext *> cancelAll)
              fiber <- Pull.eval(ensureCleanup *> bigProg)
            } yield result -> List(fiber)
        }
      }
    }

    // pulle side:
    // 1. pull one
    // 2. eval child
    // 3. ensure that child is on this pull's lease
    // 4. ensure that our tail emission stops child emission
    //
    // pull side:
    // 1. pull one
    // 2. compute roots (all nodes that don't have a parent in the emitted nodes)
    // 3. plan query
    // 4. eval next pull for result
    // 5. do pull
    // 6. parjoin the new resulting streams
    //
    // this strategy closes child streams on the pulle side, but could this occur on the pull side?
    //

    /*
      1.
      scope = root
      stream = FlatMapOutput(
        InScope(Bind(
          Acquire(IO.println("open"), _ => IO.println("close"), false),
          x => Output(x)
        )),
        _ => Bind(_ => IO.println("use"), Eval(Output(Chunk(()))))
      )
      runner = OuterRun

      2. go then viewL then match
      scope = root
      stream = InScope(Bind(
        Acquire(IO.println("open"), _ => IO.println("close"), false),
        x => Output(x)
      ))
      // FlatMapOutput implies the FlatMapR runner
      runner = FlatMapR(
        view=IdContP,
        fun=_ => Bind(_ => IO.println("use"), Eval(Output(Singleton(())))),
        runner=OuterRunner
      )
      // since FlatMapOutput <: Action then getCont = IdContP
      contP = IdContP

      3. recurse then viewL then match then goInScope
      scope = root -> child
      // goInScope creates a Bind node over the pull of child and adds endScope as the continuation
      stream = Bind(Bind(
        Acquire(IO.println("open"), _ => IO.println("close"), false),
        x => Output(x)
      ), r => endScope(child, r))
      runner = ViewRunner(
        view=IdContP,
        runner=FlatMapR(
          IdContP,
          _ => Bind(_ => IO.println("use"), Eval(Output(Singleton(())))),
          runner=OuterRunner
        )
      )

      4. recurse then viewL
      viewL = FlatMapOutput(
        Acquire(IO.println("open"), _ => IO.println("close"), false),
        x => Output(x)
      )
      getCont = Bind(..., r => endScope(child, r))
      scope = root -> child
      runner = ViewRunner(
        IdContP,
        runner=FlatMapR(
          IdContP,
          _ => Bind(_ => IO.println("use"), Eval(Output(Singleton(())))),
          runner=OuterRunner
        )
      )

      5. match
      scope = root -> child
      stream = Acquire(IO.println("open"), _ => IO.println("close"), false)
      runner = FlatMapR(
        view = Bind(..., r => endScope(child, r)),
        fun=x => Output(x),
        runner = ViewRunner(
          IdContP,
          runner=FlatMapR(
            IdContP,
            _ => Bind(_ => IO.println("use"), Eval(Output(Singleton(())))),
            runner=OuterRunner
          )
        )
      )

      6. recurse then viewL then match then> goAcquire (attach bracket to child scope)
      scope = root -> child
      stream = Succeeded(()) // since IdContP(Succeeded(())) = Succeeded(())
      runner = FlatMapR(
        view = (r => endScope(child, r)),
        fun = x => Output(x),
        runner = ViewRunner(
          IdContP,
          runner=FlatMapR(
            IdContP,
            _ => Bind(_ => IO.println("use"), Eval(Output(Singleton(())))),
            runner=OuterRunner
          )
        )
      )

      7. recurse then viewL then match then (runner: FlatMapR).done(child)
      scope = root -> child
      runner.done(child) = go(
        child,
        None,
        F ~> F,
        ViewRunner(
          IdContP,
          runner=FlatMapR(
            IdContP,
            _ => Bind(_ => IO.println("use"), Eval(Output(Singleton(())))),
            runner=OuterRunner
          )
        ),
        // FlatMapR's view(unit) where view=Bind(..., r => endScope(child, r))
        SucceedScope(child.id)
      )

      8. recurse then viewL then goCloseScope(SucceedScope(child.id), IdContP)
      we hit the `case Some(toClose) => ...` case such that child.close
      and go(child.openAncestor, None, F ~> F, sameRunnerAsLast, unit)

      I suppose that child should be closed now and "close" should appear in the console
      However, the resource is alive when "use" is printed?
     */

    Stream
      .bracketWeak(IO.println("open"))(_ => IO.println("close"))
      .scope
      .evalMap(_ => (IO.println("use")))
      .compile
      .drain

    Stream
      .bracket(IO.println("open"))(_ => IO.println("close"))
      .repeatN(4)
      .pull
      .unconsN(2)
      .flatMap {
        case None => Pull.done
        case Some((hd, tl)) =>
          Pull.eval(IO.println(s"pull of size ${hd.size}")) >>
            Pull.output(hd) >> tl.pull.echo
      }
      .stream
      .evalMap(_ => IO.println("done"))
      .compile
      .drain

    def unconsNScoped[F[_]: Async, O](n: Int)(stream: Stream[F, O]): Stream[F, Chunk[O]] = {
      def go(remaining: Int, accum: Chunk[O], tl: Stream[F, O], leases: List[F[Unit]]): Pull[F, Chunk[O], Unit] =
        tl.pull.uncons1.flatMap {
          case None => Pull.output1(accum) >> Pull.eval(leases.sequence_)
          case Some((hd, tl)) =>
            Pull.extendScopeTo(Stream.unit.covary[F]).map(_.compile.drain).flatMap { killSwitch =>
              val allLeases = killSwitch :: leases
              val newChunk = accum ++ Chunk(hd)
              if (remaining == 1) {
                Pull.output1(newChunk) >> Pull.eval(allLeases.sequence_) >> go(n, Chunk.empty, tl, Nil)
              } else {
                go(remaining - 1, newChunk, tl, allLeases)
              }
            }
        }

      go(n, Chunk.empty, stream, Nil).stream
    }

    unconsNScoped(2) {
      Stream.bracket(IO.println("open"))(_ => IO.println("close"))
    }.pull.uncons1
      .flatMap {
        case None => Pull.done
        case Some((hd, tl)) =>
          Pull.eval(IO.println(s"pull of size ${hd.size}")) >>
            Pull.output(hd) >> tl.pull.echo
      }
      .stream
      .evalMap(_ => IO.println("done"))
      .compile
      .drain

    Stream
      .resource(Resource.make(IO.println("open"))(_ => IO.println("close")))
      .repeatN(5)
      .pull
      .unconsN(2)
      .flatMap {
        case None => Pull.done
        case Some((hd, tl)) =>
          println(s"pull of size ${hd.size}")
          Pull.output1(0)
      }
      .stream
      .evalMap(_ => IO.println("use"))
      .compile
      .drain

    Stream
      .resource(Resource.make(IO.println("open"))(_ => IO.println("close")))
      .repeatN(5)
      .pull
      .uncons
      .flatMap {
        case None => Pull.done
        case Some((hd, tl)) =>
          println(s"pull of size ${hd.size}")
          Pull.output1(0)
      }
      .stream
      .evalMap(_ => IO.println("use"))
      .compile
      .drain

    Stream
      .bracket(IO.println("open"))(_ => IO.println("close"))
      .repeatN(4)
      .repeatPull(_.uncons1.flatMap {
        case None => Pull.pure(None)
        case Some((hd, tl)) =>
          println("pulled")
          Pull.output(Chunk(hd, hd)).as(Some(tl))
      })
      .evalMap(_ => IO.println("done"))
      .compile
      .drain

    def res(n: String): Stream[IO, Unit] =
      Stream.bracket(IO.println(s"open $n"))(_ => IO.println(s"close $n"))

    import scala.concurrent.duration._
    Stream
      .emits(0 to 5)
      .covary[IO]
      .flatMap(i => res("outer" + i.toString).as(i))
      .meteredStartImmediately(1.seconds)
      .switchMap { o =>
        Stream
          .emits(0 to 5)
          .covary[IO]
          .flatMap(i => res("inner" + i.toString + " of outer" + o.toString))
          .meteredStartImmediately(150.millis)
      }
      .compile
      .drain

    Stream
      .bracket(IO.println("open"))(_ => IO.println("close"))
      .repeatN(4)
      .zipWithIndex
      .map { case (_, i) => i }
      .pull
      .uncons1
      .flatMap(Pull.output1(_))
      .streamNoScope
      .flatMap {
        case None => Stream.empty
        case Some((hd, tl)) =>
          Stream.eval(IO.println(s"unconsed $hd")) >> tl
      }
      .evalMap { i => IO.println(s"use $i") }
      .compile
      .drain

    (
      Stream
        .emits((0 to 10))
        .covary[IO]
        .flatTap(i => Stream.bracket(IO.println(s"open $i"))(_ => IO.println(s"close $i")))
        .pull
        .uncons1
        .flatMap {
          case None => Pull.done
          case Some((hd, tl)) =>
            Pull.extendScopeTo(Stream.unit.covary[IO]).map(_.compile.drain).flatMap { kill =>
              Pull.eval(IO.println(s"sleeping")) >>
                Pull.eval(IO.sleep(1.second).onCancel(IO.println("cancelled"))) >>
                Pull.eval(IO.println(s"wake up, kill")) >>
                Pull.eval(kill) >>
                Pull.eval(IO.println(s"killed")) >> tl.pull.echo
            }
        }
        .stream
        .evalMap(x => IO.println(s"using $x"))
        .interruptWhen(IO.sleep(500.millis) >> IO.println("cancelling").as(().asRight[Throwable])) ++
        Stream.eval(IO.println("whee")).repeatN(2).meteredStartImmediately(1.second)
    ).compile.drain

    Stream
      .emits((0 to 10))
      .covary[IO]
      .flatTap(i => Stream.bracket(IO.println(s"open $i"))(_ => IO.println(s"close $i")))
      .interruptWhen(IO.never[Unit].attempt)
      .pull
      .uncons1
      .flatMap {
        case None           => Pull.pure(None)
        case Some((hd, tl)) => Pull.pure(Some(hd -> tl))
      }
      .flatMap(xs => Pull.output(Chunk.fromOption(xs)))
      .stream
      .evalMap(x => IO.println(s"using $x"))
      .compile
      .drain

    {
      def rec(x: Stream[IO, Int]): Pull[IO, Int, Unit] =
        x.pull.uncons1.flatMap {
          case None           => Pull.done
          case Some((hd, tl)) => Pull.eval(IO.println(s"using $hd")).flatMap(_ => rec(tl))
        }
      rec {
        Stream
          .emits((0 to 10))
          .covary[IO]
          .flatTap(i => Stream.bracket(IO.println(s"open $i"))(_ => IO.println(s"close $i")))
      }.stream
        .evalMap(x => IO.println(s"using $x"))
        .compile
        .drain
    }

    Stream
      .emits((0 to 10))
      .covary[IO]
      .flatTap(i => Stream.bracket(IO.println(s"open $i"))(_ => IO.println(s"close $i")))
      .pull
      .uncons1
      .flatMap {
        case None => Pull.done
        case Some((hd, tl)) =>
          val tlProg = Pull.eval(IO.println(s"using $hd")) >>
            Stream.emits((0 to 10)).covary[IO].evalMap(i => IO.println(s"inner $i").as(i)).pull.echo >>
            tl.pull.echo

          Stream
            .bracketWeak(IO.println(s"inner bracket open"))(_ => IO.println(s"inner bracket close"))
            .pull
            .uncons1
            .flatMap(_ => tlProg)
      }
      .stream
      .evalMap(x => IO.println(s"using $x"))
      .compile
      .drain
  }
}
