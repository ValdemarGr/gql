package gql.server.interpreter

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

    type Effect[F[_], A] = Pull[F, Nothing, (Chain[(JsonPath, Json)], Hierarchy[Stream[F, A]])]
    type Collect[F[_]] = PlanEval[Effect[F, *], NodeInfo, QueryPlan]
    type PlanEvalEffect[F[_]] = Effect[F, Collect[F]]

    final case class Hierarchy[A](children: List[(A, Hierarchy[A])])

    trait Node[F[_], A] {
      def eval(a: A, plan: QueryPlan): PlanEvalEffect[F]
    }

    def evalStreamNode[A](
        stream: Stream[IO, A],
        child: Node[IO, A],
        plan: QueryPlan,
        nodeInfo: NodeInfo
    ): PlanEvalEffect[IO] =
      stream.pull.uncons1.flatMap {
        case None => Pull.eval(IO.never)
        case Some((hd, updates)) =>
          val tl = updates.map(a => PlanEval(nodeInfo, child.eval(a, _)))
          child.eval(hd, plan).map { case (jsons, childUpdates) =>
            jsons -> Hierarchy(List(tl -> childUpdates))
          }
      }

    /*
     * 
     */
    def registerHierachy(
      path: Set[Unique.Token],
      h: Hierarchy[Stream[IO, Collect[IO]]],
      chan: IO[Channel[IO, Collect[IO]]]
    ) = {
      h.children.map{ case (col, child) => 
        IO.unique.map{ tok =>
          val newPath = path + tok
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
  }
}
