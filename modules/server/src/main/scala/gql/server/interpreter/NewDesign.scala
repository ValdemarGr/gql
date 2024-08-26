package gql.server.interpreter

import cats.implicits._
import cats.effect._
import fs2.Stream
import fs2.concurrent.Signal
import fs2.Pull

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

    final case class Collect[F[_]](
        nodeInfo: NodeInfo,
        collect: QueryPlan => Pull[F, Nothing, (List[(JsonPath, Json)], Stream[F, Collect[F]])]
    )

    trait Node[F[_], A] {
      def eval(a: A): Pull[F, Nothing, (List[(JsonPath, Json)], Stream[F, Collect[F]])]
    }

    def evalStreamNode[A](
      stream: Stream[IO, A], 
      child: Node[IO, A]
    ) = {
      stream.pull.uncons1.flatMap{
        case None => Pull.done
        case Some((hd, updates)) =>
          child.eval(hd).flatMap{ case (jsons, childUpdates) =>
            // tl is our updates
            // childUpdates is everything in our subtree
            // if tl updates, kill childUpdates
            ???
          }
      }
    }
  }
}
