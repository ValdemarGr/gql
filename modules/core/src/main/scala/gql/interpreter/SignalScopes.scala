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
package gql.interpreter

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import fs2.{Chunk, Stream}
import cats.data._
import cats._

import gql.interpreter.BackpressureSignal
import org.typelevel.paiges.Doc
import scala.concurrent.duration.FiniteDuration
// Offers a flat representation of a scope tree of streams
// It respects different consumption strategies such as "signal" and "sequential"
trait SignalScopes[F[_], A] {
  // Awaits the first element of the stream in the parent scope
  // and returns that element and the scope it was reserved in
  def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean, cursor: Cursor): F[(Scope[F], A)]

  // Filters dead events
  def unconsRelevantEvents: F[NonEmptyList[SignalScopes.ResourceInfo[F, A]]]
}

object SignalScopes {
  final case class StreamInfo[F[_]](
      scope: Scope[F],
      signal: Boolean,
      cursor: Cursor
  )

  final case class ResourceInfo[F[_], A](
      value: A,
      parent: StreamInfo[F],
      scope: Scope[F],
      index: Long
  )

  /*
    What we need is:
   * A concurrent accumulation data structure with bounded size (backpressure when full)
   * Uncons all (unblock all blocked pushers)
   */
  def apply[F[_], A: Doced](takeOne: Boolean, debug: DebugPrinter[F], accumulate: Option[FiniteDuration], root: Scope[F])(implicit
      F: Async[F]
  ): F[SignalScopes[F, A]] = {

    // Debugging state
    // Used to print the guts of the implementation if an error occurs
    F.ref(Map.empty[Unique.Token, Eval[String]]).flatMap { debugState =>
      def reserveDebugMapping(id: Unique.Token, name: Eval[String]): Resource[F, Unit] =
        Resource.make(debugState.update(_ + (id -> name)))(_ => debugState.update(_ - id))

      def reserveShowMapping[A: Show](id: Unique.Token, a: => A): Resource[F, Unit] =
        reserveDebugMapping(id, Eval.later(a.show))

      val getPrintState = debugState.get.map(_.fmap(_.value))

      def printTreeF = getPrintState >>= root.string

      def printElems(xs: List[ResourceInfo[F, A]], D: Doced[A]): F[String] =
        xs
          .traverse(x => x.scope.isOpen tupleLeft x)
          .flatMap { ys =>
            getPrintState.map { lookup =>
              DebugPrinter.Printer
                .fields(
                  ys.map { case (r, open) =>
                    DebugPrinter.Printer.fields(
                      DebugPrinter.Printer.resourceInfoDoced(open, lookup)(D).apply(r)
                    )
                  }: _*
                )
                .tightBracketBy(Doc.char('['), Doc.char(']'))
                .render(80)
            }
          }

      BackpressureSignal[F, Map[Unique.Token, ResourceInfo[F, A]], ResourceInfo[F, A]](
        Map.empty,
        { (m, v) =>
          val k = v.parent.scope.id
          m.get(k) match {
            case Some(_) if (!v.parent.signal) => None
            case _                             => Some(m + (k -> v))
          }
        }
      )
        .map { bps =>
          def unconsRelevantEvents0 =
            debug.eval(printTreeF.map(tree => s"unconsing with current tree:\n$tree")) >>
              bps.listen
                .use { sig =>
                  sig.awaitNonEmpty >>
                    accumulate.traverse_(F.sleep(_)) >>
                    fs2.Stream
                      .repeatEval(sig.uncons)
                      .map(xs => Chunk.seq(xs.values.toList))
                      .evalTap(cs => debug.eval(printElems(cs.toList, Doced[A]).map(s => s"unconsed:\n$s")))
                      .evalTap { xs =>
                        /*
                         * Consider the following pathelogical scope tree:
                         *
                         *                  stream1
                         *               __/       \__
                         *              /             \
                         *             /               \
                         *        resource1         resource2
                         *       /        \
                         *   stream2    stream3
                         *      |          |___________
                         *      |          |           |
                         *  resource3  resource4   resource5
                         *
                         * Notice that "stream"s cannot enter the scope dynamically (concurrently);
                         * They enter the scope tree when discovered during interpretation.
                         *
                         * Note that children of a "stream", resources, are homogenous, they are versions of the same datatype.
                         * "Stream"s (children of resources) are heterogeneous, but their size is not statically known (graphql lists and such).
                         *
                         * A new version of a resource will close older resources of the same type (that is, all it's parent's children older than itself).
                         */
                        // For all relevant events (resources), we must close old children of the parent scope (stream scope)
                        xs.parTraverse_ { ri =>
                          // Children are always in reverse allocation order
                          // We must drop everything older than the live child
                          ri.parent.scope.children.flatMap { allChildren =>
                            val oldChildren = allChildren.dropWhile(_.id =!= ri.scope.id).drop(1)
                            oldChildren.map(_.id).toNel.traverse_(ri.parent.scope.releaseChildren)
                          }
                        }
                      }
                      .evalMap(_.filterA(_.scope.isOpen))
                      .evalTap { cs =>
                        debug.eval {
                          printElems(cs.toList, Doced.from[A](_ => Doc.text("ditto")))
                            .map(s => s"unconsed after removing old children:\n$s")
                        }
                      }
                      .evalTap(_ => debug.eval(printTreeF.map(tree => s"tree after unconsing:\n$tree")))
                      .filter(_.nonEmpty)
                      .map(_.toNel)
                      .unNone
                      .evalTap(nel => debug(s"emitting ${nel.size} elements from uncons"))
                      .head
                      .compile
                      .lastOrError
                }

          /*
           * A new scope must be reserved for the stream
           *
           * For instance, let the stream is leased on the ambient scope:
           *      scope
           *     /     \
           *  parent  thisStream
           *         /          \
           *     emission1   emission2
           *
           * Since parent and thisStream are siblings, their releases are not ordered.
           * If opening "thisStream" depends on the resource lease of "parent" then this strategy is not sufficient.
           * We must allocate a scope just for "thisStream" and then open a child scope for each emission:
           *         scope
           *           |
           *         parent
           *           |
           *       thisStream
           *      /          \
           *  emission1   emission2
           */
          def acquireAwait0(stream: Stream[F, A], scope: Scope[F], signal: Boolean, cursor: Cursor): F[(Scope[F], A)] = {
            F.deferred[(Scope[F], A)].flatMap { head =>
              val stream0 = if (takeOne) stream.take(1) else stream

              scope
                .openChild { parentScope =>
                  val si = StreamInfo(parentScope, signal, cursor)
                  def publish1(idx: Long, a: A, scope: Scope[F]): F[Unit] =
                    if (idx === 0L) head.complete((scope, a)).void
                    else bps.publish(ResourceInfo(a, si, scope, idx))

                  stream0.zipWithIndex
                    .evalMap { case (a, i) =>
                      F.deferred[Unit].flatMap { d =>
                        parentScope
                          .openChild { scope =>
                            Resource.onFinalize(d.complete(()).void) >>
                              reserveShowMapping(scope.id, s"resource-$i")
                          }
                          .flatMap { o =>
                            o match {
                              // This is totally legal, maybe someone shut us down while we are emitting
                              case None                  => F.pure(fs2.Stream[F, Unit]())
                              case Some((childScope, _)) => publish1(i, a, childScope).as(fs2.Stream.eval(d.get))
                            }
                          }
                      }
                    }
                    .parJoinUnbounded
                    .compile
                    .drain
                    .background <*
                    reserveShowMapping(
                      parentScope.id,
                      s"${cursor.show} (signal = $signal)"
                    )
                }
                .flatMap {
                  case Some(_) => head.get
                  case None =>
                    val rootScope = scope.root
                    val msgF = getPrintState.flatMap { lookup =>
                      rootScope.string(lookup).map { tree =>
                        show"""|Parent scope was not open at:
                             |$cursor
                             |This is likely to be a bug in the interpreter.
                             |Here is the current path of the closed scope:
                             |${scope.path.map(id => lookup.get(id).getOrElse(id.toString())).mkString_(" -> \n")}
                             |Here is the current scope tree:
                             |""".stripMargin + tree
                      }
                    }
                    msgF.flatMap(msg => F.raiseError(new RuntimeException(msg)))
                }
            }
          }

          new SignalScopes[F, A] {
            def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean, cursor: Cursor): F[(Scope[F], A)] =
              acquireAwait0(stream, scope, signal, cursor)

            def unconsRelevantEvents: F[NonEmptyList[ResourceInfo[F, A]]] = unconsRelevantEvents0
          }
        }
    }
  }
}
