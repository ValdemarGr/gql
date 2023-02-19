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
import cats._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}

// Offers a flat representation of a scope tree of streams
trait StreamScopes[F[_], A] {
  // Awaits the first element of the stream in the parent scope
  // and returns that element and the scope the element was reserved in
  // The scope's parent is always only the stream lifecycle scope
  def acquireAwait(stream: Stream[F, A], scope: Scope[F], cursor: Cursor): F[(Scope[F], A)]

  // Gets changes in the entire scope tree
  def changes: Stream[F, Chunk[(Scope[F], A, Cursor)]]

  // Debugging info
  def getPrintState: F[Map[Unique.Token, String]]
}

object StreamScopes {
  def apply[F[_], A](takeOne: Boolean)(implicit F: Async[F]): F[StreamScopes[F, A]] = {
    val qF = Queue.bounded[F, Chunk[(Scope[F], A, Cursor)]](if (takeOne) 1 else 1024)

    // Debugging state
    // Used to print the guts of the implementation if an error occurs
    F.ref(Map.empty[Unique.Token, Eval[String]]).flatMap { debugState =>
      def reserveDebugMapping(id: Unique.Token, name: Eval[String]): Resource[F, Unit] =
        Resource.make(debugState.update(_ + (id -> name)))(_ => debugState.update(_ - id))

      def reserveShowMapping[A: Show](id: Unique.Token, a: A): Resource[F, Unit] =
        reserveDebugMapping(id, Eval.later(a.show))

      val getPrintState0 = debugState.get.map(_.fmap(_.value))

      qF.map { q =>
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
        def acquireAwait0(stream: Stream[F, A], scope: Scope[F], cursor: Cursor): F[(Scope[F], A)] = {
          F.deferred[(Scope[F], A)].flatMap { head =>
            def publish(idx: Long, a: A, scope: Scope[F]): F[Unit] =
              if (idx === 0L) head.complete((scope, a)).void
              else q.offer(Chunk.singleton((scope, a, cursor)))

            val stream0 = if (takeOne) stream.take(1) else stream

            scope
              .openChild { parentScope =>
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
                            case Some((childScope, _)) => publish(i, a, childScope).as(fs2.Stream.eval(d.get))
                          }
                        }
                    }
                  }
                  .parJoinUnbounded
                  .compile
                  .drain
                  .background <* reserveShowMapping(parentScope.id, cursor)
              }
              .flatMap {
                case Some(_) => head.get
                case None =>
                  val rootScope = scope.root
                  val msgF = getPrintState0.flatMap { lookup =>
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

        new StreamScopes[F, A] {
          def acquireAwait(stream: Stream[F, A], scope: Scope[F], cursor: Cursor): F[(Scope[F], A)] =
            acquireAwait0(stream, scope, cursor)

          def changes: Stream[F, Chunk[(Scope[F], A, Cursor)]] = Stream.fromQueueUnterminated(q)

          def getPrintState: F[Map[Unique.Token, String]] = getPrintState0
        }
      }
    }
  }
}
