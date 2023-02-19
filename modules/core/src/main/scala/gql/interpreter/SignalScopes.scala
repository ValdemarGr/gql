package gql.interpreter

import cats.effect.implicits._
import cats.effect._
import cats._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}
import cats.data._

// Offers a flat representation of a scope tree of streams
// It respects different consumption strategies such as "signal" and "sequential"
trait SignalScopes[F[_], A] {
  // Awaits the first element of the stream in the parent scope
  // and returns that element and the scope it was reserved in
  def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean, cursor: Cursor): F[(Scope[F], A)]

  // Filters dead events
  def unconsRelevantEvents: F[NonEmptyList[SignalScopes.LeasedValue[F, A]]]
}

object SignalScopes {
  final case class LeasedValue[F[_], A](
      scope: Scope[F],
      cursor: Cursor,
      value: A,
      signal: Boolean
  )

  sealed trait ScopeInfo[F[_]]
  object ScopeInfo {
    // We put cursor into second parameter list to avoid it being used in equality (optimization)
    final case class StreamInfo[F[_]](
        scope: Scope[F],
        signal: Boolean
    )(val cursor: Cursor)
        extends ScopeInfo[F]
    final case class ResourceInfo[F[_]](
        parent: StreamInfo[F],
        scope: Scope[F],
        index: Long
    ) extends ScopeInfo[F]
  }

  def apply[F[_], A: Doced](takeOne: Boolean, pipeF: PipeF[F], debug: DebugPrinter[F])(implicit
      F: Async[F]
  ): Stream[F, SignalScopes[F, A]] = {
    Stream.eval(Queue.bounded[F, Boolean](1)).flatMap { notifications =>
      Stream.eval(Queue.bounded[F, Chunk[ScopeInfo.ResourceInfo[F]]](if (takeOne) 1 else 1024)).flatMap { dataQ =>
        Stream.eval(F.ref[Chunk[ScopeInfo.ResourceInfo[F]]](Chunk.empty)).flatMap { sr =>
          def publish(f: Chunk[ScopeInfo.ResourceInfo[F]] => Chunk[ScopeInfo.ResourceInfo[F]]): F[Unit] =
            sr.update(f) *> notifications.offer(false)

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
                  val si = ScopeInfo.StreamInfo(parentScope, signal)(cursor)
                  def publish1(idx: Long, a: A, scope: Scope[F]): F[Unit] =
                    if (idx === 0L) head.complete((scope, a)).void
                    else dataQ.offer(Chunk.singleton(ScopeInfo.ResourceInfo(si, scope, idx)))

                  stream0.zipWithIndex
                    .evalMap { case (a, i) =>
                      F.deferred[Unit].flatMap { d =>
                        parentScope
                          .openChild(_ => Resource.onFinalize(d.complete(()).void))
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
                    .background
                }
                .flatMap {
                  case Some(_) => head.get
                  case None    => F.never /*
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
                      msgF.flatMap(msg => F.raiseError(new RuntimeException(msg)))*/
                }
            }
          }

          val unconsChunk =
            fs2.Stream
              .fromQueueUnterminated(dataQ)
              .through(pipeF[ScopeInfo.ResourceInfo[F]])
              .find(_.size > 0)
              .compile
              .lastOrError

          def reduceOpenResouces(xs: Chunk[ScopeInfo.ResourceInfo[F]]): F[List[ScopeInfo.ResourceInfo[F]]] = {
            // First figure out what events we can handle now and what to ignore
            // Group by parent scope, we wish to group children of the same parent
            val g = xs.toList
              .groupBy(_.parent)
              .toList

            // For signal events we throw away all but the last (most recent) event
            // For sequential events we use/uncons the first event and put the rest back
            val (sigs, seqs) = g.partitionEither {
              case (k, ss) if k.signal => Left(ss)
              case (_, ss)             => Right(ss)
            }
            val seqed = seqs.collect { case x :: xs => (x, xs) }

            val relevantSigs = sigs.mapFilter(_.lastOption)

            // Then filter dead scopes
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

            val allRelevant = relevantSigs ++ seqed.map { case (d, _) => d }

            // For all relevant events (resources), we must close old children of the parent scope (stream scope)
            val cleanupOldChildrenF = allRelevant.parTraverse_ { d =>
              d.scope.parent.traverse_ { parent =>
                // Children are always in reverse allocation order
                // We must drop everything older than the live child
                parent.children.flatMap { allChildren =>
                  val oldChildren = allChildren.dropWhile(_.id =!= d.scope.id).drop(1)
                  oldChildren.map(_.id).toNel.traverse_(parent.releaseChildren)
                }
              }
            }

            // Then we filter out all the dead scopes
            val filteredSigsF = relevantSigs.filterA(_.scope.isOpen)
            val filteredSeqsF = seqed.filterA { case (d, _) => d.scope.isOpen }

            cleanupOldChildrenF *> (filteredSeqsF, filteredSigsF).flatMapN { case (seqs, sigs) =>
              val headSeqs = seqs.map { case (hd, _) => hd }
              val prefix = Chunk.seq(seqs.flatMap { case (_, tl) => tl })
              // Invokes self cycle
              // This is inteded since these events are to be sequenced
              (if (prefix.nonEmpty) publish(prefix ++ _) else F.unit) as (sigs ++ headSeqs)
            }
          }

          ???
        }
      }
    }
    ???

  }
}
