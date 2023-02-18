package gql.interpreter

import cats.effect.implicits._
import cats.effect._
import cats._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}
import cats.data._

// This is an abstraction on top of StreamScopes
// It respects different consumption strategies such as "signal" and "sequential"
trait ConfiguredStreamScopes[F[_], A] {
  // Awaits the first element of the stream in the parent scope
  // and returns that element and the scope it was reserved in
  def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean, cursor: Cursor): F[(Scope[F], A)]

  // Filters dead events
  def unconsRelevantEvents: F[NonEmptyList[ConfiguredStreamScopes.LeasedValue[F, A]]]
}

object ConfiguredStreamScopes {
  final case class LeasedValue[F[_], A](
      scope: Scope[F],
      cursor: Cursor,
      value: A,
      signal: Boolean
  )

  def apply[F[_], A: Show](takeOne: Boolean, pipeF: PipeF[F], debug: DebugPrinter[F])(implicit
      F: Async[F]
  ): Stream[F, ConfiguredStreamScopes[F, A]] = {
    Stream.eval(StreamScopes[F, (A, Boolean)](takeOne)).flatMap { scopes =>
      Stream.eval(Queue.bounded[F, Boolean](1)).flatMap { notifications =>
        Stream.eval(F.ref[Chunk[LeasedValue[F, A]]](Chunk.empty)).flatMap { sr =>
          def publish(f: Chunk[LeasedValue[F, A]] => Chunk[LeasedValue[F, A]]): F[Unit] =
            sr.update(f) *> notifications.offer(false)

          def debugShow(xs: Chain[LeasedValue[F, A]]) =
            scopes.getPrintState.flatMap { lookup =>
              xs.headOption.traverse(_.scope.root.string(lookup)).map(_.getOrElse("no tree")).flatMap { tree =>
                xs
                  .traverse(lv => lv.scope.isOpen tupleLeft lv)
                  .map { ys =>
                    s"${ys
                      .map { case (lv, open) =>
                        s"""* ${lv.cursor.show}
  name=${lookup.get(lv.scope.id).getOrElse(lv.scope.id.toString())}, 
  signal=${lv.signal}, 
  open=${open}, 
  className=${lv.value.show})
"""
                      }
                      .mkString_("")}for tree:\n$tree"
                  }

              }
            }

          def consumeChunk: F[NonEmptyList[LeasedValue[F, A]]] =
            fs2.Stream
              .fromQueueUnterminated(notifications, limit = 1)
              .evalMap(_ => sr.getAndSet(Chunk.empty))
              /*
               * Only keep the open scopes
               * This is necessary since closed scopes may have been submitted before they were closed
               * in the previous uncons:
               * submit1 -> uncons1 -> submit2 -> cleanup1 -> uncons2
               * If submit1 closed whatever was submitted in submit2, then we would have a dead event in uncons2
               */
              .evalTap(xs => debug.eval(debugShow(xs.toChain).map(s => s"got updates for:\n$s")))
              .evalMap(_.traverseFilter(d => d.scope.isOpen.ifF(d.some, none)))
              .evalMap { xs =>
                // First figure out what events we can handle now and what to ignore
                // Group by parent scope, we wish to group children of the same parent
                val g = xs.toList
                  .mapFilter(d => d.scope.parent.map(_.id) tupleRight d)
                  .groupMap { case (pid, d) => (pid, d.signal) } { case (_, d) => d }
                  .toList

                // For signal events we throw away all but the last (most recent) event
                // For sequential events we use/uncons the first event and put the rest back
                val (sigs, seqs) = g.partitionEither {
                  case ((_, true), ss)  => Left(ss)
                  case ((_, false), ss) => Right(ss)
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

                // Construct the prefix, such that they can be sequenced
                val prefixF = filteredSeqsF
                  .map(_.flatMap { case (_, tl) => tl })
                  .map(Chunk.seq)

                cleanupOldChildrenF *> (prefixF, filteredSigsF).flatMapN { case (prefix, sigs) =>
                  // Invokes self cycle
                  // This is inteded since these events are to be sequenced
                  (if (prefix.nonEmpty) publish(prefix ++ _) else F.unit) as sigs
                }
              }
              .evalTap(rs => debug.eval(debugShow(Chain.fromSeq(rs)).map(s => s"relevant updates for:\n$s")))
              .map(_.toNel)
              .unNone
              .head
              .compile
              .lastOrError

          val pipedStreamF =
            scopes.changes
              .through(pipeF[(Scope[F], (A, Boolean), Cursor)])
              .map(_.map { case (s, (a, b), c) => LeasedValue(s, c, a, b) })
              .evalMap(xs => publish(_ ++ xs))

          val alg =
            new ConfiguredStreamScopes[F, A] {
              override def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean, cursor: Cursor): F[(Scope[F], A)] =
                scopes.acquireAwait(stream.map(a => (a, signal)), scope, cursor).map { case (s, (a, _)) => (s, a) }

              override def unconsRelevantEvents: F[NonEmptyList[LeasedValue[F, A]]] = consumeChunk
            }

          fs2.Stream(alg).concurrently(pipedStreamF)
        }
      }
    }
  }
}
