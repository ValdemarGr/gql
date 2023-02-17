package gql.interpreter

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}
import scala.annotation.tailrec

// This is an abstraction on top of StreamScopes
// It respects different consumption strategies such as "signal" and "sequential"
trait ConfiguredStreamScopes[F[_], A] {
  // Awaits the first element of the stream in the parent scope
  // and returns that element and the scope it was reserved in
  def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean): F[(Scope[F], A)]

  // Filters dead events
  def unconsRelevantEvents: F[List[(Scope[F], A)]]
}

object ConfiguredStreamScopes {
  def apply[F[_], A](takeOne: Boolean, pipeF: PipeF[F])(implicit F: Async[F]): Stream[F, ConfiguredStreamScopes[F, A]] = {
    Stream.eval(StreamScopes[F, (A, Boolean)](takeOne)).flatMap { scopes =>
      Stream.eval(Queue.bounded[F, Boolean](1)).flatMap { notifications =>
        Stream.eval(F.ref[Chunk[(Scope[F], (A, Boolean))]](Chunk.empty)).flatMap { sr =>
          def publish(f: Chunk[(Scope[F], (A, Boolean))] => Chunk[(Scope[F], (A, Boolean))]): F[Unit] =
            sr.update(f) *> notifications.offer(false)

          // We use the signal for events
          // Then we uncons the events and filter out the dead ones
          def consumeChunk: F[List[(Scope[F], A)]] =
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
              .evalMap(_.traverseFilter { case d @ (s, _) => s.isOpen.ifF(d.some, none) })
              .evalMap { xs =>
                // First figure out what events we can handle now and what to ignore
                // Group by parent scope, we wish to group children of the same parent
                val g = xs.toList
                  .mapFilter { case d @ (s, _) => s.parent.map(_.id) tupleRight d }
                  .groupMap { case (pid, (_, (_, b))) => (pid, b) } { case (_, (s, (a, _))) => (s, a) }
                  .toList
                val (sigs, seqs) = g.partitionEither {
                  case ((_, true), ss)  => Left(ss)
                  case ((_, false), ss) => Right(ss)
                }
                val seqed = seqs.collect { case x :: xs => (x, xs) }

                val relevantSigs = sigs.mapFilter(_.lastOption)

                // Then filter dead scopes
                // If a scope occurs as a child of another live scope that was also updated, then the child scope is outdated
                // A dead child scope will be closed automatically when the parent releases old children
                val liveIds = (
                  sigs.flatMap(_.map { case (s, _) => s.id }) ++ seqed.map { case ((s, _), _) => s.id }
                ).toSet

                @tailrec
                def didParentUpdate_(s: Scope[F]): Boolean =
                  if (liveIds.contains(s.id)) true
                  else
                    s.parent match {
                      case None    => false
                      case Some(p) => didParentUpdate_(p)
                    }

                def didParentUpdate(s: Scope[F]): Boolean =
                  s.parent.fold(false)(didParentUpdate_)

                val relevantSigs2 = relevantSigs.filter { case (s, _) => !didParentUpdate(s) }

                // If head is no longer live, then we must close it
                val relevantSeqs2 = seqed.filter { case ((hd, _), _) => !didParentUpdate(hd) }

                val allRelevant = relevantSigs2 ++ relevantSeqs2.map { case ((s, a), _) => (s, a) }

                // For all relevant events, we must close old children of the parent scope
                val cleanupOldChildrenF = allRelevant.parTraverse_ { case (liveChild, _) =>
                  liveChild.parent.traverse_ { parent =>
                    // Children are always in reverse allocation order
                    // We must drop everything older than the live child
                    parent.children.flatMap { allChildren =>
                      val oldChildren = allChildren.dropWhile(_.id =!= liveChild.id).drop(1)
                      oldChildren.map(_.id).toNel.traverse_(parent.releaseChildren)
                    }
                  }
                }

                val toPutBack = relevantSeqs2.flatMap { case (_, tl) => tl }

                val prefix = Chunk.seq(toPutBack.map { case (s, a) => (s, (a, false)) })

                // Invokes self cycle
                // This is inteded since these events are to be sequenced
                (if (prefix.nonEmpty) publish(prefix ++ _) else F.unit) &>
                  cleanupOldChildrenF as allRelevant
              }
              .filter(_.nonEmpty)
              .take(1)
              .compile
              .lastOrError

          val pipedStreamF =
            scopes.changes
              .through(pipeF[(Scope[F], (A, Boolean))])
              .evalMap(xs => publish(_ ++ xs))

          val alg =
            new ConfiguredStreamScopes[F, A] {
              override def acquireAwait(stream: Stream[F, A], scope: Scope[F], signal: Boolean): F[(Scope[F], A)] =
                scopes.acquireAwait(stream.map(a => (a, signal)), scope).map { case (s, (a, _)) => (s, a) }

              override def unconsRelevantEvents: F[List[(Scope[F], A)]] = consumeChunk
            }

          fs2.Stream(alg).concurrently(pipedStreamF)
        }
      }
    }
  }
}
