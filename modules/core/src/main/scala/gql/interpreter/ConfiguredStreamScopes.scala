package gql.interpreter

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import cats.effect.std._
import fs2.{Chunk, Stream}
import fs2.concurrent.SignallingRef
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
    Stream.resource(Supervisor[F]).flatMap { sup =>
      Stream.eval(StreamScopes[F, (A, Boolean)](takeOne)).flatMap { scopes =>
        Stream.eval(SignallingRef[F, Chunk[(Scope[F], (A, Boolean))]](Chunk.empty)).flatMap { sr =>
          def consumeChunk: F[List[(Scope[F], A)]] =
            sr.discrete
              .filter(_.nonEmpty)
              .take(1)
              .compile
              .lastOrError
              .flatMap { _ =>
                sr.modify { xs =>
                  val asLst = xs.toList
                  // First figure out what events we can handle now and what to ignore
                  val g = xs.toList.groupMap { case (s, (_, b)) => (s.id, b) } { case (s, (a, _)) => (s, a) }.toList
                  val (sigs, seqs) = g.partitionEither {
                    case ((_, true), ss)  => Left(ss)
                    case ((_, false), ss) => Right(ss)
                  }

                  val partedSigs = sigs.map(_.splitAt(xs.size - 1))

                  val relevantSigs = partedSigs.collect { case (_, x :: Nil) => x }
                  val outdatedSigsF = partedSigs.flatMap { case (ys, _) => ys }.parTraverse_ { case (s, _) => s.closeInParent }

                  // Then eliminiate dead events
                  // If a scope occurs as a child of another scope that was updated, then the child scope is outdated
                  val ids = asLst.map { case (s, _) => s.id }.toSet
                  @tailrec
                  def didParentUpdate(s: Scope[F]): Boolean =
                    if (ids.contains(s.id)) true
                    else
                      s.parent match {
                        case None    => false
                        case Some(p) => didParentUpdate(p)
                      }

                  val (outdatedSigs2, relevantSigs2) = relevantSigs.partitionEither {
                    case (s, _) if didParentUpdate(s) => Left(s)
                    case (s, a)                       => Right((s, a))
                  }
                  val outdatedSigs2F = outdatedSigs2.parTraverse_(_.closeInParent)

                  val (outdatedSeqs2, relevantSeqs2) = seqs
                    .collect { case x :: xs => (x, xs) }
                    .partitionEither {
                      case ((hd, _), _) if didParentUpdate(hd) => Left(hd)
                      case ((hd, a), tl)                       => Right((hd, a, tl))
                    }
                  val outdatedSeqs2F = outdatedSeqs2.parTraverse_(_.closeInParent)

                  val allOutdatedF = outdatedSigsF &> outdatedSigs2F &> outdatedSeqs2F
                  val allRelevant = relevantSigs2 ++ relevantSeqs2.map { case (s, a, _) => (s, a) }
                  val toPutBack = relevantSeqs2.flatMap { case (_, _, tl) => tl }
                  (Chunk.seq(toPutBack.map { case (s, a) => (s, (a, false)) }), sup.supervise(allOutdatedF).void as allRelevant)
                }.flatten
              }

          val pipedStreamF =
            scopes.changes
              .through(pipeF[(Scope[F], (A, Boolean))])
              .evalMap(xs => sr.update(_ ++ xs))

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
