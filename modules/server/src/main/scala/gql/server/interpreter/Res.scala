package gql.server.interpreter

import cats.implicits.*
import cats.effect.*
import cats.effect.std.Semaphore
import cats.effect.implicits.*

import Res.*
trait Res[F[_]] {
  // deadlock free
  def lease[A](fa: Resource[F, A]): Resource[F, Option[(Lease, A)]]

  def leaseNow[A](fa: Resource[F, A]): F[Boolean]

  def keepAlive: Resource[F, Boolean]

  def release(lease: Lease): F[Unit]
}

object Res {
  final case class Lease(token: Unique.Token)

  // uses the semaphore to figure out if resource is closed
  def make[F[_]](implicit F: Async[F]): Resource[F, Res[F]] = {
    val permits = Int.MaxValue
    for {
      sem <- Resource.eval(Semaphore[F](permits.toLong))
      state <- Resource.eval(Ref[F].of(Map.empty[Lease, F[Unit]]))
      _ <- Resource.onFinalize {
        sem.acquireN(permits.toLong) >>
          state.modify { s =>
            (Map.empty, s.values.toList.parSequence_)
          }.flatten
      }
      api = new Res[F] {
        override def lease[A](fa: Resource[F, A]): Resource[F, Option[(Lease, A)]] =
          Resource.eval(F.unique.map(Lease(_))).flatMap { id =>
            // 1 lease, blocks global cleanup, shared lock, use lock to decide if closed
            sem.tryPermit.evalMap {
              case false => F.pure(Option.empty[(Lease, A)]) // closed
              case true =>
                F.uncancelable { poll =>
                  poll(fa.allocated).flatMap { case (a, free) =>
                    state
                      .modify { m =>
                        (m + (id -> free), F.unit)
                      }
                      .flatten
                      .as(Option((id, a)))
                  }
                }
            }
          }

        def leaseNow[A](fa: Resource[F, A]): F[Boolean] =
          lease(fa).use {
            case Some(_) => F.pure(true)
            case None    => F.pure(false)
          }

        def keepAlive: Resource[F, Boolean] =
          sem.tryPermit

        override def release(lease: Lease): F[Unit] =
          sem.tryPermit.use {
            case false => F.unit // already closed
            case true =>
              F.uncancelable { _ =>
                state.modify { m =>
                  (m - lease, m.get(lease).sequence_)
                }.flatten
              }
          }
      }
    } yield api
  }
}
