package gql.relational

import cats._
import cats.implicits._
import cats.effect.std.Hotswap
import cats.effect.std.Mutex
import cats.effect._

trait LazyResource[F[_], A] { self =>
  def get: Resource[F, A]

  def forceClose: F[Unit]

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G)(implicit F: MonadCancelThrow[F]) = new LazyResource[G, A] {
    def get = self.get.mapK(fk)

    def forceClose = fk(self.forceClose)
  }
}

object LazyResource {
  def fromResource[F[_], A](res: Resource[F, A])(implicit F: Concurrent[F]): Resource[F,LazyResource[F,A]] =
    Hotswap.create[F, A].evalMap { hs =>
      Mutex[F].map { mtx =>
        new LazyResource[F, A] {
          override def forceClose: F[Unit] = hs.clear

          override def get: Resource[F, A] =
            mtx.lock >>
              hs.get.evalMap {
                case None      => hs.swap(res)
                case Some(ses) => F.pure(ses)
              }
        }
      }
    }

  implicit def functorForLazyResource[F[_]]: Functor[LazyResource[F, *]] = new Functor[LazyResource[F, *]] {
    override def map[A, B](fa: LazyResource[F, A])(f: A => B): LazyResource[F, B] = new LazyResource[F, B] {
      override def get: Resource[F, B] = fa.get.map(f)
      override def forceClose: F[Unit] = fa.forceClose
    }
  }
}
