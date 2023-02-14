package gql.interpreter

import cats.effect._
import cats.implicits._

trait Lease[F[_]] {
  // Opens a child lease on this lease
  def child[A](r: Resource[F, A]): F[(Lease[F], A)]

  // Release all children and self
  def release: F[Unit]
}

object Lease {
    /*def apply[F[_]]() = 
        Resource.eval()
        new Lease[F] {
            def child[A](r: Resource[F, A]): F[(Lease[F], A)] = ???

            def release: F[Unit] = ???
        }*/
}
