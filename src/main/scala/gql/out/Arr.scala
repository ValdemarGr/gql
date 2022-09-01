package gql.out

import cats._
import cats.effect._

final case class Arr[F[_], A](of: Output[F, A]) extends Output[F, Vector[A]] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, Vector[A]] = Arr(of.mapK(fk))
}
