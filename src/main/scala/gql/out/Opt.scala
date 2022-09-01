package gql.out

import cats._
import cats.effect._

final case class Opt[F[_], A](of: Output[F, A]) extends Output[F, Option[A]] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, Option[A]] = Opt(of.mapK(fk))
}
