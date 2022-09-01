package gql.out

import cats.effect._
import cats._

trait Output[F[_], +A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, A]
}
