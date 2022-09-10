package gql.out

import cats.data._
import cats._
import cats.effect._

final case class Enum[F[_], A](name: String, encoder: NonEmptyMap[A, String]) extends Toplevel[F, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Output[G, A] =
    Enum(name, encoder)
}
