package gql.out

import io.circe.Encoder
import cats._
import cats.effect._

final case class Scalar[F[_], A](name: String, encoder: Encoder[A]) extends Toplevel[F, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Scalar[G, A] =
    Scalar(name, encoder)
}
