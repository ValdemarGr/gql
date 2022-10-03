package gql.resolver

import cats._
import cats.effect._

final case class CompositionResolver[F[_], I, O](
    left: Resolver[F, I, Any],
    right: Resolver[F, Any, O]
) extends Resolver[F, I, O] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Resolver[G, I, O] =
    CompositionResolver(left.mapK(fk), right.mapK(fk))

  override def contramap[B](g: B => I): Resolver[F, B, O] =
    CompositionResolver(left.contramap(g), right)
}
