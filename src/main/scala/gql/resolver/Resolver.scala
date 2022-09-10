package gql.resolver

import cats.effect._
import cats._
import cats.data._

trait Resolver[F[_], I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]
}

trait LeafResolver[F[_], I, A] extends Resolver[F, I, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): LeafResolver[G, I, A]

  override def contramap[B](g: B => I): LeafResolver[F, B, A]
}

final case class EffectResolver[F[_], I, A](resolve: I => IorT[F, String, A]) extends LeafResolver[F, I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): EffectResolver[G, I, A] =
    EffectResolver(resolve.andThen(_.mapK(fk)))

  def contramap[B](g: B => I): EffectResolver[F, B, A] =
    EffectResolver(g andThen resolve)
}
