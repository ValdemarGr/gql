package gql.resolver

import cats.effect._
import cats._

sealed trait Resolver[F[_], I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Resolver[G, I, A]

  def contraMap[B](g: B => I): Resolver[F, B, A]
}

sealed trait LeafResolver[F[_], I, A] extends Resolver[F, I, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): LeafResolver[G, I, A]

  override def contraMap[B](g: B => I): LeafResolver[F, B, A]
}

object LeafResolver {
  final case class Pure[F[_], I, A](resolve: I => A) extends LeafResolver[F, I, A] {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Pure[G, I, A] =
      Pure(resolve)

    def contraMap[B](g: B => I): Pure[F, B, A] =
      Pure(g andThen resolve)
  }

  final case class Effect[F[_], I, A](resolve: I => F[A]) extends LeafResolver[F, I, A] {
    def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Effect[G, I, A] =
      Effect(resolve.andThen(fk.apply))

    def contraMap[B](g: B => I): Effect[F, B, A] =
      Effect(g andThen resolve)
  }
}
