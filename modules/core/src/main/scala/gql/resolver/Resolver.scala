package gql.resolver

import cats.effect._
import cats._
import cats.data._

trait Resolver[F[_], -I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]
}

final case class FallibleResolver[F[_], I, A](resolve: I => F[Ior[String, A]]) extends Resolver[F, I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): FallibleResolver[G, I, A] =
    FallibleResolver(resolve.andThen(fk.apply))

  def contramap[B](g: B => I): FallibleResolver[F, B, A] =
    FallibleResolver(g andThen resolve)
}

final case class EffectResolver[F[_], I, A](resolve: I => F[A]) extends Resolver[F, I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): EffectResolver[G, I, A] =
    EffectResolver(resolve.andThen(fk.apply))

  def contramap[B](g: B => I): EffectResolver[F, B, A] =
    EffectResolver(g andThen resolve)
}

final case class PureResolver[F[_], I, A](resolve: I => A) extends Resolver[F, I, A] {
  override def mapK[G[_]: Functor](fk: F ~> G): PureResolver[G, I, A] =
    PureResolver(resolve)

  def contramap[B](g: B => I): PureResolver[F, B, A] =
    PureResolver(g andThen resolve)
}
