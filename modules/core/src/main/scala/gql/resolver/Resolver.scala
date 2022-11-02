package gql.resolver

import cats._
import cats.data._
import fs2.Stream

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

final case class StreamResolver[F[_], I, A](
    stream: I => Stream[F, IorNec[String, A]]
) extends Resolver[F, I, A] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A] =
    StreamResolver(stream.andThen(_.translate(fk)))

  override def contramap[B](g: B => I): Resolver[F, B, A] =
    StreamResolver[F, B, A](i => stream(g(i)))
}

final case class CompositionResolver[F[_], I, A, O](
    left: Resolver[F, I, A],
    right: Resolver[F, A, O]
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, O] =
    CompositionResolver(left.mapK(fk), right.mapK(fk))

  override def contramap[B](g: B => I): Resolver[F, B, O] =
    CompositionResolver(left.contramap(g), right)
}

final case class CacheResolver[F[_], I, I2, O](
    first: I => F[Either[I2, O]],
    fallback: Resolver[F, I2, O]
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, O] =
    CacheResolver(first.andThen(fk.apply), fallback.mapK(fk))

  override def contramap[B](g: B => I): Resolver[F, B, O] =
    CacheResolver(i => first(g(i)), fallback)
}
