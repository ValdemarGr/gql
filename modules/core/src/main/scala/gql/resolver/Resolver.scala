package gql.resolver

import cats.effect._
import cats._
import cats.data._

trait Resolver[F[_], -I, A] {
  def mapK[G[_]: Functor](fk: F ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]

  def andThen[O2](next: Resolver[F, A, O2]): Resolver[F, I, O2] =
    CompositionResolver(this.asInstanceOf[Resolver[F, I, Any]], next.asInstanceOf[Resolver[F, Any, O2]])
}

final case class EffectResolver[F[_], I, A](resolve: I => F[Ior[String, A]]) extends Resolver[F, I, A] {
  override def mapK[G[_]: Functor](fk: F ~> G): EffectResolver[G, I, A] =
    EffectResolver(resolve.andThen(fk.apply))

  def contramap[B](g: B => I): EffectResolver[F, B, A] =
    EffectResolver(g andThen resolve)
}
