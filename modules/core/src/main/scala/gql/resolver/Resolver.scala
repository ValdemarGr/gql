package gql.resolver

import cats.effect._
import cats._
import cats.data._

trait Resolver[+F[_], -I, A] {
  def mapK[F2[x] >: F[x], G[_]: Functor](fk: F2 ~> G): Resolver[G, I, A]

  def contramap[B](g: B => I): Resolver[F, B, A]

  def andThen[F2[x] >: F[x], O2](next: Resolver[F2, A, O2]): Resolver[F2, I, O2] =
    CompositionResolver(this.asInstanceOf[Resolver[F2, I, Any]], next.asInstanceOf[Resolver[F2, Any, O2]])
}

final case class EffectResolver[F[_], I, A](resolve: I => F[Ior[String, A]]) extends Resolver[F, I, A] {
  def mapK[F2[x] >: F[x], G[_]: Functor](fk: F2 ~> G): EffectResolver[G, I, A] =
    EffectResolver(resolve.andThen(fk.apply))

  def contramap[B](g: B => I): EffectResolver[F, B, A] =
    EffectResolver(g andThen resolve)
}

final case class PureResolver[I, A](resolve: I => A) extends Resolver[Nothing, I, A] {
  override def mapK[F2[x] >: Nothing, G[_]: Functor](fk: F2 ~> G): PureResolver[I, A] =
    this

  def contramap[B](g: B => I): PureResolver[B, A] =
    PureResolver(g andThen resolve)
}
