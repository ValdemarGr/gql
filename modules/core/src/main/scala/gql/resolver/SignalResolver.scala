package gql.resolver

import cats.effect._
import cats.implicits._
import cats._
import cats.data._

/*
 * I input type
 * K the key that defines what the stream regards
 * A the output type
 * T the intermediate type that the stream emits and is transformed to A
 */
final case class SignalResolver[F[_], I, R, A](
    resolver: LeafResolver[F, (I, R), A],
    head: I => IorT[F, String, R],
    ref: StreamRef[F, I, R]
) extends Resolver[F, I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, R, A] =
    SignalResolver(
      resolver.mapK(fk),
      i => head(i).mapK(fk),
      ref.mapK(fk)
    )

  def contramap[C](g: C => I): SignalResolver[F, C, R, A] =
    SignalResolver(
      resolver.contramap[(C, R)] { case (c, t) => (g(c), t) },
      i => head(g(i)),
      ref.contramap[C](g)
    )
}

object SignalResolver {
  def apply2[F[_]: MonadCancelThrow, I, R, A](sr: StreamRef[F, I, R])(hd: I => IorT[F, String, R])(
      resolver: LeafResolver[F, (I, R), A]
  ): SignalResolver[F, I, R, A] =
    SignalResolver(resolver, hd, sr)
}
