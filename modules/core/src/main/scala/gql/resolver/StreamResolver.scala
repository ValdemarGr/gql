package gql.resolver

import cats._
import cats.data._
import cats.effect._
import fs2.Stream

final case class StreamResolver[F[_], I, R, A](
    // The resolver must not be another stream
    // It is possible if nested streams had a distinguished path (CursorGroup), but they don't
    resolver: LeafResolver[F, (I, R), A],
    stream: I => Stream[F, IorNec[String, R]]
) extends Resolver[F, I, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Resolver[G, I, A] =
    StreamResolver(resolver.mapK(fk), stream.andThen(_.translate(fk)))

  override def contramap[B](g: B => I): Resolver[F, B, A] =
    StreamResolver[F, B, R, A](resolver.contramap[(B, R)] { case (b, r) => (g(b), r) }, i => stream(g(i)))
}
