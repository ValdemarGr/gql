package gql.resolver

import cats._
import cats.data._
import cats.effect._
import fs2.Stream

final case class StreamResolver[F[_], I, R, A](
    resolver: Resolver[F, (I, R), A],
    stream: I => Stream[F, IorNec[String, R]]
) extends LeafResolver[F, I, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): LeafResolver[G, I, A] = ???

  override def contramap[B](g: B => I): LeafResolver[F, B, A] = ???
}
