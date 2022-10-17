package gql.resolver

import cats._
import cats.data._
import cats.effect._
import fs2.Stream

final case class StreamResolver[+F[_], I, R, A](
    stream: I => Stream[F, IorNec[String, R]]
) extends Resolver[F, I, A] {
  override def mapK[F2[x] >: F[x], G[_]: Functor](fk: F2 ~> G): Resolver[G, I, A] =
    StreamResolver(stream.andThen(_.translate(fk)))

  override def contramap[B](g: B => I): Resolver[F, B, A] =
    StreamResolver[F, B, R, A](i => stream(g(i)))
}
