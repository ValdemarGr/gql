package gql.out

import cats.effect._
import cats._
import gql.Arg
import gql.resolver.Resolver

final case class Field[F[_], I, T, A](
    args: Arg[A],
    resolve: Resolver[F, (I, A), T],
    output: Eval[Output[F, T]]
) {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Field[G, I, T, A] =
    Field[G, I, T, A](
      args,
      resolve.mapK(fk),
      output.map(_.mapK(fk))
    )

  def contramap[B](g: B => I): Field[F, B, T, A] =
    Field(
      args,
      resolve.contramap[(B, A)] { case (b, a) => (g(b), a) },
      output
    )
}
