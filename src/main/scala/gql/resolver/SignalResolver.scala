package gql.resolver

import cats.effect._
import cats.implicits._
import cats._
import cats.arrow.FunctionK

/*
 * I input type
 * K the key that defines what the stream regards
 * A the output type
 * T the intermediate type that the stream emits and is transformed to A
 */
final case class SignalResolver[F[_]: MonadCancelThrow, I, K, A, T](
    resolver: LeafResolver[F, (I, T), A],
    head: I => F[T],
    tail: I => F[SignalResolver.DataStreamTail[K, T]]
) extends Resolver[F, I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, K, A, T] =
    SignalResolver(
      resolver.mapK(fk),
      i => fk(head(i)),
      i => fk(tail(i))
    )

  def contramap[C](g: C => I): SignalResolver[F, C, K, A, T] =
    SignalResolver(
      resolver.contramap[(C, T)] { case (c, t) => (g(c), t) },
      i => head(g(i)),
      i => tail(g(i)).map(dst => dst.copy(ref = StreamReference(dst.ref.id)))
    )
}

object SignalResolver {
  final case class DataStreamTail[K, T](
      ref: StreamReference[K, T],
      key: K
  )
}
