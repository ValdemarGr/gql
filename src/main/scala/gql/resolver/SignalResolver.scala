package gql.resolver

import cats.effect._
import cats.implicits._
import cats._
import cats.arrow.FunctionK

// sealed trait ConstrainedPipe[A, B] {
//   def pipe[F[_]](implicit F: Async[F]): fs2.Pipe[F, A, B]
// }

/*
 * I input type
 * K the key that defines what the stream regards
 * A the output type
 * T the intermediate type that the stream emits and is transformed to A
 */
final case class SignalResolver[F[_]: MonadCancelThrow, I, K, A, T](
    head: LeafResolver[F, I, A],
    tail: I => F[SignalResolver.DataStreamTail[K, T]],
    filterMap: (I, T) => F[Option[A]]
) extends Resolver[F, I, T] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, K, A, T] =
    SignalResolver(
      head.mapK(fk),
      i => fk(tail(i)),
      (i, a) => fk(filterMap(i, a))
    )

  def contramap[C](g: C => I): SignalResolver[F, C, K, A, T] =
    SignalResolver(
      head.contramap(g),
      i => tail(g(i)).map(dst => dst.copy(ref = StreamReference(dst.ref.id))),
      (i, a) => filterMap(g(i), a)
      // new ConstrainedPipe[A, B] {
      //   def pipe[F[_]](implicit F: Async[F]): fs2.Pipe[F, A, B] = postTail(g(i), a).pipe[F]
      // }
    )

  // def through[C](f: B => F[C] /*, tailOp: ConstrainedPipe[(I, A, B), C]*/ )(implicit F: FlatMap[F]): SignalResolver[F, I, A, C] =
  //   SignalResolver(
  //     head,
  //     tail,
  //     postTail = (i, a) => postTail(i, a).flatMap(_.traverse(f))
  //     // new ConstrainedPipe[A, C] {
  //     //   def pipe[F[_]](implicit F: Async[F]): fs2.Pipe[F, A, C] =
  //     //     _.through(postTail(i, a).pipe[F]).map(b => (i, a, b)).through(tailOp.pipe[F])
  //     // }
  //   )
}

object SignalResolver {
  final case class InputValues(
      input: Any,
      head: Any
  )

  final case class DataStreamTail[K, T](
      ref: StreamReference[K, T],
      key: K
  )
}
