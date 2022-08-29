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
// TODO consider further if having signals occur as list elements is better;
// [Type!]! here the index in the list itsef, can be a signal.
// Special cases are Option where size = 1 | 0 and singular where size = 1
// Like batch, have a post-processing
/*
 * final case class SignalResolver[F[_]: MonadCancelThrow, I, K, A, T](
 *  resolver: LeafResolver[F, (I, List[T]), A],
 *  head: I => F[List[T]],
 *  tail: I => F[List[SignalResolver.DataStreamTail[K, T]]]
 * )
 */
final case class SignalResolver[F[_]: MonadCancelThrow, I, K, A, T](
    resolver: LeafResolver[F, (I, T), A],
    head: I => F[T],
    tail: I => F[SignalResolver.DataStreamTail[K, T]]
    // resolver: LeafResolver[F, (I, T2), A],
    // eval: I => F[(List[(K, T, Ref[T])], List[(K, T)] => F[T2])]
    // TODO add filter eval scan implementation
    // evalScanFilter: (Z, (Z, I, T) => F[(Z, Option[T])])
    // post
) extends Resolver[F, I, A] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, K, A, T] =
    SignalResolver(
      resolver.mapK(fk),
      i => fk(head(i)),
      i => fk(tail(i))
      // (i, a) => fk(filterMap(i, a))
    )

  def contramap[C](g: C => I): SignalResolver[F, C, K, A, T] =
    SignalResolver(
      resolver.contramap[(C, T)] { case (c, t) => (g(c), t) },
      i => head(g(i)),
      i => tail(g(i)).map(dst => dst.copy(ref = StreamReference(dst.ref.id)))
      // (i, a) => filterMap(g(i), a)
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
  final case class DataStreamTail[K, T](
      ref: StreamReference[K, T],
      key: K
  )
}
