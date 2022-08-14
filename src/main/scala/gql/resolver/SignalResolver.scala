package gql.resolver

import cats.effect._
import cats.implicits._
import cats._

sealed trait ConstrainedPipe[A, B] {
  def pipe[F[_]: Async]: fs2.Pipe[F, A, B]
}

final case class SignalResolver[F[_]: MonadCancelThrow, I, A, B](
    // No elem
    head: LeafResolver[F, I, A],
    // The first element was gotten, the infinite tail is defined as such
    // I = The first initial input, A = The first output from head
    tail: I => F[SignalResolver.DataStreamTail[I, A]],
    // Post-processing of both head and tail, allows a map function on the structure
    postHead: (I, A) => F[B],
    // This cannot be a pipe, since we, the definer of this resolver
    // don't control F, the toplevel interpreter of the resolver has some F bound
    // To continue allowing complex control of the stream we can instead summon the typeclass which allows
    // any combinator in fs2; Async. Attempting to construct mapK and contraMap with fs2.Pipe is impossible
    // and one must instead constrain the implementation to a bidirectional mapping F ~> G and G ~> F for mapK
    // and C => I and I => C for contraMap.
    postTail: (I, A) => ConstrainedPipe[A, B]
) extends Resolver[F, I, B] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, A, B] =
    SignalResolver(
      head.mapK(fk),
      i => fk(tail(i)),
      (i, a) => fk(postHead(i, a)),
      postTail
    )

  def contramap[C](g: C => I): SignalResolver[F, C, A, B] =
    SignalResolver(
      head.contramap(g),
      i => tail(g(i)).map(dst => dst.copy(ref = StreamReference(dst.ref.id))),
      (i, a) => postHead(g(i), a),
      (i, a) =>
        new ConstrainedPipe[A, B] {
          def pipe[F[_]: Async]: fs2.Pipe[F, A, B] = postTail(g(i), a).pipe[F]
        }
    )

  def through[C](headOp: B => F[C], tailOp: ConstrainedPipe[(I, A, B), C])(implicit F: FlatMap[F]): SignalResolver[F, I, A, C] =
    SignalResolver(
      head,
      tail,
      postHead = (i, a) => postHead(i, a).flatMap(headOp),
      postTail = (i, a) =>
        new ConstrainedPipe[A, C] {
          def pipe[F[_]: Async]: fs2.Pipe[F, A, C] =
            _.through(postTail(i, a).pipe[F]).map(b => (i, a, b)).through(tailOp.pipe[F])
        }
    )
}

object SignalResolver {
  final case class InputValues(
      input: Any,
      head: Any
  )

  final case class DataStreamTail[I, A](
      ref: StreamReference[I, A],
      inputValues: InputValues
  )
}
