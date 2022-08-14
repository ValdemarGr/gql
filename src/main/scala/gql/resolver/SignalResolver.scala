package gql.resolver

import cats.effect._
import cats.implicits._
import cats._

final case class SignalResolver[F[_]: MonadCancelThrow, I, A, B](
    // No elem
    head: LeafResolver[F, I, A],
    // The first element was gotten, the infinite tail is defined as such
    // I = The first initial input, A = The first output from head
    tail: I => F[SignalResolver.DataStreamTail[I, A]],
    // Post-processing of both head and tail, allows a map function on the structure
    postHead: (I, A) => F[B],
    postTail: (I, A) => fs2.Stream[F, B]
) extends Resolver[F, I, B] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, A, B] =
    SignalResolver(
      head.mapK(fk),
      i => fk(tail(i)),
      (i, a) => fk(postHead(i, a)),
      (i, a) => postTail(i, a).translate(fk)
    )

  def contramap[C](g: C => I): SignalResolver[F, C, A, B] =
    SignalResolver(
      head.contramap(g),
      i => tail(g(i)).map(dst => dst.copy(ref = StreamReference(dst.ref.id))),
      (i, a) => postHead(g(i), a),
      (i, a) => postTail(g(i), a)
    )

  def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): SignalResolver[F, I, A, C] =
    SignalResolver(head, tail, (i, a) => postHead(i, a).flatMap(f), (i, a) => postTail(i, a).evalMap(f))

  def through(pipe: fs2.Pipe[F, (I, A, B), B]): SignalResolver[F, I, A, B] =
    copy(postTail = (i, a) => postTail(i, a).map(b => (i, a, b)).through(pipe))
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
