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
    post: (I, A) => F[B]
) extends Resolver[F, I, B] {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SignalResolver[G, I, A, B] =
    SignalResolver(
      head.mapK(fk),
      i => fk(tail(i)),
      (i, a) => fk(post(i, a))
    )

  def contramap[C](g: C => I): SignalResolver[F, C, A, B] =
    SignalResolver(
      head.contramap(g),
      i => tail(g(i)).map(dst => dst.copy(ref = StreamReference(dst.ref.id))),
      (i, a) => post(g(i), a)
    )

  def flatMapF[C](f: B => F[C])(implicit F: FlatMap[F]): SignalResolver[F, I, A, C] =
    SignalResolver(head, tail, (i, a) => post(i, a).flatMap(f))
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