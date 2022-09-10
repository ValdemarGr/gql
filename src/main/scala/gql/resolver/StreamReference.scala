package gql.resolver

import cats.data._
import cats._
import cats.effect._
import gql.SchemaState
import cats.implicits._

final case class StreamReference[K, T](id: Int) {
  def apply[F[_]: MonadCancelThrow, I, A](resolver: LeafResolver[F, (I, T), A])(hd: I => EitherT[F, String, T])(
      k: I => EitherT[F, String, K]
  ): SignalResolver[F, I, K, A, T] =
    SignalResolver[F, I, K, A, T](resolver, hd, k.andThen(_.map(SignalResolver.DataStreamTail(this, _))))
}

object StreamReference {
  def apply[F[_], K, T](
      pickSubscription: K => Resource[F, fs2.Stream[F, T]]
  ): State[SchemaState[F], StreamReference[K, T]] =
    State { s =>
      val id = s.nextId
      val entry = pickSubscription.asInstanceOf[Any => Resource[F, fs2.Stream[F, Any]]]
      (s.copy(nextId = id + 1, streams = s.streams + (id -> entry)), StreamReference(id))
    }
}
