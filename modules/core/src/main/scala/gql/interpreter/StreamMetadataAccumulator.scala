package gql.interpreter

import cats.effect._
import gql.resolver._
import gql.PreparedQuery._
import cats.implicits._
import fs2.Stream

trait StreamMetadataAccumulator[F[_], A, B] {
  def add(context: A, stream: Stream[F, B]): F[(Unique.Token, Either[Throwable, B])]

  def getState: F[Map[Unique.Token, A]]
}

object StreamMetadataAccumulator {
  def apply[F[_], A, B](implicit streamSup: StreamSupervisor[F, B], F: Concurrent[F]) =
    F.ref(Map.empty[Unique.Token, A]).map { state =>
      new StreamMetadataAccumulator[F, A, B] {
        override def add(context: A, stream: Stream[F, B]): F[(Unique.Token, Either[Throwable, B])] =
          streamSup
            .acquireAwait(stream)
            .flatTap { case (token, _) => state.update(_ + (token -> context)) }

        override def getState: F[Map[Unique.Token, A]] = state.get
      }
    }
}
