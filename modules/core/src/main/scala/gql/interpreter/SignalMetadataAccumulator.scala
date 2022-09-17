package gql.interpreter

import cats.effect._
import gql.resolver._
import gql.PreparedQuery._
import cats.implicits._

trait SignalMetadataAccumulator[F[_], A] {
  def add(context: A, ref: Int, key: Any): F[BigInt]

  def remove(id: BigInt): F[Unit]

  def getState: F[Map[BigInt, A]]
}

object SignalMetadataAccumulator {
  def apply[F[_], A](implicit sigAlg: SubscriptionSupervisor[F], F: Concurrent[F]) =
    F.ref(Map.empty[BigInt, A]).map { state =>
      new SignalMetadataAccumulator[F, A] {
        def add(context: A, ref: Int, key: Any): F[BigInt] =
          sigAlg.subscribe(ref, key).flatMap(id => state.update(_ + (id -> context)).as(id))

        def remove(id: BigInt): F[Unit] = sigAlg.remove(id) >> state.update(_ - id)

        def getState: F[Map[BigInt, A]] =
          state.get
      }
    }
}
