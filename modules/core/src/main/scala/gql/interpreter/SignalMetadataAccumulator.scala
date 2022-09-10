package gql.interpreter

import cats.effect._
import gql.resolver._
import gql.PreparedQuery._
import cats.implicits._

trait SignalMetadataAccumulator[F[_]] {
  def add(
      cursor: Cursor,
      initialValue: Any,
      field: PreparedDataField[F, Any, Any],
      ref: StreamReference[Any, Any],
      key: Any
  ): F[BigInt]

  def remove(id: BigInt): F[Unit]

  def getState: F[Map[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])]]
}

object SignalMetadataAccumulator {
  def apply[F[_]](implicit sigAlg: SubscriptionSupervisor[F], F: Concurrent[F]) =
    F.ref(Map.empty[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])]).map { state =>
      new SignalMetadataAccumulator[F] {
        def add(
            cursor: Cursor,
            initialValue: Any,
            field: PreparedDataField[F, Any, Any],
            ref: StreamReference[Any, Any],
            key: Any
        ): F[BigInt] =
          sigAlg.subscribe(ref, key).flatMap(id => state.update(_ + (id -> (cursor, initialValue, field))).as(id))

        def remove(id: BigInt): F[Unit] = sigAlg.remove(id) >> state.update(_ - id)

        def getState: F[Map[BigInt, (Cursor, Any, PreparedDataField[F, Any, Any])]] =
          state.get
      }
    }
}
