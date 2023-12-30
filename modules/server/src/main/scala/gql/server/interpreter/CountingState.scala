package gql.server.interpreter

import cats.effect._
import cats._
import cats.implicits._
import gql.resolver._
import gql.preparation._
import gql.server.planner.NodeTree

trait CountingState[F[_]] {
  def visitMultiplicity(n: Int): F[CountingState[F]]

  def submit[K, V](ubi: UniqueBatchInstance[K, V], k: K): F[Option[Map[K, V]]]
}

object CountingState {
  def init[F[_]](
    dag: NodeTree
  )(implicit F: Async[F]) = {
    F
  }
}
