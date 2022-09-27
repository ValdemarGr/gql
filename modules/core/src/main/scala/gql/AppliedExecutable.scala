package gql

import gql.PreparedQuery

sealed trait AppliedExecutable[F[_]]

object AppliedExecutable {
  final case class ValidationError[F[_]](msg: PreparedQuery.PositionalError) extends AppliedExecutable[F]
  final case class Query[F[_]](fa: F[QueryResult]) extends AppliedExecutable[F]
  final case class Mutation[F[_]](fa: F[QueryResult]) extends AppliedExecutable[F]
  final case class Subscription[F[_]](fa: fs2.Stream[F, QueryResult]) extends AppliedExecutable[F]
}
