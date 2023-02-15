package gql.interpreter

import cats.effect._

trait Lease[F[_]] {
  def scope: Scope[F]

  def id: Unique.Token

  def release: F[Unit]
}
