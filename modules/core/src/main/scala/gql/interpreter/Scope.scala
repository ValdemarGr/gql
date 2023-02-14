package gql.interpreter

import cats.implicits._
import cats.data._
import cats.effect._

/*
 * A scope is a structured representation of a tree of resources
 *
 * The invariants and semantics for Scope are:
 * - A scope's parent is immutable
 * - A scope's children are mutable
 * - A scope can be opened and closed, when closed any children attempting to open a resource will fail
 * - A child creates a lease on its parent before opening it's own resource
 */
trait Scope[F[_]] {
  def id: Unique.Token

  def parent: Option[Scope[F]]

  def children: F[Chain[Scope[F]]]

  def openChild: Resource[F, Option[Scope[F]]]

  def openChild2: F[Scope[F]]

  // False if the scope is closed
  def lease: Resource[F, Boolean]

  def leaseHere[A](r: Resource[F, A]): Resource[F, Option[A]] =
    lease.flatMap {
      case false => Resource.pure[F, Option[A]](None)
      case true  => r.map(_.some)
    }

  def leaseHere2[A](r: Resource[F, A]): F[A]
}

object Scope {
}
