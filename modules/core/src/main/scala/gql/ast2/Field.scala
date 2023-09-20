package gql.ast2

import cats._
import gql.resolver.Resolver

trait FieldAttribute[+F[_], -A, B]
trait AnonymousFieldAttribute[+F[_], B] extends FieldAttribute[F, Any, B]

case class AbstractField[+F[_], A](
    attributes: List[AnonymousFieldAttribute[F, A]]
) extends AnyField[F, Any, A]

sealed trait AnyField[+F[_], -A, B] {
  def attributes: List[FieldAttribute[F, A, B]]

  // def asAbstract: AbstractField[F, B]
}

abstract class Field[+F[_], -A, B](
    val resolver: Resolver[F, A, B],
    val attributes: List[FieldAttribute[F, A, B]]
) extends AnyField[F, A, B]

case class ContravariantField[+F[_], -A, B](
    override val resolver: Resolver[F, A, B],
    override val attributes: List[AnonymousFieldAttribute[F, B]]
) extends Field[F, A, B](resolver, attributes)
