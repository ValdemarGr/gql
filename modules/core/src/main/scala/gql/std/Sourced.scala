package gql.std

import org.tpolecat.sourcepos.SourcePos
import cats._

final case class Sourced[A](value: A, position: SourcePos)

object Sourced extends LowPrioritySourcedImplicits {
  implicit val functorForSourced: Functor[Sourced] = new Functor[Sourced] {
    override def map[A, B](fa: Sourced[A])(f: A => B): Sourced[B] = Sourced(f(fa.value), fa.position)
  }

  implicit def sourcedForAnyTypeclassLike[A](implicit a: A, sp: SourcePos): Sourced[A] = Sourced(a, sp)
}

trait LowPrioritySourcedImplicits {
  implicit def sourcedConversionForAnyValue[A](value: A)(implicit sp: SourcePos): Sourced[A] = Sourced(value, sp)
}
