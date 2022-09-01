package gql.out

import scala.reflect.ClassTag

trait SimplePrism[A, B] extends (A => Option[B]) {
  def contramap[C](f: C => A): SimplePrism[C, B] = c => apply(f(c))

  def map[C](f: B => C): SimplePrism[A, C] = a => apply(a).map(f)
}

object SimplePrism {
  implicit def fromSubtypeRelationship[A, B <: A: ClassTag]: SimplePrism[A, B] =
    Some(_).collect { case b: B => b }
}
