package gql.interpreter

import org.typelevel.paiges._

// Like show, but for docs.
// The doc algorithm is global, so we need to keep things in "doc" for as long as possible to get the best results.
trait Doced[A] {
  def apply(a: A): Doc
}

object Doced {
  def apply[A](implicit ev: Doced[A]): Doced[A] = ev

  def doc[A](a: A)(implicit ev: Doced[A]): Doc = ev(a)

  def from[A](f: A => Doc): Doced[A] = f(_)
}