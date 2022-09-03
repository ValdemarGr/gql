package gql.parser

import cats.parse.{Parser => P, Caret, Parser0 => P0}

final case class Pos[+A](caret: Caret, value: A)

object Pos {
  def pos[A](p: P[A]): P[Pos[A]] =
    (p ~ P.caret).map { case (a, c) => Pos(c, a) }

  def pos0[A](p: P0[A]): P0[Pos[A]] =
    (p ~ P.caret).map { case (a, c) => Pos(c, a) }
}
