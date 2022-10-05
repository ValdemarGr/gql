package gql.interpreter

import cats.data._

sealed trait GraphArc
object GraphArc {
  final case class Field(id: Int, name: String) extends GraphArc
  final case class Index(index: Int) extends GraphArc
  final case class Fragment(id: Int, name: String) extends GraphArc
}

final case class Cursor(path: Chain[GraphArc]) {
  def add(next: GraphArc): Cursor = Cursor(path :+ next)

  def index(idx: Int) = add(GraphArc.Index(idx))
  def field(id: Int, name: String) = add(GraphArc.Field(id, name))
  def fragment(id: Int, name: String) = add(GraphArc.Fragment(id, name))

  def headOption = path.headOption

  def head = headOption.get

  def lastOption = path.lastOption

  def last = lastOption.get

  def tail = Cursor(Chain.fromOption(path.uncons).flatMap { case (_, tl) => tl })

  def uncons = path.uncons.map { case (p, tl) => (p, Cursor(tl)) }

  def dropInit = Cursor(Chain.fromOption(path.initLast).flatMap { case (c, _) => c })
}

object Cursor {
  def empty = Cursor(Chain.empty)
}

final case class EvalNode[A](cursor: Cursor, value: A) {
  def setValue[B](value: B): EvalNode[B] = copy(value = value)

  def modify(f: Cursor => Cursor): EvalNode[A] = copy(cursor = f(cursor))

  def succeed[B](value: B, f: Cursor => Cursor): EvalNode[B] =
    EvalNode(f(cursor), value)

  def succeed[B](value: B): EvalNode[B] = succeed(value, identity)
}

object EvalNode {
  def empty[A](value: A) = EvalNode[A](Cursor.empty, value)
}
