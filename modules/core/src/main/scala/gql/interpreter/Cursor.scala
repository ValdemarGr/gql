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

final case class CursorGroup(
    startPosition: Cursor,
    relativePath: Cursor,
    id: BigInt
) {
  lazy val absolutePath = Cursor(startPosition.path ++ relativePath.path)

  def index(i: Int): CursorGroup = CursorGroup(startPosition, relativePath.index(i), id)
  def field(id: Int, name: String): CursorGroup = CursorGroup(startPosition, relativePath.field(id, name), id)
  def fragment(id: Int, name: String): CursorGroup = CursorGroup(startPosition, relativePath.fragment(id, name), id)
}

object CursorGroup {
  def startAt(id: BigInt, start: Cursor) = CursorGroup(start, Cursor.empty, id)

  def empty(id: BigInt) = startAt(id, Cursor.empty)
}

final case class EvalNode[A](cursorGroup: CursorGroup, value: A) {
  def setValue[B](value: B): EvalNode[B] = copy(value = value)

  def modify(f: CursorGroup => CursorGroup): EvalNode[A] = copy(cursorGroup = f(cursorGroup))

  def succeed[B](value: B, f: CursorGroup => CursorGroup): EvalNode[B] =
    EvalNode(f(cursorGroup), value)

  def succeed[B](value: B): EvalNode[B] = succeed(value, identity)
}

object EvalNode {
  def startAt[A](value: A, cursorGroup: BigInt, startPosition: Cursor) =
    EvalNode(CursorGroup.startAt(cursorGroup, startPosition), value)

  def empty[A](value: A, cursorGroup: BigInt) = startAt(value, cursorGroup, Cursor.empty)
}
