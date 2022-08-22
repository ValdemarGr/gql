package gql.interpreter

import cats.data._

sealed trait GraphArc
final case class Ided(id: Int) extends GraphArc
final case class Index(index: Int) extends GraphArc

final case class Cursor(path: Chain[GraphArc]) {
  def add(next: GraphArc): Cursor = Cursor(path :+ next)
  def index(idx: Int) = add(Index(idx))
  def ided(id: Int) = add(Ided(id))

  def headOption = path.headOption

  def head = headOption.get

  def tail = Cursor(Chain.fromOption(path.uncons).flatMap { case (_, tl) => tl })
}

object Cursor {
  def empty = Cursor(Chain.empty)
}

final case class NodePosition(
    startPosition: Cursor,
    relativePath: Cursor
) {
  lazy val absolutePath = Cursor(startPosition.path ++ relativePath.path)

  def index(i: Int): NodePosition = NodePosition(startPosition, relativePath.index(i))
  def ided(id: Int): NodePosition = NodePosition(startPosition, relativePath.ided(id))
}

object NodePosition {
  def startAt(cursor: Cursor): NodePosition = NodePosition(cursor, Cursor.empty)

  def empty = startAt(Cursor.empty)
}

final case class NodeValue(
    position: NodePosition,
    value: Any
) {
  def index(xs: List[Any]): List[NodeValue] =
    xs.zipWithIndex.map { case (x, i) => NodeValue(position.index(i), x) }
  def ided(id: Int, value: Any): NodeValue =
    NodeValue(position.ided(id), value)

  def setValue(value: Any): NodeValue = copy(value = value)
}

object NodeValue {
  def empty[A](value: A) = NodeValue(NodePosition(Cursor(Chain.empty), Cursor(Chain.empty)), value)
}
