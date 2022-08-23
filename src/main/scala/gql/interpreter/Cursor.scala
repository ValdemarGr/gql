package gql.interpreter

import cats.data._
import cats.instances.stream

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

  def uncons = path.uncons.map { case (p, tl) => (p, Cursor(tl)) }

  def dropInit = Cursor(Chain.fromOption(path.initLast).flatMap { case (c, _) => c })
}

object Cursor {
  def empty = Cursor(Chain.empty)
}

final case class NodeMeta(
    startPosition: Cursor,
    relativePath: Cursor,
    stableId: BigInt
) {
  lazy val absolutePath = Cursor(startPosition.path ++ relativePath.path)

  def index(i: Int): NodeMeta = NodeMeta(startPosition, relativePath.index(i), stableId)
  def ided(id: Int): NodeMeta = NodeMeta(startPosition, relativePath.ided(id), stableId)
}

object NodeMeta {
  def startAt(startPosition: Cursor, stableId: BigInt) = NodeMeta(startPosition, Cursor.empty, stableId)

  def empty(stableId: BigInt) = startAt(Cursor.empty, stableId)
}

final case class NodeValue(
    meta: NodeMeta,
    value: Any
) {
  def index(xs: List[Any]): List[NodeValue] =
    xs.zipWithIndex.map { case (x, i) => NodeValue(meta.index(i), x) }
  def ided(id: Int, value: Any): NodeValue =
    NodeValue(meta.ided(id), value)

  def setValue(value: Any): NodeValue = copy(value = value)
}

object NodeValue {
  def startAt[A](value: A, stableId: BigInt, startPosition: Cursor) =
    NodeValue(NodeMeta.startAt(startPosition, stableId), value)

  def empty[A](value: A, stableId: BigInt) = startAt(value, stableId, Cursor.empty)
}
