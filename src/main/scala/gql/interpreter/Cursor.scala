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

  def uncons = path.uncons.map{ case (p, tl) => (p, Cursor(tl)) }
}

object Cursor {
  def empty = Cursor(Chain.empty)
}

final case class NodeMeta(
    relativePath: Cursor,
    stableId: BigInt
) {
  def index(i: Int): NodeMeta = NodeMeta(relativePath.index(i), stableId)
  def ided(id: Int): NodeMeta = NodeMeta(relativePath.ided(id), stableId)
}

object NodeMeta {
  def empty(stableId: BigInt) = NodeMeta(Cursor.empty, stableId)
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
  def empty[A](value: A, stableId: BigInt) = NodeValue(NodeMeta.empty(stableId), value)
}
