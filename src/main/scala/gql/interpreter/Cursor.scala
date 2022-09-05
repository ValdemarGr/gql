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

object LastPath {
  def unapply(cursor: Cursor): Option[GraphArc] =
    cursor.path.initLast
      .flatMap { case (c, l) => Some(l).filter(_ => c.isEmpty) }
}

final case class NodeMeta(
    startPosition: Cursor,
    relativePath: Cursor,
    cursorGroup: BigInt
) {
  lazy val absolutePath = Cursor(startPosition.path ++ relativePath.path)

  def index(i: Int): NodeMeta = NodeMeta(startPosition, relativePath.index(i), cursorGroup)
  def ided(id: Int): NodeMeta = NodeMeta(startPosition, relativePath.ided(id), cursorGroup)
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

final case class EvalNode(meta: NodeMeta, value: Any) {
  def setValue(value: Any): EvalNode = copy(value = value)

  def modify(f: NodeMeta => NodeMeta): EvalNode = copy(meta = f(meta))

  def succeed(value: Any, f: NodeMeta => NodeMeta): EvalNode =
    EvalNode(f(meta), value)

  def succeed(value: Any): EvalNode = succeed(value, identity)

  def fail(error: String, f: NodeMeta => NodeMeta): EvalNode =
    EvalNode(f(meta), error)

  def fail(error: String): EvalNode = fail(error, identity)
}

object EvalNode {
  def startAt[A](value: A, cursorGroup: BigInt, startPosition: Cursor) =
    EvalNode(NodeMeta.startAt(startPosition, cursorGroup), value)

  def empty[A](value: A, cursorGroup: BigInt) = startAt(value, cursorGroup, Cursor.empty)
}

final case class EvalFailure(meta: Chain[NodeMeta], error: Option[String], internal: String, throwable: Option[Throwable])
