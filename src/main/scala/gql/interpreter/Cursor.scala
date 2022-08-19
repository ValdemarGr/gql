package gql.interpreter

sealed trait GraphPath
final case class Ided(id: Int) extends GraphPath
final case class Index(index: Int) extends GraphPath

final case class Cursor(path: Vector[GraphPath]) {
  def add(next: GraphPath): Cursor = Cursor(path :+ next)
  def index(idx: Int) = add(Index(idx))
  def ided(id: Int) = add(Ided(id))
}

final case class NodeValue(cursor: Cursor, value: Any) {
  def index(xs: List[Any]): List[NodeValue] =
    xs.zipWithIndex.map { case (x, i) => NodeValue(cursor.index(i), x) }
  def ided(id: Int, value: Any): NodeValue =
    NodeValue(cursor.ided(id), value)
}

object NodeValue {
  def empty[A](value: A) = NodeValue(Cursor(Vector.empty), value)
}
