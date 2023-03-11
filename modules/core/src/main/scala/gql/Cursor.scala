package gql

import cats.data._
import cats.implicits._
import cats._

sealed trait GraphArc {
    def asString: String
}
object GraphArc {
  final case class Field(name: String) extends GraphArc {
    def asString = name
  }
  final case class Index(index: Int) extends GraphArc {
    def asString = index.toString
  }
}

final case class Cursor(path: Chain[GraphArc]) {
  def add(next: GraphArc): Cursor = Cursor(path :+ next)

  def index(idx: Int) = add(GraphArc.Index(idx))
  def field(name: String) = add(GraphArc.Field(name))

  def headOption = path.headOption

  def head = headOption.get

  def lastOption = path.lastOption

  def last = lastOption.get

  def tail = Cursor(Chain.fromOption(path.uncons).flatMap { case (_, tl) => tl })

  def uncons = path.uncons.map { case (p, tl) => (p, Cursor(tl)) }

  def dropInit = Cursor(Chain.fromOption(path.initLast).flatMap { case (c, _) => c })

  lazy val formatted = {
    val tl = path.map {
      case GraphArc.Field(name) => s".$name"
      case GraphArc.Index(i)    => s"[$i]"
    }
    s"root${tl.mkString_("")}"
  }
}

object Cursor {
    val empty = Cursor(Chain.empty)

    implicit val monoidForCursor = new Monoid[Cursor] {
      def empty = Cursor.empty
      def combine(x: Cursor, y: Cursor) = Cursor(x.path ++ y.path)
    }

    implicit val showForCursor = Show.show[Cursor](_.formatted)
}