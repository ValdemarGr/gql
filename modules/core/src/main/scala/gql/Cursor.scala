/*
 * Copyright 2024 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql

import cats.data._
import cats.implicits._
import cats._

/** One arc in a GraphQL path.
  */
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

/** A collection of GraphQL arcs wraped up with an associated algebra. Usually used for reporting where an error occured.
  */
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

  implicit val monoidForCursor: Monoid[Cursor] = new Monoid[Cursor] {
    def empty = Cursor.empty
    def combine(x: Cursor, y: Cursor) = Cursor(x.path ++ y.path)
  }

  implicit val showForCursor: Show[Cursor] = Show.show[Cursor](_.formatted)
}
