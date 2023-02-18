/*
 * Copyright 2023 Valdemar Grange
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
package gql.interpreter

import cats.data._
import cats._
import cats.implicits._

sealed trait GraphArc
object GraphArc {
  final case class Field(name: String) extends GraphArc
  final case class Index(index: Int) extends GraphArc
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
}

object Cursor {
  def empty = Cursor(Chain.empty)

  implicit lazy val semigroupForCursor: Semigroup[Cursor] = new Semigroup[Cursor] {
    override def combine(x: Cursor, y: Cursor): Cursor =
      Cursor(x.path ++ y.path)
  }

  implicit lazy val showForCursor = Show.show[Cursor]{ c =>
    val tl = c.path.map{
      case GraphArc.Field(name) => s".$name"
      case GraphArc.Index(i) =>  s"[$i]"
    }
    s"root${tl.mkString_("")}"
  }
}