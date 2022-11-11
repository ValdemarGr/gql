/*
 * Copyright 2022 Valdemar Grange
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

  implicit lazy val semigroupForCursor: Semigroup[Cursor] = new Semigroup[Cursor] {
    override def combine(x: Cursor, y: Cursor): Cursor =
      Cursor(x.path ++ y.path)
  }
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
