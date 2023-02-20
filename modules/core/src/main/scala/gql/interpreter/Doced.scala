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

import org.typelevel.paiges._

// Like show, but for docs.
// The doc algorithm is global, so we need to keep things in "doc" for as long as possible to get the best results.
trait Doced[A] {
  def apply(a: A): Doc
}

object Doced {
  def apply[A](implicit ev: Doced[A]): Doced[A] = ev

  def doc[A](a: A)(implicit ev: Doced[A]): Doc = ev(a)

  def from[A](f: A => Doc): Doced[A] = f(_)

  def empty[A]: Doced[A] = _ => Doc.empty
}
