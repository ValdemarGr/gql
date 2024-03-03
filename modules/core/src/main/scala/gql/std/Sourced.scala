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
package gql.std

import org.tpolecat.sourcepos.SourcePos
import cats._

final case class Sourced[A](value: A, position: SourcePos)

object Sourced extends LowPrioritySourcedImplicits {
  implicit val functorForSourced: Functor[Sourced] = new Functor[Sourced] {
    override def map[A, B](fa: Sourced[A])(f: A => B): Sourced[B] = Sourced(f(fa.value), fa.position)
  }

  implicit def sourcedForAnyTypeclassLike[A](implicit a: A, sp: SourcePos): Sourced[A] = Sourced(a, sp)
}

trait LowPrioritySourcedImplicits {
  implicit def sourcedConversionForAnyValue[A](value: A)(implicit sp: SourcePos): Sourced[A] = Sourced(value, sp)
}
