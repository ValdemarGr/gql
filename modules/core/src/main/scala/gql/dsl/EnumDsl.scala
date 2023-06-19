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
package gql.dsl

import gql.ast._
import cats.data._

trait EnumDslFull {
  def enumVal[A](value: A): EnumValue[A] =
    EnumValue(value)

  def enumType[A](name: String, hd: (String, EnumValue[? <: A]), tl: (String, EnumValue[? <: A])*) =
    Enum[A](name, NonEmptyList(hd, tl.toList))
}

object EnumDsl extends EnumDslFull
