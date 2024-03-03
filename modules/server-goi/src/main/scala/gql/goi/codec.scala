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
package gql.goi

import cats.implicits._

object codec {
  def enumCodec[A <: enumeratum.EnumEntry](e: enumeratum.Enum[A], name: String): IDCodec[A] =
    IDCodec.make(s => e.withNameEither(s).leftMap(_.getMessage()), _.entryName, name)

  val string: IDCodec[String] = IDCodec.stringInstance

  val int: IDCodec[Int] = IDCodec.intInstance

  val long: IDCodec[Long] = IDCodec.longInstance

  val float: IDCodec[Float] = IDCodec.floatInstance

  val double: IDCodec[Double] = IDCodec.doubleInstance

  val boolean: IDCodec[Boolean] = IDCodec.booleanInstance

  val uuid: IDCodec[java.util.UUID] = IDCodec.uuidInstance

  val localDate: IDCodec[java.time.LocalDate] = IDCodec.dateInstance
}
