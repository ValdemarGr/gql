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
package gql.parser

import io.circe._
import io.circe.syntax._

sealed trait AnyValue
sealed trait Const extends AnyValue

/*
 * GraphQL values are a strict superset of JSON.
 * Types that nest other types such as list and object must preserve the information that GraphQL values contain.
 * Enums and Strings in GraphQL are for instance represented as strings, but a string which is provided as a GraphQL value
 * shouldn't be allowed as an enum value.
 */
sealed trait Value[+V <: AnyValue, C] extends Product with Serializable {
  def translate[A](f: C => A): Value[V, A]
}
sealed trait NonVar[C] extends Value[Const, C]
object Value {
  // Const can safely be converted to AnyValue (With variable)
  // AnyValue cannot safely be converted to Const, since that would lose the variables
  // That is, Const is a subtype of AnyValue
  final case class VariableValue[C](v: String, c: C = ()) extends Value[AnyValue, C] {
    def translate[A](f: C => A): Value[AnyValue, A] = VariableValue(v, f(c))
  }
  final case class IntValue[C](v: BigInt, c: C = ()) extends NonVar[C] {
    def translate[A](f: C => A): Value[Const, A] = IntValue(v, f(c))
  }
  final case class FloatValue[C](v: BigDecimal, c: C = ()) extends NonVar[C] {
    def translate[A](f: C => A): Value[Const, A] = FloatValue(v, f(c))
  }
  final case class StringValue[C](v: String, c: C = ()) extends NonVar[C] {
    def translate[A](f: C => A): Value[Const, A] = StringValue(v, f(c))
  }
  final case class BooleanValue[C](v: Boolean, c: C = ()) extends NonVar[C] {
    def translate[A](f: C => A): Value[Const, A] = BooleanValue(v, f(c))
  }
  final case class NullValue[C](c: C = ()) extends NonVar[C] {
    def translate[A](f: C => A): Value[Const, A] = NullValue(f(c))
  }
  final case class EnumValue[C](v: String, c: C = ()) extends NonVar[C] {
    def translate[A](f: C => A): Value[Const, A] = EnumValue(v, f(c))
  }
  final case class ListValue[+V <: AnyValue, C](v: List[Value[V, C]], c: C = ()) extends Value[V, C] {
    def translate[A](f: C => A): Value[V, A] = ListValue(v.map(_.translate(f)), f(c))
  }
  final case class ObjectValue[+V <: AnyValue, C](v: List[(String, Value[V, C])], c: C = ()) extends Value[V, C] {
    def translate[A](f: C => A): Value[V, A] = ObjectValue(v.map { case (k, v) => k -> v.translate(f) }, f(c))
  }

  def fromJson(j: Json): Value[Const, Unit] = j.fold[Value[Const, Unit]](
    jsonNull = NullValue(),
    jsonBoolean = BooleanValue(_),
    jsonNumber = n => n.toBigInt.map(IntValue(_)).getOrElse(FloatValue(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble)))),
    jsonString = StringValue(_),
    jsonArray = a => ListValue[Const, Unit](a.toList.map(fromJson)),
    jsonObject = jo => ObjectValue[Const, Unit](jo.toList.map { case (k, v) => k -> fromJson(v) })
  )

  implicit val decoder: Decoder[Value[Const, Unit]] = Decoder.decodeJson.map(fromJson)

  implicit def encoder[C]: Encoder[Value[Const, C]] = Encoder.instance[Value[Const, C]] {
    case i: IntValue[C]           => i.v.asJson
    case f: FloatValue[C]         => f.v.asJson
    case s: StringValue[C]        => s.v.asJson
    case b: BooleanValue[C]       => b.v.asJson
    case NullValue(_)             => Json.Null
    case e: EnumValue[C]          => e.v.asJson
    case l: ListValue[Const, C]   => l.v.map(_.asJson).asJson
    case o: ObjectValue[Const, C] => JsonObject.fromIterable(o.v.map { case (k, v) => k -> v.asJson }).asJson
  }
}
