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
trait Value[+V >: Const <: AnyValue]
object Value {
  // Const can safely be converted to AnyValue (With variable)
  // AnyValue cannot safely be converted to Const, since that would lose the variables
  // That is, Const is a subtype of AnyValue
  final case class VariableValue(v: String) extends Value[AnyValue]
  final case class IntValue[+V >: Const <: AnyValue](v: BigInt) extends Value[V]
  final case class FloatValue[+V >: Const <: AnyValue](v: BigDecimal) extends Value[V]
  final case class StringValue[+V >: Const <: AnyValue](v: String) extends Value[V]
  final case class BooleanValue[+V >: Const <: AnyValue](v: Boolean) extends Value[V]
  final case class NullValue[+V >: Const <: AnyValue]() extends Value[V]
  final case class EnumValue[+V >: Const <: AnyValue](v: String) extends Value[V]
  final case class ListValue[+V >: Const <: AnyValue](v: List[Value[V]]) extends Value[V]
  final case class ObjectValue[+V >: Const <: AnyValue](v: List[(String, Value[V])]) extends Value[V]

  def fromJson(j: Json): Value[Const] = j.fold(
    jsonNull = NullValue(),
    jsonBoolean = BooleanValue(_),
    jsonNumber = n => n.toBigInt.map(IntValue(_)).getOrElse(FloatValue(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble)))),
    jsonString = StringValue(_),
    jsonArray = a => ListValue(a.toList.map(fromJson)),
    jsonObject = jo => ObjectValue(jo.toList.map { case (k, v) => k -> fromJson(v) })
  )

  implicit val decoder = Decoder.decodeJson.map(fromJson)

  implicit val encoder: Encoder[Value[Const]] = Encoder.instance[Value[Const]] {
    case i: IntValue[Const]     => i.v.asJson
    case f: FloatValue[Const]   => f.v.asJson
    case s: StringValue[Const]  => s.v.asJson
    case b: BooleanValue[Const] => b.v.asJson
    case NullValue()            => Json.Null
    case e: EnumValue[Const]    => e.v.asJson
    case l: ListValue[Const]    => l.v.map(_.asJson).asJson
    case o: ObjectValue[Const]  => JsonObject.fromIterable(o.v.map { case (k, v) => k -> v.asJson }).asJson
  }
}
