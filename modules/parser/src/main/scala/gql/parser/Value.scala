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
sealed trait Value[+V <: AnyValue] extends Product with Serializable
sealed trait NonVar extends Value[Const]
object Value {
  // Const can safely be converted to AnyValue (With variable)
  // AnyValue cannot safely be converted to Const, since that would lose the variables
  // That is, Const is a subtype of AnyValue
  final case class VariableValue(v: String) extends Value[AnyValue]
  final case class IntValue(v: BigInt) extends NonVar
  final case class FloatValue(v: BigDecimal) extends NonVar
  final case class StringValue(v: String) extends NonVar
  final case class BooleanValue(v: Boolean) extends NonVar
  final case class NullValue() extends NonVar
  final case class EnumValue(v: String) extends NonVar
  final case class ListValue[+V <: AnyValue](v: List[Value[V]]) extends Value[V]
  final case class ObjectValue[+V <: AnyValue](v: List[(String, Value[V])]) extends Value[V]

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
    case i: IntValue     => i.v.asJson
    case f: FloatValue   => f.v.asJson
    case s: StringValue  => s.v.asJson
    case b: BooleanValue => b.v.asJson
    case NullValue()            => Json.Null
    case e: EnumValue    => e.v.asJson
    case l: ListValue[Const]    => l.v.map(_.asJson).asJson
    case o: ObjectValue[Const]  => JsonObject.fromIterable(o.v.map { case (k, v) => k -> v.asJson }).asJson
  }
}
