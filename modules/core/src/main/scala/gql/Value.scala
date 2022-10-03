package gql

import io.circe.Json

sealed trait Value {
  def asJson: Json

  def name: String
}

/*
 * GraphQL values are a strict superset of JSON.
 * Types that nest other types such as list and object must preserve the information that GraphQL values contain.
 * Enums and Strings in GraphQL are for instance represented as strings, but a string which is provided as a GraphQL value
 * shouldn't be allowed as an enum value.
 */
object Value {
  final case class IntValue(v: BigInt) extends Value {
    override lazy val asJson: Json = Json.fromBigInt(v)
    override def name: String = "Int"
  }
  final case class FloatValue(v: BigDecimal) extends Value {
    override lazy val asJson: Json = Json.fromBigDecimal(v)
    override def name: String = "Float"
  }
  final case class StringValue(v: String) extends Value {
    override lazy val asJson: Json = Json.fromString(v)
    override def name: String = "String"
  }
  final case class BooleanValue(v: Boolean) extends Value {
    override lazy val asJson: Json = Json.fromBoolean(v)
    override def name: String = "Boolean"
  }
  case object NullValue extends Value {
    override lazy val asJson: Json = Json.Null
    override def name: String = "Null"
  }
  final case class EnumValue(v: String) extends Value {
    override lazy val asJson: Json = Json.fromString(v)
    override def name: String = "Enum"
  }
  final case class ArrayValue(v: Vector[Value]) extends Value {
    override lazy val asJson: Json = Json.fromValues(v.map(_.asJson))
    override def name: String = "List"
  }
  final case class ObjectValue(fields: Map[String, Value]) extends Value {
    override lazy val asJson: Json = Json.fromFields(fields.map { case (k, v) => k -> v.asJson })
    override def name: String = "Object"
  }

  def fromJson(j: Json): Value = j.fold(
    jsonNull = NullValue,
    jsonBoolean = BooleanValue(_),
    jsonNumber = n => n.toBigInt.map(IntValue(_)).getOrElse(FloatValue(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble)))),
    jsonString = StringValue(_),
    jsonArray = a => ArrayValue(a.map(fromJson)),
    jsonObject = jo => ObjectValue(jo.toMap.view.mapValues(fromJson).toMap)
  )
}
