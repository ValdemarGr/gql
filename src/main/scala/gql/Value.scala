package gql

import io.circe.Json

sealed trait Value {
  def asJson: Json

  def name: String
}

/*
 * GraphQL values are a strict superset of JSON, so in addition to json, extra information is embedded.
 */
object Value {
  final case class JsonValue(value: Json) extends Value {
    override lazy val asJson = value
    override def name: String = value.name
  }
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
  final case class ListValue(v: List[Value]) extends Value {
    override lazy val asJson: Json = Json.fromValues(v.map(_.asJson))
    override def name: String = "List"
  }
  final case class ObjectValue(fields: Map[String, Value]) extends Value {
    override lazy val asJson: Json = Json.fromFields(fields.map { case (k, v) => k -> v.asJson })
    override def name: String = "Object"
  }
}
