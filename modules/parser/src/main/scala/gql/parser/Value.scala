package gql.parser

sealed trait Value
object Value {
final case class VariableValue(v: String) extends Value
final case class IntValue(v: BigInt) extends Value
final case class FloatValue(v: BigDecimal) extends Value
final case class StringValue(v: String) extends Value
final case class BooleanValue(v: Boolean) extends Value
case object NullValue extends Value
final case class EnumValue(v: String) extends Value
final case class ListValue(v: List[Value]) extends Value
final case class ObjectValue(v: List[(String, Value)]) extends Value {
    def toMap: Map[String, Value] = v.toMap
}
}
