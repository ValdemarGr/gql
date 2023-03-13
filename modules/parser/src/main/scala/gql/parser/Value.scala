package gql.parser

sealed trait AnyValue
sealed trait Const extends AnyValue
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
}
