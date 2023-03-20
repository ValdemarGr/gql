package gql

import gql.parser.{QueryAst => QA, Value => V, AnyValue, Const}
import cats.data._
import io.circe._
import cats.mtl._
import cats._
import cats.implicits._
import gql.parser.QueryAst
import gql.parser.Pos
import gql.ast._
import gql.Arg
import gql.InverseModifierStack
import gql.ModifierStack

package object preparation {
    def pValueName(v: V[AnyValue]): String = {
    import V._
    v match {
      case ObjectValue(_)   => "object"
      case StringValue(_)   => "string"
      case ListValue(_)     => "list"
      case V.EnumValue(_)   => "enum"
      case BooleanValue(_)  => "boolean"
      case NullValue()      => "null"
      case FloatValue(_)    => "float"
      case IntValue(_)      => "int"
      case VariableValue(_) => "variable"
    }
  }
}
