package gql.dsl

import gql.ast._
import cats.data._
import cats._
import scala.reflect.ClassTag
import gql.parser.{Value => V, Const, QueryAst => QA}
import gql._

trait EnumDslFull {
  def enumVal[A](value: A): EnumValue[A] =
    EnumValue(value)

  def enumType[A](name: String, hd: (String, EnumValue[? <: A]), tl: (String, EnumValue[? <: A])*) =
    Enum[A](name, NonEmptyList(hd, tl.toList))
}

object EnumDsl extends EnumDslFull
