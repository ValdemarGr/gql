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
import gql.resolver._

package object preparation {
  final case class Variable(
      tpe: gql.parser.Type,
      value: Either[Json, V[Const]]
  )
  type VariableMap = Map[String, Variable]

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

  def fieldName[G[_], C](f: FieldInfo[G, C]): String =
    s"'${f.alias.getOrElse(f.name)}'${f.alias.map(x => s" (alias for '$x')").mkString}"

  type UsedArgs = Set[String]

  type Used[F[_], A] = WriterT[F, UsedArgs, A]

  def collectArgs[G[_]](step: Step[G, ?, ?]): Chain[Arg[?]] =
    step match {
      case Step.Alg.Argument(a)   => Chain.one(a)
      case Step.Alg.First(s)      => collectArgs(s)
      case Step.Alg.Choose(l, r)  => collectArgs(l) ++ collectArgs(r)
      case Step.Alg.Compose(l, r) => collectArgs(l) ++ collectArgs(r)
      case _                      => Chain.empty
    }
}
