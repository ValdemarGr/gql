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
package gql

import cats.data._
import gql.Arg
import gql.parser.AnyValue
import gql.parser.Const
import gql.parser.{Value => V}
import gql.resolver._
import io.circe._

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
