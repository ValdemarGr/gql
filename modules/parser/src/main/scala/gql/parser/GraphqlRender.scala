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
package gql.parser

import org.typelevel.paiges._

object GraphqlRender {
  def renderValue(v: Value[AnyValue]): Doc = {
    import Value._
    v match {
      case IntValue(v)     => Doc.text(v.toString)
      case StringValue(v)  => Doc.text(s""""$v"""")
      case FloatValue(v)   => Doc.text(v.toString)
      case NullValue()     => Doc.text("null")
      case BooleanValue(v) => Doc.text(v.toString)
      case ListValue(v) =>
        Doc.intercalate(Doc.comma + Doc.line, v.map(renderValue)).tightBracketBy(Doc.char('['), Doc.char(']'))
      case ObjectValue(fields) =>
        Doc
          .intercalate(
            Doc.comma + Doc.line,
            fields.map { case (k, v) => Doc.text(k) + Doc.text(": ") + renderValue(v) }
          )
          .bracketBy(Doc.char('{'), Doc.char('}'))
      case EnumValue(v)     => Doc.text(v)
      case VariableValue(v) => Doc.text(s"$$${v}")
    }
  }

  def renderType(t: Type): Doc = {
    import Type._
    t match {
      case Named(name) => Doc.text(name)
      case List(t)     => Doc.char('[') + renderType(t) + Doc.char(']')
      case NonNull(t)  => renderType(t) + Doc.char('!')
    }
  }
}
