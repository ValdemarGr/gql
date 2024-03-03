/*
 * Copyright 2024 Valdemar Grange
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
package gql.client.codegen

import gql.parser.{QueryAst => QA}
import gql.parser.{Value => V, AnyValue}
import cats._
import org.typelevel.paiges.Doc
import cats.implicits._

object RenderHelpers {
  def modifyHead(f: Char => Char): String => String = { str =>
    val hd = str.headOption.map(f(_).toString()).getOrElse("")
    hd + str.drop(1)
  }

  val toCaml: String => String =
    modifyHead(_.toLower)

  val toPascal: String => String =
    modifyHead(_.toUpper)

  def escapeFieldName(name: String): String =
    if (reservedScalaWords.contains(name)) s"`$name`" else name

  def scalaField(name: String, tpe: String): Doc =
    Doc.text(escapeFieldName(name)) + Doc.char(':') + Doc.space + Doc.text(tpe)

  def hardIntercalate(left: Doc, right: Doc, xs: List[Doc], sep: Doc = Doc.empty) = {
    left +
      (Doc.hardLine + Doc.intercalate(sep + Doc.hardLine, xs)).nested(2).grouped +
      right
  }

  def hardIntercalateBracket(left: Char, sep: Doc = Doc.empty)(xs: List[Doc])(right: Char) =
    hardIntercalate(Doc.char(left), Doc.hardLine + Doc.char(right), xs, sep)

  def quoted(doc: Doc): Doc =
    Doc.char('"') + doc + Doc.char('"')

  def quoted(str: String): Doc = quoted(Doc.text(str))

  def params(xs: List[Doc]): Doc =
    Doc.intercalate(Doc.comma + Doc.space, xs).tightBracketBy(Doc.char('('), Doc.char(')'))

  def blockScope(prefix: Doc)(body: Doc*): Doc =
    hardIntercalate(Doc.char('{') + prefix, Doc.hardLine + Doc.char('}'), body.toList, Doc.hardLine)

  def typeParams(xs: List[Doc]): Doc =
    Doc.intercalate(Doc.comma + Doc.space, xs).tightBracketBy(Doc.char('['), Doc.char(']'))

  def method(name: String, ps: List[Doc]): Doc =
    Doc.char('.') + Doc.text(name) + params(ps)

  def mapN(n: Int)(f: Doc): Doc = {
    val fn = if (n == 1) "map" else "mapN"
    method(fn, List(f))
  }

  def caseClass(name: String, xs: List[Doc], methods: List[Doc]): Doc =
    Doc.text(s"final case class $name") +
      hardIntercalateBracket('(', Doc.comma)(xs)(')') + methods.toNel
        .map { ms =>
          val d = Doc.hardLine + Doc.intercalate(Doc.hardLine + Doc.hardLine, ms.toList)

          Doc.text(" {") +
            d.grouped.nested(2) +
            Doc.hardLine + Doc.char('}')
        }
        .getOrElse(Doc.empty)

  def verticalApply(name: String, params: List[Doc]): Doc =
    Doc.text(name) + hardIntercalateBracket('(', Doc.comma)(params)(')')

  def imp(t: String) = Doc.text(s"import $t")

  def obj[G[_]: Foldable](name: String, body: G[Doc]): Doc =
    Doc.text("object") + Doc.space + Doc.text(name) + Doc.space +
      hardIntercalateBracket('{', Doc.hardLine)(body.toList)('}')

  def optUnless(cond: Boolean)(tpe: String): String =
    if (cond) tpe else s"Option[${tpe}]"

  def generateValue[C](v: V[AnyValue, C], anyValue: Boolean): Doc = {
    import V._
    val tpe = if (anyValue) "AnyValue" else "Const"
    v match {
      case IntValue(v, _)     => Doc.text(s"V.IntValue(${v.toString()})")
      case StringValue(v, _)  => Doc.text(s"""V.StringValue("$v")""")
      case FloatValue(v, _)   => Doc.text(s"""V.FloatValue("$v")""")
      case NullValue(_)       => Doc.text(s"""V.NullValue()""")
      case BooleanValue(v, _) => Doc.text(s"""V.BooleanValue(${v.toString()})""")
      case ListValue(v, _) =>
        Doc.text(s"V.ListValue[${tpe}, Unit](") +
          Doc
            .intercalate(Doc.comma + Doc.line, v.map(generateValue(_, anyValue)))
            .tightBracketBy(Doc.text("List("), Doc.char(')')) +
          Doc.text(")")
      case ObjectValue(fields, _) =>
        Doc.text(s"V.ObjectValue[${tpe}, Unit](") +
          Doc
            .intercalate(
              Doc.comma + Doc.line,
              fields.map { case (k, v) => quoted(k) + Doc.text(" -> ") + generateValue(v, anyValue) }
            )
            .bracketBy(Doc.text("List("), Doc.char(')')) +
          Doc.text(")")
      case EnumValue(v, _)     => Doc.text(s"""V.EnumValue("$v")""")
      case VariableValue(v, _) => Doc.text(s"""V.VariableValue("$v")""")
    }
  }

  def generateArgument[C](a: QA.Argument[C, AnyValue]): Doc = {
    Doc.text("arg") + params(List(quoted(a.name), generateValue(a.value, anyValue = true)))
  }

  def generateDirective[C](d: QA.Directive[C, AnyValue]): Doc = {
    val xs = d.arguments.toList.flatMap(_.nel.toList).map(generateArgument)
    Doc.text("directive") + params(quoted(d.name) :: xs)
  }

  // https://github.com/rgueldem/ScalaPB/blob/5bd28cb38728e29dc3743a695b98709243f89381/compiler-plugin/src/main/scala/scalapb/compiler/DescriptorImplicits.scala#L1106
  lazy val reservedScalaWords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "given",
    "if",
    "implicit",
    "import",
    "infix",
    "inline",
    "lazy",
    "macro",
    "match",
    "ne",
    "new",
    "null",
    "object",
    "opaque",
    "open",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "transparent",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )
}
