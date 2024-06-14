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
package gql.parser

import cats.implicits._
import cats.parse.{Parser => P}
import cats.parse.Rfc5234
import cats.parse.Numbers
import cats.parse.Caret
import QueryAst._

// https://spec.graphql.org/June2018/#sec-Source-Text
object GraphqlParser {
  val whiteSpace = Rfc5234.wsp

  // if this is slow, use charWhile
  val sourceCharacter: P[Char] =
    P.charIn(('\u0020' to '\uFFFF') :+ '\u0009' :+ '\u000A' :+ '\u000D')

  val lineTerminator = Rfc5234.lf | Rfc5234.crlf | Rfc5234.cr

  val comment: P[Unit] = (P.char('#') ~ sourceCharacter.repUntil0(lineTerminator)).void

  val sep = lineTerminator | whiteSpace | P.char(',') | comment
  val seps0 = sep.rep0.void

  def p[A](p: P[A]): P[A] = p <* sep.rep0.void
  def t(c: Char): P[Unit] = p(P.char(c))
  def s(s: String): P[Unit] = p(P.string(s))

  lazy val name: P[String] = p {
    val rng = ('a' to 'z') :++ ('A' to 'Z') :+ '_'
    val begin = P.charIn(rng)

    val tl = begin | Numbers.digit

    (begin ~ tl.rep0).map { case (c, s) => s"$c${s.mkString}" }
  }

  def constValueFields[A >: Const <: AnyValue](vp: => P[Value[A, Caret]]): P[Value[A, Caret]] = P.defer {
    import Value._
    val numParser: P[Value[Const, Caret]] = Pos.pos(p(Numbers.jsonNumber)).map { ps =>
      val n = BigDecimal(ps.value)
      if (n.scale <= 0) Value.IntValue[Caret](n.toBigInt, ps.caret)
      else Value.FloatValue[Caret](n, ps.caret)
    }

    val strParser: P[Value[Const, Caret]] = Pos.pos(stringValue).map(p => StringValue(p.value, p.caret))
    val boolParser: P[Value[Const, Caret]] = Pos.pos(booleanValue).map(p => BooleanValue(p.value, p.caret))
    val nullParser: P[Value[Const, Caret]] = Pos.pos(nullValue).map(p => NullValue(p.caret))
    val enumParser: P[Value[Const, Caret]] = Pos.pos(enumValue).map(p => EnumValue(p.value, p.caret))

    val p1: P[Value[Const, Caret]] = numParser |
      strParser |
      boolParser |
      nullParser |
      enumParser

    val p2: P[Value[A, Caret]] =
      Pos.pos(listValue(vp)).map(p => ListValue[A, Caret](p.value, p.caret)) |
        Pos.pos(objectValue(vp)).map(p => ObjectValue[A, Caret](p.value, p.caret))

    p1 | p2
  }

  lazy val anyValueFields: P[Value[AnyValue, Caret]] = P.defer {
    import Value._
    Pos.pos(variable).map(p => VariableValue(p.value, p.caret))
  }

  lazy val constValue: P[Value[Const, Caret]] = constValueFields(constValue) /* {
    lazy val l: P[Value[Const, Caret]] = constValueFields(l)
    l
  } | value.flatMap(_ => P.failWith("Variable must not occur in a const context"))*/

  lazy val value: P[Value[AnyValue, Caret]] = anyValueFields | constValueFields(value)

  def arguments[A >: Const <: AnyValue](vp: => P[Value[A, Caret]]) =
    argument(vp).rep.between(t('('), t(')')).map(QueryAst.Arguments.apply)

  def argument[A >: Const <: AnyValue](vp: => P[Value[A, Caret]]) =
    (name ~ (t(':') *> vp)).map { case (n, v) => QueryAst.Argument(n, v) }

  lazy val argumentsConst = arguments(constValue)

  lazy val argumentsAny = arguments(value)

  lazy val booleanValue =
    s("true").as(true) |
      s("false").as(false)

  lazy val nullValue: P[Unit] =
    s("null")

  lazy val enumValue: P[String] =
    (!(booleanValue.void | nullValue)).with1 *> name

  def listValue[A <: AnyValue](vp: P[Value[A, Caret]]): P[List[Value[A, Caret]]] =
    vp.rep0.with1.between(t('['), t(']'))

  def objectValue[A <: AnyValue](vp: P[Value[A, Caret]]): P[List[(String, Value[A, Caret])]] =
    t('{').soft *> objectField(vp).rep0 <* t('}')

  def objectField[A <: AnyValue](vp: P[Value[A, Caret]]): P[(String, Value[A, Caret])] =
    name ~ (t(':') *> vp)

  lazy val variable =
    t('$') *> name

  def defaultValue[A <: AnyValue](p: => P[Value[A, Caret]]) = t('=') *> p

  lazy val namedType: P[NonNullType] = name.map(Type.Named(_))

  lazy val nonNullType: P[NonNullType] =
    namedType <* t('!') |
      listType <* t('!')

  lazy val description = stringValue

  lazy val intValue: P[BigInt] = p(Numbers.bigInt)

  lazy val floatValue: P[BigDecimal] = p(Numbers.jsonNumber).map(BigDecimal(_))

  lazy val stringValue: P[String] = p {
    val d = P.char('"')
    d *> (
      ((P.backtrack(d.rep(2, 2)) *> blockStringCharacter.rep0) <* d.rep(3, 3)) |
        (stringCharacter.rep0.with1 <* d)
    )
  }.map(_.mkString_(""))

  lazy val stringCharacter: P[String] =
    ((!(P.charIn('"', '\\').void | lineTerminator)).with1 *> sourceCharacter.map(_.toString())) |
      (P.string("\\u").as("\\u") ~ escapedUnicode).map { case (x, y) =>
        x + y
      } |
      (P.charIn('\\') ~ escapedCharacter).map { case (x, y) => x.toString() + y }

  lazy val escapedUnicode: P[String] =
    P.charIn(('0' to '9') :++ ('a' to 'f') :++ ('A' to 'F')).rep(4, 4).map(_.mkString_(""))

  lazy val escapedCharacter: P[Char] =
    P.charIn('"', '\\', '/', 'b', 'f', 'n', 'r', 't')

  lazy val blockStringCharacter: P[String] = {
    val qs = P.string("\"\"\"")
    (!(qs | (P.char('\\') *> qs))).with1 *> sourceCharacter
  }.map(_.toString())

  lazy val `type`: P[Type] = {
    import Type._
    (P.defer(namedType | listType) ~ t('!').?).map {
      case (x, Some(_)) => NonNull(x)
      case (x, None)    => x
    }
  }

  lazy val listType: P[NonNullType] =
    `type`.between(t('['), t(']')).map(Type.List(_))

  def directives[A >: Const <: AnyValue](vp: => P[Value[A, Caret]]): P[Directives[Caret, A]] =
    directive[A](vp).rep.map(Directives(_))

  def directive[A >: Const <: AnyValue](vp: => P[Value[A, Caret]]): P[Directive[Caret, A]] =
    (s("@") *> name ~ arguments(vp).?).map { case (n, a) => Directive(n, a) }

  lazy val directivesConst = directives(constValue)

  lazy val directivesAny = directives(value)
}
