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

import cats.implicits._
import cats.parse.{Parser => P}
import cats.parse.Rfc5234
import cats.parse.Numbers

// https://spec.graphql.org/June2018/#sec-Source-Text
object GraphqlParser {
  val whiteSpace = Rfc5234.wsp

  val lineTerminator = Rfc5234.lf | Rfc5234.crlf | Rfc5234.cr

  val sep = lineTerminator | whiteSpace | P.char(',')
  val seps0 = sep.rep0.void

  def p[A](p: P[A]): P[A] = p <* sep.rep0.void
  def t(c: Char): P[Unit] = p(P.char(c))
  def s(s: String): P[Unit] = p(P.string(s))

  // if this is slow, use charWhile
  val sourceCharacter: P[Char] =
    P.charIn(('\u0020' to '\uFFFF') :+ '\u0009' :+ '\u000A' :+ '\u000D')

  lazy val name: P[String] = p {
    val rng = ('a' to 'z') :++ ('A' to 'Z') :+ '_'
    val begin = P.charIn(rng)

    val tl = begin | Numbers.digit

    (begin ~ tl.rep0).map { case (c, s) => s"$c${s.mkString}" }
  }

  def constValueFields[A >: Const <: AnyValue](vp: => P[Value[A]]): P[Value[A]] = P.defer {
    import Value._
    val p1: P[Value[Const]] = p(Numbers.jsonNumber).map { s =>
      val n = BigDecimal(s)
      if (n.scale <= 0) Value.IntValue(n.toBigInt)
      else Value.FloatValue(n)
    } |
      stringValue.map(StringValue(_)) |
      booleanValue.map(BooleanValue(_)) |
      nullValue.as(NullValue()) |
      enumValue.map(EnumValue(_))

    val p2: P[Value[A]] =
      listValue(vp).map(ListValue[A](_)) |
        objectValue(vp).map(ObjectValue[A](_))

    p1 | p2
  }

  lazy val anyValueFields: P[Value[AnyValue]] = P.defer {
    import Value._
    variable.map(VariableValue(_))
  }

  lazy val constValue: P[Value[Const]] = constValueFields(constValue)

  lazy val value: P[Value[AnyValue]] = anyValueFields | constValueFields(value)

  lazy val booleanValue =
    s("true").as(true) |
      s("false").as(false)

  lazy val nullValue: P[Unit] =
    s("null")

  lazy val enumValue: P[String] =
    (!(booleanValue | nullValue)).with1 *> name

  def listValue[A <: AnyValue](vp: P[Value[A]]): P[List[Value[A]]] =
    vp.rep0.with1.between(t('['), t(']'))

  def objectValue[A <: AnyValue](vp: P[Value[A]]): P[List[(String, Value[A])]] =
    objectField(vp).rep.between(t('{'), t('}')).map(_.toList)

  def objectField[A <: AnyValue](vp: P[Value[A]]): P[(String, Value[A])] =
    name ~ (t(':') *> vp)

  lazy val variable =
    t('$') *> name

  def defaultValue[A <: AnyValue](p: => P[Value[A]]) = t('=') *> p

  lazy val namedType: P[NonNullType] = name.map(Type.Named(_))

  lazy val nonNullType: P[NonNullType] =
    namedType <* t('!') |
      listType <* t('!')

  lazy val description = stringValue

  lazy val intValue: P[BigInt] = p(Numbers.bigInt)

  lazy val floatValue: P[BigDecimal] = p(Numbers.jsonNumber).map(BigDecimal(_))

  lazy val stringValue: P[String] = p {
    val d = P.char('"')
    d *> ((d.rep(2, 2) *> (blockStringCharacter.rep0.with1 <* d.rep(3, 3))) | (stringCharacter.rep0.with1 <* d))
  }.map(_.mkString_(""))

  lazy val stringCharacter: P[String] =
    ((!(P.charIn('"', '\\') | lineTerminator)).with1 *> sourceCharacter.map(_.toString())) |
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
}
