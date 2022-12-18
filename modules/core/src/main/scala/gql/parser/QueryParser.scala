/*
 * Copyright 2022 Valdemar Grange
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
import cats.data.NonEmptyList

// https://spec.graphql.org/June2018/#sec-Source-Text
object QueryParser {
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

  sealed trait ExecutableDefinition
  object ExecutableDefinition {
    final case class Operation(o: Pos[OperationDefinition]) extends ExecutableDefinition
    final case class Fragment(f: Pos[FragmentDefinition]) extends ExecutableDefinition
  }
  lazy val executableDefinition = seps0.with1 *> {
    import ExecutableDefinition._
    Pos.pos(fragmentDefinition).map(Fragment(_)) |
      Pos.pos(operationDefinition).map(Operation(_))
  }

  sealed trait OperationDefinition
  object OperationDefinition {
    final case class Detailed(
        tpe: OperationType,
        name: Option[String],
        variableDefinitions: Option[VariableDefinitions],
        selectionSet: SelectionSet
    ) extends OperationDefinition

    final case class Simple(selectionSet: SelectionSet) extends OperationDefinition
  }
  lazy val operationDefinition = {
    import OperationDefinition._
    P.backtrack(selectionSet).map(Simple(_)) |
      (operationType ~ name.? ~ variableDefinitions.? ~ selectionSet).map { case (((opt, name), vars), ss) =>
        Detailed(opt, name, vars, ss)
      }

  }

  sealed trait OperationType
  object OperationType {
    case object Query extends OperationType
    case object Mutation extends OperationType
    case object Subscription extends OperationType
  }

  lazy val operationType = {
    import OperationType._

    s("query").as(Query) |
      s("mutation").as(Mutation) |
      s("subscription").as(Subscription)
  }

  final case class SelectionSet(selections: NonEmptyList[Pos[Selection]])
  lazy val selectionSet: P[SelectionSet] = P.defer {
    Pos.pos(selection).rep.between(t('{'), t('}')).map(SelectionSet(_))
  }

  sealed trait Selection
  object Selection {
    final case class FieldSelection(field: Field) extends Selection
    final case class FragmentSpreadSelection(fragmentSpread: FragmentSpread) extends Selection
    final case class InlineFragmentSelection(inlineFragment: InlineFragment) extends Selection
  }
  lazy val selection: P[Selection] = {
    import Selection._
    field.map(FieldSelection(_)) |
      // expects on, backtrack on failure
      P.backtrack(fragmentSpread.map(FragmentSpreadSelection(_))) |
      inlineFragment.map(InlineFragmentSelection(_))

  }

  final case class Field(
      alias: Option[String],
      name: String,
      arguments: Option[Arguments],
      selectionSet: Pos[Option[SelectionSet]]
  )
  lazy val field: P[Field] = P.defer {
    (P.backtrack(alias).?.with1 ~ name ~ arguments.? ~ Pos.pos0(selectionSet.?))
      .map { case (((a, n), args), s) => Field(a, n, args, s) }
  }

  lazy val alias = name <* t(':')

  final case class Arguments(nel: NonEmptyList[Argument])
  lazy val arguments =
    argument.rep.between(t('('), t(')')).map(Arguments.apply)

  final case class Argument(name: String, value: Value)
  lazy val argument =
    (name ~ (t(':') *> value)).map { case (n, v) => Argument(n, v) }

  final case class FragmentSpread(fragmentName: String)
  lazy val fragmentSpread =
    (s("...") *> fragmentName).map(FragmentSpread.apply)

  final case class InlineFragment(typeCondition: String, selectionSet: SelectionSet)
  lazy val inlineFragment =
    ((s("...") *> typeCondition).soft ~ selectionSet).map { case (t, s) => InlineFragment(t, s) }

  final case class FragmentDefinition(
      name: String,
      typeCnd: String,
      selectionSet: SelectionSet
  )
  lazy val fragmentDefinition =
    (s("fragment") *> fragmentName ~ typeCondition ~ selectionSet).map { case ((n, t), s) =>
      FragmentDefinition(n, t, s)
    }

  lazy val fragmentName: P[String] =
    (!s("on")).with1 *> name

  lazy val typeCondition: P[String] =
    s("on") *> name

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
    final case class ObjectValue(v: List[(String, Value)]) extends Value
  }
  lazy val value: P[Value] = P.defer {
    import Value._
    variable.map(VariableValue(_)) |
      p(Numbers.jsonNumber).map { s =>
        val n = BigDecimal(s)
        if (n.scale <= 0) Value.IntValue(n.toBigInt)
        else Value.FloatValue(n)
      } |
      stringValue.map(StringValue(_)) |
      booleanValue.map(BooleanValue(_)) |
      nullValue.as(NullValue) |
      enumValue.map(EnumValue(_)) |
      listValue.map(ListValue(_)) |
      objectValue.map(ObjectValue(_))
  }

  lazy val booleanValue =
    s("true").as(true) |
      s("false").as(false)

  lazy val nullValue: P[Unit] =
    s("null")

  lazy val enumValue: P[String] =
    (!(booleanValue | nullValue)).with1 *> name

  lazy val listValue = value.rep0.with1.between(t('['), t(']'))

  lazy val objectValue = objectField.rep.between(t('{'), t('}')).map(_.toList)

  lazy val objectField = name ~ (t(':') *> value)

  final case class VariableDefinitions(nel: NonEmptyList[Pos[VariableDefinition]])
  lazy val variableDefinitions =
    variableDefinition.rep
      .between(t('('), t(')'))
      .map(VariableDefinitions(_))

  final case class VariableDefinition(name: String, tpe: Type, defaultValue: Option[Value])
  lazy val variableDefinition = Pos.pos {
    (variable ~ (t(':') *> `type`) ~ defaultValue.?).map { case ((n, t), d) => VariableDefinition(n, t, d) }
  }

  lazy val variable =
    t('$') *> name

  lazy val defaultValue = t('=') *> value

  lazy val namedType = name.map(Type.Named(_))

  lazy val nonNullType: P[Type] =
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

  sealed trait Type
  object Type {
    final case class Named(name: String) extends Type
    final case class List(of: Type) extends Type
    final case class NonNull(of: Type) extends Type
  }
  lazy val `type`: P[Type] = {
    import Type._
    (P.defer(namedType | listType.map(List(_))) ~ t('!').?).map {
      case (x, Some(_)) => NonNull(x)
      case (x, None)    => x
    }
  }

  lazy val listType: P[Type] =
    `type`.between(t('['), t(']'))
}
