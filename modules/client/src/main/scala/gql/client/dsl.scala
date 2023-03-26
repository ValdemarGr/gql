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
package gql.client

import cats.data._
import cats.implicits._
import gql._
import gql.parser.{Value => V, QueryAst => P, AnyValue, Const}
import java.util.UUID
import java.time.LocalDate
import gql.std.Sourced
import org.tpolecat.sourcepos.SourcePos

object dsl {
  def sel[A](fieldName: String, alias: String)(implicit sq: Sourced[SubQuery[A]]): SelectionSet[A] =
    SelectionSet.lift(sq.map(Selection.Field(fieldName, Some(alias), Nil, _)))

  def sel[A](fieldName: String)(implicit sq: Sourced[SubQuery[A]]): SelectionSet[A] =
    SelectionSet.lift(sq.map(Selection.Field(fieldName, None, Nil, _)))

  def sel[A](fieldName: String, argHd: P.Argument, argTl: P.Argument*)(implicit sq: Sourced[SubQuery[A]]): SelectionSet[A] =
    SelectionSet.lift(sq.map(Selection.Field(fieldName, None, argHd :: argTl.toList, _)))

  def sel[A](fieldName: String, alias: String, argHd: P.Argument, argTl: P.Argument*)(implicit sq: Sourced[SubQuery[A]]): SelectionSet[A] =
    SelectionSet.lift(sq.map(Selection.Field(fieldName, Some(alias), argHd :: argTl.toList, _)))

  def inlineFrag[A](on: String, matchAlso: String*)(implicit q: Sourced[SelectionSet[A]]): SelectionSet[Option[A]] =
    SelectionSet.lift(q.map(Selection.InlineFragment(on, Chain.fromSeq(matchAlso), _)))

  def fragment[A](name: String, on: String, matchAlso: String*)(implicit q: Sourced[SelectionSet[A]]): SelectionSet[Option[A]] =
    SelectionSet.lift(q.map(Selection.Fragment(name, on, Chain.fromSeq(matchAlso), _)))

  def list[A](implicit sq: SubQuery[A]): SubQuery[List[A]] = ListModifier(sq)

  def opt[A](implicit sq: SubQuery[A]): SubQuery[Option[A]] = OptionModifier(sq)

  def oneOf[A](hd: SelectionSet[Option[A]], tl: SelectionSet[Option[A]]*)(implicit sp: SourcePos): SelectionSet[Option[A]] =
    NonEmptyChain
      .of(hd, tl: _*)
      .nonEmptySequence
      .emap { xs =>
        val hd = xs.collect { case Some(x) => x }
        if (hd.length > 1) Left("More than one sub-SelectionSet matched")
        else Right(hd.headOption)
      }

  def variable[A](name: String)(implicit
      tn: Typename[A],
      encoder: io.circe.Encoder[A]
  ): Var[A, VariableName[A]] = Var[A](name, tn.stack.invert.show(identity))

  def variable[A](name: String, default: V[Const])(implicit
      tn: Typename[A],
      encoder: io.circe.Encoder[A]
  ): Var[Option[A], VariableName[A]] = Var[A](name, tn.stack.invert.show(identity), default)

  def value[A](a: A)(implicit enc: io.circe.Encoder[A]) =
    V.fromJson(enc(a))

  def arg(name: String, value: V[AnyValue]): P.Argument =
    P.Argument(name, value)

  def arg(name: String, vn: VariableName[?]): P.Argument =
    P.Argument(name, vn.asValue)

  def arg[A](name: String, a: A)(implicit enc: io.circe.Encoder[A]): P.Argument =
    P.Argument(name, value(a))

  def embed[A](implicit ss: SelectionSet[A]): SelectionSet[A] = ss

  final case class Typename[A](stack: InverseModifierStack[String]) {
    def push[B](m: InverseModifier): Typename[B] = Typename(stack.push(m))
  }
  object Typename {
    implicit def typenameStackForList[A](implicit ta: Typename[A]): Typename[List[A]] =
      ta.push(InverseModifier.List)

    implicit def typenameStackForOption[A](implicit ta: Typename[A]): Typename[Option[A]] =
      ta.push(InverseModifier.Optional)

    implicit lazy val typenameForString: Typename[String] =
      Typename(InverseModifierStack(Nil, "String"))

    implicit lazy val typenameForInt: Typename[Int] =
      Typename(InverseModifierStack(Nil, "Int"))

    implicit lazy val typenameForUUID: Typename[UUID] =
      Typename(InverseModifierStack(Nil, "UUID"))

    implicit lazy val typenameForFloat: Typename[Float] =
      Typename(InverseModifierStack(Nil, "Float"))

    implicit lazy val typenameForBoolean: Typename[Boolean] =
      Typename(InverseModifierStack(Nil, "Boolean"))

    implicit lazy val typenameForLocalDate: Typename[LocalDate] =
      Typename(InverseModifierStack(Nil, "String"))
  }

  implicit class SyntaxForOptionalSelectionSet[A](q: SelectionSet[Option[A]]) {
    def required: SelectionSet[A] =
      q.emap(_.toRight("Required field was null"))
  }
}
