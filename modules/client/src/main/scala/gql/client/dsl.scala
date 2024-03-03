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

object dsl {
  def directive(name: String, args: P.Argument[Unit, AnyValue]*): P.Directive[Unit, AnyValue] =
    P.Directive(name, args.toList.toNel.map(P.Arguments(_)))

  object sel {
    def apply[A](fieldName: String)(implicit sq: SubQuery[A]): SelectionSet[A] =
      build[A](fieldName, identity)

    def build[A](fieldName: String, f: Selection.Field[A] => Selection.Field[A])(implicit sq: SubQuery[A]): SelectionSet[A] =
      SelectionSet.lift(f(Selection.Field(fieldName, None, Nil, sq, Nil)))
  }

  object inlineFrag {
    def apply[A](on: String)(implicit q: SelectionSet[A]): SelectionSet[Option[A]] =
      build[A](on, x => x)

    def build[A](on: String, f: Selection.InlineFragment[A] => Selection.InlineFragment[A])(implicit
        q: SelectionSet[A]
    ): SelectionSet[Option[A]] =
      SelectionSet.lift(f(Selection.InlineFragment(on, q, Nil)))
  }

  object fragment {
    def apply[A](name: String, on: String)(implicit q: SelectionSet[A]): Fragment[A] =
      Fragment(name, on, q, Nil)

    object spread {
      def apply[A](implicit frag: Fragment[A]): SelectionSet[Option[A]] =
        build[A](identity)

      def build[A](f: Selection.FragmentSpread[A] => Selection.FragmentSpread[A])(implicit
          frag: Fragment[A]
      ): SelectionSet[Option[A]] =
        SelectionSet.lift(f(Selection.FragmentSpread(frag, Nil)))
    }
  }

  def list[A](implicit sq: SubQuery[A]): SubQuery[List[A]] = ListModifier(sq)

  def opt[A](implicit sq: SubQuery[A]): SubQuery[Option[A]] = OptionModifier(sq)

  def oneOf[A](hd: SelectionSet[Option[A]], tl: SelectionSet[Option[A]]*): SelectionSet[Option[A]] =
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

  def omittableVariable[A](name: String, default: V[Const, Unit])(implicit
      tn: Typename[A],
      encoder: io.circe.Encoder[A]
  ): Var[Option[A], VariableName[A]] = Var[A](name, tn.stack.invert.show(identity), Some(default))

  def omittableVariable[A](name: String)(implicit
      tn: Typename[A],
      encoder: io.circe.Encoder[A]
  ): Var[Option[A], VariableName[A]] = Var[A](name, tn.stack.invert.show(identity), None)

  def value[A](a: A)(implicit enc: io.circe.Encoder[A]) =
    V.fromJson(enc(a))

  def arg(name: String, value: V[AnyValue, Unit]): P.Argument[Unit, AnyValue] =
    P.Argument(name, value)

  def arg(name: String, vn: VariableName[?]): P.Argument[Unit, AnyValue] =
    P.Argument(name, vn.asValue)

  def arg[A](name: String, a: A)(implicit enc: io.circe.Encoder[A]): P.Argument[Unit, AnyValue] =
    P.Argument(name, value(a))

  def embed[A](implicit ss: SelectionSet[A]): SelectionSet[A] = ss

  def typename[A](name: String): Typename[A] = Typename(InverseModifierStack(Nil, name))

  final case class Typename[A](stack: InverseModifierStack[String]) {
    def push[B](m: InverseModifier): Typename[B] = Typename(stack.push(m))
  }
  object Typename {
    implicit def typenameStackForList[A](implicit ta: Typename[A]): Typename[List[A]] =
      ta.push(InverseModifier.List)

    implicit def typenameStackForOption[A](implicit ta: Typename[A]): Typename[Option[A]] =
      ta.push(InverseModifier.Optional)

    implicit lazy val typenameForString: Typename[String] = typename("String")

    implicit lazy val typenameForInt: Typename[Int] = typename("Int")

    implicit lazy val typenameForLong: Typename[Long] = typename("Long")

    implicit lazy val typenameForDouble: Typename[Double] = typename("Double")

    implicit lazy val typenameForBigInt: Typename[BigInt] = typename("BigInt")

    implicit lazy val typenameForBigDecimal: Typename[BigDecimal] = typename("BigDecimal")

    implicit lazy val typenameForUUID: Typename[UUID] = typename("UUID")

    implicit lazy val typenameForFloat: Typename[Float] = typename("Float")

    implicit lazy val typenameForBoolean: Typename[Boolean] = typename("Boolean")
  }

  implicit class SyntaxForOptionalSelectionSet[A](q: SelectionSet[Option[A]]) {
    def required: SelectionSet[A] =
      q.emap(_.toRight("Required field was null"))

    def requiredFragment(name: String, on: String): SelectionSet[A] =
      (sel[String]("__typename"), q).tupled.emap { case (tn, oa) =>
        oa.toRight(s"Expected fragment '${name}' to match the typename `${on}`, but the typename `${tn}` was found.")
      }
  }
}
