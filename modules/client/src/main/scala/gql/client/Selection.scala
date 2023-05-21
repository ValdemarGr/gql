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
import cats._
import cats.implicits._
import gql.std.FreeApply
import gql.parser.{QueryAst => P}

/*
 * A SubQuery is either:
 *  - a terminal scalar/enum decoder
 *  - a collection of selections (called Query)
 *
 * Think of it as what can occur on the right hand side of a field
 * { fieldname SubQuery }
 * For instance if it is terminal
 * { fieldname }
 * Or if it is a collection of selections
 * { fieldname { subfield1, subfield2 } }
 */
sealed trait SubQuery[A]
object SubQuery extends LowPrioritySubQueryImplicits {
  implicit def listForSubQuery[A](implicit subQuery: SubQuery[A]): SubQuery[List[A]] = ListModifier(subQuery)

  implicit def optionForSubQuery[A](implicit subQuery: SubQuery[A]): SubQuery[Option[A]] = OptionModifier(subQuery)
}
trait LowPrioritySubQueryImplicits {
  implicit def terminalForCirceDecoder[A](implicit decoder: io.circe.Decoder[A]): SubQuery[A] = Terminal(decoder)
}

final case class Terminal[A](decoder: io.circe.Decoder[A]) extends SubQuery[A]

final case class ListModifier[A](subQuery: SubQuery[A]) extends SubQuery[List[A]]

final case class OptionModifier[A](subQuery: SubQuery[A]) extends SubQuery[Option[A]]

final case class SelectionSet[A](impl: FreeApply[Selection, ValidatedNec[String, A]]) extends SubQuery[A] {
  def vmap[B](f: A => ValidatedNec[String, B]): SelectionSet[B] = SelectionSet(impl.map(_.andThen(f)))

  def emap[B](f: A => Either[String, B]): SelectionSet[B] =
    SelectionSet(impl.map(_.andThen(a => f(a).toValidatedNec)))
}

object SelectionSet {
  // type SourcedSel[A] = Sourced[Selection[A]]

  implicit val applyForSelectionSet: Apply[SelectionSet] = {
    new Apply[SelectionSet] {
      override def map[A, B](fa: SelectionSet[A])(f: A => B): SelectionSet[B] = SelectionSet {
        fa.impl.map(_.map(f))
      }

      override def ap[A, B](ff: SelectionSet[A => B])(fa: SelectionSet[A]): SelectionSet[B] = SelectionSet {
        (fa.impl, ff.impl).mapN(_ ap _)
      }
    }
  }

  def lift[A](sel: Selection[A]): SelectionSet[A] =
    SelectionSet(FreeApply.lift[Selection, A](sel).map(_.validNec))
}

/*
 * A selection occurs in a query and covers the following cases:
 *  - a field { name }
 *  - a fragment { ...Frag }
 *  - an inline fragment { ... on Type { name } }
 */
sealed trait Selection[A]

object Selection {
  final case class Field[A](
      fieldName: String,
      alias: Option[String],
      args: List[P.Argument[Unit]],
      subQuery: SubQuery[A]
  ) extends Selection[A]

  final case class Fragment[A](
      name: String,
      on: String,
      subSelection: SelectionSet[A]
  ) extends Selection[Option[A]]

  final case class InlineFragment[A](
      on: String,
      subSelection: SelectionSet[A]
  ) extends Selection[Option[A]]
}
