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
    SelectionSet(impl.map(_.andThen(f(_).toValidatedNec)))
}

object SelectionSet {
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
    SelectionSet(FreeApply.lift(sel).map(_.validNec))
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
      args: List[P.Argument],
      subQuery: SubQuery[A]
  ) extends Selection[A]

  final case class Fragment[A](
      name: String,
      on: String,
      matchAlso: Chain[String],
      subSelection: SelectionSet[A]
  ) extends Selection[Option[A]] {
    val matchAlsoSet: Set[String] = matchAlso.toList.toSet
  }

  final case class InlineFragment[A](
      on: String,
      matchAlso: Chain[String],
      subSelection: SelectionSet[A]
  ) extends Selection[Option[A]] {
    val matchAlsoSet: Set[String] = matchAlso.toList.toSet
  }
}
