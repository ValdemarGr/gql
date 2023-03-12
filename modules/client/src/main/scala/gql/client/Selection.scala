package gql.client

import cats.data._
import cats._
import cats.implicits._
import gql.std.FreeApply

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
object SubQuery extends  LowPrioritySubQueryImplicits {
  implicit def listForSubQuery[A](implicit subQuery: SubQuery[A]): SubQuery[List[A]] = ListModifier(subQuery)

  implicit def optionForSubQuery[A](implicit subQuery: SubQuery[A]): SubQuery[Option[A]] = OptionModifier(subQuery)
}
trait LowPrioritySubQueryImplicits {
  implicit def terminalForCirceDecoder[A](implicit decoder: io.circe.Decoder[A]): SubQuery[A] = Terminal(decoder)
}

final case class Terminal[A](decoder: io.circe.Decoder[A]) extends SubQuery[A]

final case class ListModifier[A](subQuery: SubQuery[A]) extends SubQuery[List[A]]

final case class OptionModifier[A](subQuery: SubQuery[A]) extends SubQuery[Option[A]]

final case class SelectionSet[A](impl: SelectionSet.Impl[?, A]) extends SubQuery[A] {
  def vmap[B](f: A => ValidatedNec[String, B]): SelectionSet[B] = SelectionSet(impl.vmap(f))

  def emap[B](f: A => Either[String, B]): SelectionSet[B] = SelectionSet(impl.emap(f))
}

object SelectionSet {
  final case class Impl[A, B](
      underlying: FreeApply[Selection, A],
      emap: A => ValidatedNec[String, B]
  ) {
    def vmap[C](f: B => ValidatedNec[String, C]): Impl[A, C] = copy(emap = emap.andThen(_ andThen f))

    def emap[C](f: B => Either[String, C]): Impl[A, C] =
      copy(emap = emap.andThen(_.toEither.flatMap(f(_).leftMap(NonEmptyChain.one(_))).toValidated))
  }

  def lift[A](sel: Selection[A]): SelectionSet[A] =
    SelectionSet(Impl[A, A](FreeApply.lift(sel), _.validNec))

  implicit val applyForQuery: Apply[SelectionSet] = {
    new Apply[SelectionSet] {
      override def map[A, B](fa: SelectionSet[A])(f: A => B): SelectionSet[B] = SelectionSet {
        fa.impl match {
          case g: Impl[a, A] => Impl[a, B](g.underlying, g.emap.andThen(_.map(f)))
        }
      }

      override def ap[A, B](ff: SelectionSet[A => B])(fa: SelectionSet[A]): SelectionSet[B] = SelectionSet {
        (ff.impl, fa.impl) match {
          case (g1: Impl[a1, A => B], g2: Impl[a2, A]) =>
            Impl[ValidatedNec[String, B], B](
              (g1.underlying.map(g1.emap), g2.underlying.map(g2.emap)).mapN((_, _).mapN(_(_))),
              x => x
            )
        }
      }
    }
  }
}

final case class Arg(
    name: String,
    value: gql.parser.QueryParser.Value
)

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
      args: List[Arg],
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
