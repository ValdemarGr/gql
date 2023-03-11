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
object SubQuery {
  implicit def terminalDecoder[A](implicit decoder: io.circe.Decoder[A]): SubQuery[A] = Terminal(decoder)
}

final case class Terminal[A](decoder: io.circe.Decoder[A]) extends SubQuery[A]

final case class Query[A](impl: Query.Impl[?, A]) extends SubQuery[A] {
  def vmap[B](f: A => ValidatedNec[String, B]): Query[B] = Query(impl.vmap(f))

  def emap[B](f: A => Either[String, B]): Query[B] = Query(impl.emap(f))
}

object Query {
  final case class Impl[A, B](
      underlying: FreeApply[Selection, A],
      emap: A => ValidatedNec[String, B]
  ) {
    def vmap[C](f: B => ValidatedNec[String, C]): Impl[A, C] = copy(emap = emap.andThen(_ andThen f))

    def emap[C](f: B => Either[String, C]): Impl[A, C] =
      copy(emap = emap.andThen(_.toEither.flatMap(f(_).leftMap(NonEmptyChain.one(_))).toValidated))
  }

  def lift[A](sel: Selection[A]): Query[A] =
    Query(Impl[A, A](FreeApply.lift(sel), _.validNec))

  implicit val applyForQuery: Apply[Query] = {
    new Apply[Query] {
      override def map[A, B](fa: Query[A])(f: A => B): Query[B] = Query {
        fa.impl match {
          case g: Impl[a, A] => Impl[a, B](g.underlying, g.emap.andThen(_.map(f)))
        }
      }

      override def ap[A, B](ff: Query[A => B])(fa: Query[A]): Query[B] = Query {
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

/*
 * A selection occurs in a query and covers the following cases:
 *  - a field { name }
 *  - a fragment { ..}
 *  - an inline fragment
 */
sealed trait Selection[A]

object Selection {
  final case class Field[A](
      fieldname: String,
      alias: Option[String],
      subQuery: SubQuery[A]
  ) extends Selection[A]

  final case class Fragment[A](
      name: String,
      on: String,
      matchAlso: Chain[String],
      subSelection: Query[A]
  ) extends Selection[Option[A]]

  final case class InlineFragment[A](
      on: String,
      matchAlso: Chain[String],
      subSelection: Query[A]
  ) extends Selection[Option[A]]
}