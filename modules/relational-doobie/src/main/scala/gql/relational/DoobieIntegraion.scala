package gql.relational.doobie

import cats.effect._
import cats.implicits._
import cats._
import cats.data._
import doobie._
import doobie.implicits._
import gql.relational.{QueryAlgebra, QueryDsl}

object DoobieIntegraion extends QueryAlgebra {
  type Frag = doobie.Fragment
  def stringToFrag(s: String): Frag = doobie.Fragment.const(s)
  implicit def appliedFragmentMonoid: Monoid[Frag] = doobie.Fragment.FragmentMonoid

  type Decoder[A] = doobie.Read[A]
  type Encoder[A] = doobie.Write[A]

  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] =
    new doobie.Read(
      d.gets.map { case (ev, _) => (ev, doobie.enumerated.Nullability.Nullable) },
      { (rs, n) =>
        val xs = d.gets.zipWithIndex.traverse { case ((ev, _), i) => ev.unsafeGetNullable(rs, n + i) }
        xs.as(d.unsafeGet(rs, n))
      }
    )

  implicit def applicativeForDecoder: Applicative[Decoder] = doobie.Read.ReadApply

  type Connection[F[_]] = Transactor[F]
  implicit def doobieQueryable[F[_]: MonadCancelThrow]: Queryable[F] = new Queryable[F] {
    def apply[A](query: Frag, decoder: Decoder[A], connection: Connection[F]): F[List[A]] = 
      query.query(decoder).to[List].transact(connection)
  }
}

object dsl extends QueryDsl(DoobieIntegraion) {
  import algebra._

  trait DoobieTable extends Table {
    def aliased(x: Fragment): Fragment =
      Fragment.const(alias) ++ fr"." ++ x

    def sel[A](x: String, d: Decoder[A]): (Fragment, Query.Select[A]) = {
      val col = aliased(Fragment.const(x))
      col -> Query.Select(Chain(col), d)
    }
  }
}