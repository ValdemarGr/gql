package gql.relational

import cats.effect._
import gql.ast._
import gql.dsl._
import cats.implicits._
import cats._
import java.util.UUID
import gql.Arg
import gql.EmptyableArg
import skunk.codec.all._
import cats.data._
import gql.resolver.Resolver
import cats.effect.std.AtomicCell
import cats.effect.std.Supervisor
import cats.effect.std.Hotswap
import cats.effect.std.Mutex
import doobie._
import doobie.implicits._

object DoobieIntegraion extends QueryAlgebra with QueryDsl {
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

  trait DoobieTable[A] extends Table[A] {
    def aliased(x: Fragment): Fragment =
      Fragment.const(alias) ++ fr"." ++ x

    def sel[A](x: String, d: Decoder[A]): (Fragment, Query.Select[A]) = {
      val col = aliased(Fragment.const(x))
      col -> Query.Select(Chain(col), d)
    }
  }
}

object ExampleDoobie {
  import DoobieIntegraion._
  import doobie.postgres.implicits._

  case class ContractTable(alias: String) extends DoobieTable[UUID] {
    def table = fr"contract"
    def groupingKey = fr"id"
    def groupingKeyDecoder = Read[UUID]

    val (id, selId) = sel("id", Read[UUID])
    val (name, selName) = sel("name", Read[String])
  }
  val contractTable = table(ContractTable)

  implicit lazy val contract2: Type[IO, QueryResult[ContractTable]] = tpe[IO, QueryResult[ContractTable]](
    "Contract2",
    "name" -> query(_.selName),
    "id" -> query(_.selId)
  )
}