package gql.relational

import gql.ast._
import gql.dsl._
import cats.implicits._
import fs2.Pure
import skunk.codec._
import skunk._
import cats._
import gql.resolver.Resolver
import gql.resolver.Step
import gql.std.FreeApply
import cats.data._

object Testttt {
  // lib
  final case class Projections[F[_], A](columns: NonEmptyChain[String], fa: F[A]) {
    def mapF[G[_], B](f: F[A] => G[B]): Projections[G, B] = copy(fa = f(fa))
  }

  object Projections {
    implicit def applyForProjections[F[_]: Apply]: Apply[Projections[F, *]] = new Apply[Projections[F, *]] {
      override def map[A, B](fa: Projections[F, A])(f: A => B): Projections[F, B] =
        fa.mapF(_.map(f))

      override def ap[A, B](ff: Projections[F, A => B])(fa: Projections[F, A]): Projections[F, B] =
        Projections(ff.columns ++ fa.columns, ff.fa <*> fa.fa)
    }
  }

  final case class Join(table: String, originColumn: String, targetColumn: String)
  type Joins = Chain[Join]

  final case class Query[F[_], A](joins: Joins, projection: Projections[F, A])

  def startJoin(table: String, originColumn: String, targetColumn: String): Joins =
    Chain.one(Join(table, originColumn, targetColumn))

  def resolveJoin[F[_]](table: String, originColumn: String, targetColumn: String): Resolver[F, Joins, Joins] =
    Resolver.lift[F, Joins](xs => xs ++ startJoin(table, originColumn, targetColumn))

  def join[F[_]](table: String, originColumn: String, targetColumn: String)(impl: => Out[F, Joins]): Field[F, Joins, Joins] =
    build.from(resolveJoin[F](table, originColumn, targetColumn))(impl)

  trait RelationalDsl[F[_], G[_]] {
    def select[A](column: String, fa: F[A]): Projections[F, A] =
      Projections(NonEmptyChain.one(column), fa)

    def sel[A](column: String, fa: F[A])(implicit out: => Out[G, A]): Field[G, Joins, A] =
      build.from(select(column, fa).resolve)(out)

    def queryRunner[A]: Resolver[G, Query[F, A], A]

    implicit class ProjectionsOps[A](private val fa: Projections[F, A]) {
      def resolve: Resolver[G, Joins, A] =
        Resolver.lift[G, Joins](joins => Query(joins, fa)) andThen queryRunner[A]
    }
  }

  // skunk
  def queryRunner[F[_], A] = Resolver
    .batch[F, Query[Decoder, A], A] { queries =>
      ???
    }
    .runA(null)
    .value
    .forceOne(null)

  import cats.effect._
  val skunkDsl = new RelationalDsl[Decoder, IO] {
    override def queryRunner[A]: Resolver[IO, Query[Decoder, A], A] = ???
  }
  import skunkDsl._
  import skunk.codec.all._

  lazy val user: Type[IO, Joins] = tpe[IO, Joins](
    "User",
    "id" -> sel("id", uuid),
    "name" -> sel("name", varchar),
    "age" -> sel("age", int4),
    "appearsOn" -> join("contract", "contract_id", "id")(contract)
  )

  lazy val contract: Type[IO, Joins] = tpe[IO, Joins](
    "Contract",
    "id" -> sel("id", uuid),
    "name" -> sel("name", varchar),
    "users" -> join("user", "id", "contract_id")(user)
  )
}
