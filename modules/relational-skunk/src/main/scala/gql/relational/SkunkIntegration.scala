package gql.relational.skunk

import cats.effect._
import skunk.implicits._
import skunk._
import cats._
import cats.data._
import gql.relational.{QueryAlgebra, QueryDsl, LazyResource}

object SkunkIntegration extends QueryAlgebra {
  type Frag = skunk.AppliedFragment

  def stringToFrag(s: String): Frag = sql"#${s}".apply(Void)
  implicit def appliedFragmentMonoid: Monoid[Frag] = skunk.AppliedFragment.MonoidAppFragment

  override implicit def applicativeForDecoder: Applicative[Decoder] =
    Decoder.ApplicativeDecoder

  type Encoder[A] = skunk.Encoder[A]
  type Decoder[A] = skunk.Decoder[A]
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] = d.opt

  type Connection[F[_]] = Resource[F, Session[F]]
  implicit def skunkQueryable[F[_]: MonadCancelThrow]: Queryable[F] = new Queryable[F] {
    def apply[A](query: AppliedFragment, decoder: Decoder[A], connection: Connection[F]): F[List[A]] =
      connection.use(_.execute(query.fragment.query(decoder))(query.argument))
  }
}

object dsl extends QueryDsl(SkunkIntegration) {
  import algebra._
  trait SkunkTable extends Table {
    def aliased[A](x: Fragment[A]): Fragment[A] =
      sql"#${alias}.${x}"

    def sel[A](x: String, d: Decoder[A]): (Fragment[Void], Query.Select[A]) = {
      val col = aliased(sql"#${x}")
      col -> Query.Select(Chain(col.apply(Void)), d)
    }
  }

  trait SkunkTableAlg[T <: Table] extends TableAlg[T] {
    def join[G[_]: QueryAlgebra.JoinType](joinPred: T => Fragment[Void])(implicit dummy: DummyImplicit): Query[G, T] =
      join[G](joinPred.andThen(_.apply(Void)))
  }
  def skunkTable[T <: Table](f: String => T): SkunkTableAlg[T] = new SkunkTableAlg[T] {
    def make: String => T = f
  }
  
  type LazyConnection[F[_]] = LazyResource[F, Session[F]]
  def lazyPool[F[_]: Concurrent](pool: Resource[F, Session[F]]): Resource[F, LazyConnection[F]] =
    LazyResource.fromResource(pool)
}