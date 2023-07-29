package gql.relational

import cats.effect._
import skunk.implicits._
import gql.ast._
import gql.dsl._
import cats.implicits._
import fs2.Pure
import skunk.codec._
import skunk._
import cats._
import gql.resolver.Resolver
import cats.data._
import scala.reflect.ClassTag
import java.util.UUID
import gql.Arg
import gql.EmptyableArg

object Test6 {
  trait TableDef[A] {
    def table: AppliedFragment
    def pk: AppliedFragment
    def pkCodec: Codec[A]
  }

  trait Table[A] extends TableDef[A] {
    def alias: Fragment[Void]

    def aliased[A](x: Fragment[A]): Fragment[A] =
      sql"${alias}.$x"

    def aliased(x: AppliedFragment): AppliedFragment =
      aliased[x.A](x.fragment).apply(x.argument)

    def select[A](name: AppliedFragment, codec: Codec[A]): Select[A] =
      Select(aliased(name), codec)

    def col(name: String): Fragment[Void] =
      aliased(sql"#$name")

    def sel[A](name: String, codec: Codec[A]): (Fragment[Void], Select[A]) = {
      val c = col(name)
      c -> Select(c.apply(Void), codec)
    }

    def selPk: Select[A] = select(pk, pkCodec)
  }

  abstract class TableRef[A](val td: TableDef[A]) extends Table[A] {
    def table = td.table
    def pk = td.pk
    def pkCodec = td.pkCodec
  }

  // type ReadQueryResult[A] = TableFieldAttribute[?, ?, ?, ?, A] => Option[A]

  // case class QueryResult[B](readQueryResult: ReadQueryResult[?])

  trait QueryResult[A] {
    def read[F[_], A0, B, ArgType, Q](tfa: TableFieldAttribute[F, A0, B, ArgType, Q]): Option[Q]
  }

  sealed trait Query[A]
  case class Continue[A](passthrough: A) extends Query[QueryResult[A]]
  case class Select[A](col: AppliedFragment, decoder: Decoder[A]) extends Query[A]
  case class Ap[A, B](f: Query[A => B], a: Query[A]) extends Query[B]
  implicit val applyForQuery: Apply[Query] = ???

  sealed trait JoinType[G[_]]
  object JoinType extends LowPrioJoinTypeImplicits1 {
    // A => A instead of Id since scala doesn't want reduce Id to A => A, and Id is noisy
    case object One extends JoinType[Lambda[A => A]]
    case object Opt extends JoinType[Option]
    case class Traversable[G[_]](val G: Traverse[G]) extends JoinType[G]

    implicit val joinTypeOne: JoinType[Lambda[A => A]] = JoinType.One
  }
  trait LowPrioJoinTypeImplicits1 extends LowPrioJoinTypeImplicits2 {
    implicit val joinTypeOpt: JoinType[Option] = JoinType.Opt
  }
  trait LowPrioJoinTypeImplicits2 {
    implicit def joinTypeTraversable[G[_]](implicit G: Traverse[G]): JoinType[G] = JoinType.Traversable(G)
  }

  case class Join[G[_], A, T <: Table[?]](
      tbl: Fragment[Void] => T,
      joinPred: T => AppliedFragment,
      jt: JoinType[G],
      sq: T => Query[A]
  ) extends Query[G[A]] {
    def andThen[B](f: T => Query[B]): Join[G, B, T] =
      copy[G, B, T](sq = f)
  }

  case class ParentData(
      path: NonEmptyList[(Table[?], Fragment[Void])]
  )

  final case class PartialJoinAnd[G[_]](private val dummy: Boolean = false) {
    def apply[A, T <: Table[?]](f: Fragment[Void] => T)(pred: T => AppliedFragment)(g: T => Query[A])(implicit
        jt: JoinType[G]
    ): Query[G[A]] =
      Join[G, A, T](f, pred, jt, g)
  }

  final case class PartialJoin[G[_]](private val dummy: Boolean = false) {
    def apply[A, T <: Table[?]](f: Fragment[Void] => T, passthrough: A)(pred: T => AppliedFragment)(implicit
        jt: JoinType[G]
    ): Query[G[QueryResult[A]]] =
      Join[G, QueryResult[A], T](f, pred, jt, _ => Continue[A](passthrough))

    def apply[T <: Table[?]](f: Fragment[Void] => T)(pred: T => AppliedFragment)(implicit jt: JoinType[G]): Query[G[QueryResult[T]]] =
      Join[G, QueryResult[T], T](f, pred, jt, t => Continue[T](t))
  }

  def join[G[_]]: PartialJoin[G] = PartialJoin()

  def joinAnd[G[_]]: PartialJoinAnd[G] = PartialJoinAnd()

  def queryResolveFull[F[_], A, B, C, D](a: EmptyableArg[C])(f: (A, C) => Query[B])(
      resolver: Resolver[F, B, B] => Resolver[F, B, D]
  )(implicit tpe: => Out[F, D]): Field[F, QueryResult[A], D] = {
    val tfa = new TableFieldAttribute[F, A, D, C, B] {
      def arg = a
      def query(value: A, argument: C): Query[B] = f(value, argument)
    }

    val r = Resolver.id[F, QueryResult[A]]
    val r2 = a match {
      case EmptyableArg.Empty   => r
      case EmptyableArg.Lift(a) => r.arg(a).map { case (_, qr) => qr }
    }

    val field = build[F, QueryResult[A]](
      _.andThen(r2)
        .emap { qr =>
          val b: Option[B] = qr.read(tfa)
          b.toRightIor("internal query association error, could not read result from query result")
        }
        .andThen(resolver(Resolver.id[F, B]))
    )(tpe)

    field.addAttributes(tfa)
  }

  final class QueryBuilder[F[_], A] {
    def relTpe(
        name: String,
        hd: (String, Field[F, QueryResult[A], ?]),
        tl: (String, Field[F, QueryResult[A], ?])*
    ): Type[F, QueryResult[A]] =
      tpe[F, QueryResult[A]](name, hd, tl: _*)

    def apply[B](f: A => Query[B])(implicit tpe: => Out[F, B]): Field[F, QueryResult[A], B] =
      queryResolveFull[F, A, B, Unit, B](EmptyableArg.Empty) { case (a, _) => f(a) }(identity)(tpe)

    def apply[B, C](a: Arg[C])(f: (A, C) => Query[B])(implicit tpe: => Out[F, B]): Field[F, QueryResult[A], B] =
      queryResolveFull[F, A, B, C, B](EmptyableArg.Lift(a)) { case (a, c) => f(a, c) }(identity)(tpe)

    def resolve[B, C](f: A => Query[B])(resolver: Resolver[F, B, B] => Resolver[F, B, C])(implicit
        tpe: => Out[F, C]
    ): Field[F, QueryResult[A], C] =
      queryResolveFull[F, A, B, Unit, C](EmptyableArg.Empty) { case (a, _) => f(a) }(resolver)(tpe)

    def resolve[B, C, D](a: Arg[C])(f: (A, C) => Query[B])(resolver: Resolver[F, B, B] => Resolver[F, B, D])(implicit
        tpe: => Out[F, D]
    ): Field[F, QueryResult[A], D] =
      queryResolveFull[F, A, B, C, D](EmptyableArg.Lift(a)) { case (a, c) => f(a, c) }(resolver)(tpe)
  }
  def buildQuery[F[_], A]: QueryBuilder[F, A] = new QueryBuilder[F, A]

  def queryBuilder[F[_], A]: PartiallyAppliedQueryBuilder[F, A] = new PartiallyAppliedQueryBuilder[F, A]

  final class PartiallyAppliedQueryBuilder[F[_], A](private val dummy: Boolean = false) {
    def apply[B](f: QueryBuilder[F, A] => B): B = f(buildQuery[F, A])
  }

  def query[F[_], A, B](f: A => Query[B])(implicit tpe: => Out[F, B]): Field[F, QueryResult[A], B] = 
    queryResolveFull[F, A, B, Unit, B](EmptyableArg.Empty) { case (a, _) => f(a) }(identity)(tpe)

  def query[F[_], A, B, C](a: Arg[C])(f: (A, C) => Query[B])(implicit tpe: => Out[F, B]): Field[F, QueryResult[A], B] = 
    queryResolveFull[F, A, B, C, B](EmptyableArg.Lift(a)) { case (a, c) => f(a, c) }(identity)(tpe)

  /*
   Two run strategies:
    1. Explicitly mark fields as "query boundaries", e.g can produce QueryResult from nothing.
    2. Every query can kick-start the query process; every query node can become the root if no active parent query tree is in process.

   Considerations:
    If we break the "join chain" then we should probably see that as a query boundary.
    Unforseen multiplicity can occur if we midlessly join values.

    We define query boundary to be when a query is not associated with any parent query.
    We must stop the "search for query fields" process if we encounter a query boundary.

    Furthermore, seperating queries like this allows query boundaries to be monadic.
    I think being explicit about query boundaries is the way to go.
   */

  def runQuery[F[_], A, B](pool: Resource[F, Session[F]])(f: A => Query[B])(implicit tpe: => Out[F, B]): Field[F, A, B] = ???

  trait TableFieldAttribute[F[_], A, B, ArgType, Q] extends FieldAttribute[F, QueryResult[A], B] {
    def arg: EmptyableArg[ArgType]
    def query(value: A, argument: ArgType): Query[Q]
  }

  trait TableFieldQueryBoundary[F[_], A, B] extends FieldAttribute[F, A, B]

  def relTpe[F[_], A](
      name: String,
      hd: (String, Field[F, QueryResult[A], ?]),
      tl: (String, Field[F, QueryResult[A], ?])*
  ): Type[F, QueryResult[A]] = ???

  import skunk.implicits._
  import skunk.codec.all._
  case class EntityTable(alias: Fragment[Void]) extends Table[UUID] {
    def table = void"entity"
    def pk = void"id"
    def pkCodec = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
    val (age, selAge) = sel("age", int4)
    val (height, selHeight) = sel("height", float8)
  }

  case class ContractTable(alias: Fragment[Void]) extends Table[UUID] {
    def table = void"contract"
    def pk = void"id"
    def pkCodec = uuid

    val (id, selId) = sel("id", uuid)
    val (portfolioId, selPortfolioId) = sel("portfolio_id", uuid)
    val (name, selName) = sel("name", text)
  }

  case class ContractEntityTable(alias: Fragment[Void]) extends Table[UUID] {
    def table = void"contract_entity"
    def pk = void"contract_id"
    def pkCodec = uuid

    val (contractId, selContractId) = sel("contract_id", uuid)
    val (entityId, selEntityId) = sel("entity_id", uuid)
  }

  implicit val entity: Type[IO, QueryResult[EntityTable]] =
    relTpe[IO, EntityTable](
      "Entity",
      "id" -> query(_.selId),
      "name" -> query(_.selName)
    )

  implicit val contract: Type[IO, QueryResult[ContractTable]] = queryBuilder[IO, ContractTable] { qb =>
    qb.relTpe(
      "Contract",
      "id" -> query(_.selId),
      "name" -> query(_.selName),
      "builderName" -> qb(_.selName),
      "doubleName" -> qb.resolve(_.selName)(_.evalMap(x => IO(x + x))),
      "entities" -> query { c =>
        joinAnd[List](ContractEntityTable(_))(cet => sql"${cet.contractId} = ${c.id}".apply(Void)) { cet =>
          join(EntityTable(_))(e => sql"${e.id} = ${cet.entityId}".apply(Void))
        }
      }
    )
  }

  case class Portfolio(id: String)
  implicit val portfolio = tpe[IO, Portfolio](
    "Portfolio",
    "id" -> lift(_.id),
    "contracts" -> runQuery(null) { p =>
      join(ContractTable(_))(c => sql"${c.portfolioId} = $text".apply(p.id))
    }
  )

  // val o: Query[Id[Seq[QueryResult[Unit]]]] =
  //   join(ContractTable)(c => sql"$c.id = 'ololo'".apply(Void)).andThen { c =>
  //     join[Seq](EntityTable)(e => sql"$e.contract_id = $c.id".apply(Void))
  //   }

  /*

  val x =
    joinOne("table_a")(a => sql"$a.id = 'hey'".apply(Void)).andThen{ a =>
      joinList("table_b")(b => sql"$b.a_id = $a.id".apply(Void)).andThen { b =>
        joinOpt("table_c")(c => sql"$c.b_id = $b.id".apply(Void)).andThen { c =>
          select(sql"$c.name", text)
        }
      }
    }: Query[List[Option[String]]]

  x.toField(implicitly[Out[IO, List[Option[String]]]])

  val otherType: Type[IO, QueryResult] = ???

  val y = parent { p =>
    joinOne("table_a")(a => sql"$a.id = 'hey' and $a.parent_id = $p.id".apply(Void)).andThen{ a =>
      joinList("table_b")(b => sql"$b.a_id = $a.id".apply(Void)).andThen { b =>
        joinOne("table_c")(c => sql"$c.b_id = $b.id".apply(Void)).andThen { _ =>
          continue(b) // assume that c was just used as a filter
        }
      }
    }
  }: Query[List[List[QueryResult]]]

  y.toField(lst(lst(otherType)))

  sel(a, b) ~= parent(select(_, b)).toField

  val z = sel("my_col", text): Field[Int, QueryResult, String]

   */

  // sealed trait
}

object Test5 {
  /*
   // v1
   relTpe[IO](
     "Contract",
     "id" -> sel("id", uuid),
     "entities" -> joined(entityIdsArg) { case (parent, entityIds) =>
       for {
         ceg <- innerJoin("contract_entity_group") { t =>
           Equals(parent.col("id"), t.col("contract_id"))
         }
         e <- leftJoin("entity") { t =>
           And(
             Equals(ceg.col("entity_id"), t.col("id")),
             In(e.col("id"), Const(entityIds, uuid.list(entityIds)))
           )
         }
       } yield e
     }
   )

   // v2
   def relType[F[_]](
     name: String,
     columns: (String, Field[F,  QueryResult, ?])*
   ): Type[F, QueryResult] = ???

   def joinOne(str: String)(predFromTableAlias: Fragment[Void] => AppliedFragment): State[Int, Fragment[Void]] = ???

   def joinList(str: String)(predFromTableAlias: Fragment[Void] => AppliedFragment): State[Int, Fragment[Void]] = ???

   def sel[F[_], A](col: String, dec: Decoder[A]): Field[F, QueryResult, A] = ???

   def select[A](col: String, dec: Decoder[A]): Query[A] = ???

   def materialize[F[_]]: Query ~> Field[F, QueryResult, *] = ???

   val entity = relType[IO](
     "Entity",
     "id" -> sel("id", uuid),
     "name" -> sel("name", text),
     "friendlyText" -> (select("name", text), select("age", int4))
       .mapN(_ + " is " + _.toString())
       .toField[F]
   )

   sealed trait Join[G[_]]
   case class Done() extends Join[Id]
   case class One[G[_]](blah: String, next: Join[A]) extends Join[A]
   case class Lst[G[_]](blah: String, next: Join[A]) extends Join[Lambda[A => List[G[A]]]]
   case class Opt[G[_]](blah: String, next: Join[A]) extends Join[Lambda[A => Option[G[A]]]]

   relTpe[IO](
     "Contract",
     "id" -> sel("id", uuid),
     "entities" -> entity.join(entityIdsArg) { entityIds =>
        joinOne("contract_entity_group")(t => sql"$t.id = $c.id").join { ceg =>
          joinList("entity"){ t =>
            sql"$t.id = $ceg.entity_id and $t.id in (${uuid.list(entityIds)})".apply(entityIds)
          }
        }
     }
   )

   // v3
   -||- much utility

   // maybe we can opt for an applicative approach
   // the only problem is the graphql argument
   // maybe we can summon all the join structure first, then construct the predicates later

   relType[IO](
     "Contract",
     "id" -> sel("id", uuid),
     "entities" -> join { parent =>
       joinOne() { ceg =>
         joinLst { e =>

         }
       }
       entity
     }
   )

   */
  object olo {
    sealed trait Join[G[_]]
    case class Done() extends Join[Id]
    case class One[G[_]](next: Join[G]) extends Join[G]
    case class Lst[G[_]](next: Join[G]) extends Join[Lambda[A => List[G[A]]]]
    case class Opt[G[_]](next: Join[G]) extends Join[Lambda[A => Option[G[A]]]]

    def summonFor[F[_], G[_]](f: Unit => Join[G])(implicit summonned: Out[F, G[String]]) = summonned

    val y: Out[Pure, List[Option[Id[String]]]] = summonFor { _ =>
      Lst(One(Opt(Done())))
    }
  }

  sealed trait SkunkRel[A]
  object SkunkRel {
    case class Select[A](column: String, decoder: Decoder[A]) extends SkunkRel[A]

    case class JoinRel[A](j: Join[SkunkRel, A]) extends SkunkRel[A]

    final case class Ap[A, B](fa: SkunkRel[A], ff: SkunkRel[A => B]) extends SkunkRel[B]

    implicit val applyInstance: Apply[SkunkRel] = ???
  }

  case class JoinDeps(column: String, table: String, childColumn: String)
  sealed trait Join[F[_], A]
  object Join {
    case class Cont[F[_], A](fa: Eval[F[A]]) extends Join[F, A]
    case class Inner[F[_], A](deps: JoinDeps, ij: Join[F, A]) extends Join[F, A]
    case class Lst[F[_], A, B](deps: JoinDeps, joinColDecoder: Decoder[B], ij: Join[F, A]) extends Join[F, List[A]]
    case class Opt[F[_], A](deps: JoinDeps, ij: Join[F, A]) extends Join[F, Option[A]]
  }

  // type GlobalJoin[F[_]] = Join[Out[F, *], A]

  def select[A](column: String, dec: Decoder[A]): SkunkRel[A] =
    SkunkRel.Select(column, dec)

  def relJoin[A](col: String, table: String, childCol: String)(inner: SkunkRel[A]): SkunkRel[A] =
    SkunkRel.JoinRel(Join.Inner(JoinDeps(col, table, childCol), Join.Cont(Eval.later(inner))))

  def embed[F[_]](o: => Out[F, QueryResult]): Join[Id, Out[F, QueryResult]] =
    Join.Cont[Id, Out[F, QueryResult]](Eval.later(o))

  def join[F[_]](col: String, table: String, childCol: String)(j: Join[Id, Out[F, QueryResult]]) =
    Join.Inner(JoinDeps(col, table, childCol), j)

  def relJoinList[A, B](col: String, table: String, childCol: String, columnDec: Decoder[B])(inner: SkunkRel[A]): SkunkRel[List[A]] =
    SkunkRel.JoinRel(Join.Lst(JoinDeps(col, table, childCol), columnDec, Join.Cont(Eval.later(inner))))

  type QueryResult = Map[SkunkRel[?], Any]

  final case class SkunkRelFieldAttribute[F[_], A, B](rel: SkunkRel[A]) extends FieldAttribute[F, QueryResult, B]

  final case class SkunkJoinFieldAttribute[F[_], A, B](join: Join[Id, A]) extends FieldAttribute[F, QueryResult, B]

  def resolveWith[F[_], A: ClassTag, B, C](
      sr: SkunkRel[A]
  )(f: Resolver[F, QueryResult, A] => Resolver[F, QueryResult, B])(implicit out: => Out[F, B]): Field[F, QueryResult, B] =
    build
      .from {
        f {
          Resolver.id[F, QueryResult].emap { qr =>
            qr.get(sr) match {
              case Some(a: A) => a.rightIor
              case _          => s"internal query error".leftIor
            }
          }
        }
      }(out)
      .addAttributes(SkunkRelFieldAttribute(sr))

  // def query(j: Join[Id, A])

  import cats.effect._

  relJoinList("id", "contract_user_rel", "contract_id", skunk.codec.all.uuid) {
    relJoin("user_id", "user", "id") {
      (
        select("name", skunk.codec.all.text),
        select("age", skunk.codec.all.int4)
      ).tupled
    }
  }
}

object Test4 {
  // lib
  final case class Rel[F[_], +Representation, A](index: Representation, effect: F[A])

  object Rel {
    implicit def applicativeForRel[F[_]: Applicative, Representation: Monoid]: Applicative[Rel[F, Representation, *]] =
      new Applicative[Rel[F, Representation, *]] {
        override def ap[A, B](ff: Rel[F, Representation, A => B])(fa: Rel[F, Representation, A]): Rel[F, Representation, B] =
          Rel(ff.index |+| fa.index, ff.effect <*> fa.effect)

        override def pure[A](x: A): Rel[F, Representation, A] =
          Rel(Monoid[Representation].empty, Applicative[F].pure(x))
      }
  }

  sealed trait ResultLevel
  object ResultLevel {
    final case class Join(result: ResultLevel) extends ResultLevel
    final case class Select[A](a: A) extends ResultLevel
  }
  type QueryResult = Map[String, ResultLevel]

  abstract class RelFieldAttribute[F[_], G[_], Representation, A, B](
      val rel: Rel[G, Representation, A]
  ) extends FieldAttribute[F, QueryResult, B]

  // skunk
  sealed trait SkunkRepresentation
  object SkunkRepresentation {
    final case class Select(column: String) extends SkunkRepresentation
    // final case class Join(column: String, )
  }

  type SkunkRel[A] = Rel[Decoder, List[String], A]

  final case class SkunkFieldAttribute[F[_], A, B](override val rel: SkunkRel[A])
      extends RelFieldAttribute[F, Decoder, List[String], A, B](rel)

  def select[A](column: String, decoder: Decoder[A]): SkunkRel[A] =
    Rel(List(column), decoder)

  def resolveWith[F[_], A: ClassTag, B](fieldName: String, rel: SkunkRel[A])(
      f: Resolver[F, QueryResult, A] => Resolver[F, QueryResult, B]
  )(implicit out: => Out[F, B]): Field[F, QueryResult, B] =
    gql.dsl
      .build[F, QueryResult](res =>
        f(res.emap { m =>
          m.get(fieldName) match {
            case None                           => s"$fieldName was not present in result".leftIor
            case Some(ResultLevel.Join(_))      => s"join".leftIor
            case Some(ResultLevel.Select(a: A)) => a.rightIor
            case Some(ResultLevel.Select(_))    => "not of type A".leftIor
          }
        })
      )(out)
      .addAttributes(SkunkFieldAttribute(rel))

  def resolve[F[_], A: ClassTag](fieldName: String, rel: SkunkRel[A])(implicit out: => Out[F, A]) =
    resolveWith[F, A, A](fieldName, rel)(identity)(implicitly, out)

  resolve("stuff", select("name", skunk.codec.all.text))

  resolveWith[cats.effect.IO, (String, Int), String](
    "stuff2",
    (
      select("name", skunk.codec.all.text),
      select[Int]("age", skunk.codec.all.int4)
    ).tupled
  )(_.map { case (s, _) => s })
}

object Test3 {
  // lib
  final case class Rel[F[_], +Representation, A](index: Representation, effect: F[A])

  object Rel {
    implicit def applicativeForRel[F[_]: Applicative, Representation: Monoid]: Applicative[Rel[F, Representation, *]] =
      new Applicative[Rel[F, Representation, *]] {
        override def ap[A, B](ff: Rel[F, Representation, A => B])(fa: Rel[F, Representation, A]): Rel[F, Representation, B] =
          Rel(ff.index |+| fa.index, ff.effect <*> fa.effect)

        override def pure[A](x: A): Rel[F, Representation, A] =
          Rel(Monoid[Representation].empty, Applicative[F].pure(x))
      }
  }

  sealed trait RelFields[F[_], +Representation, A]
  final case class One[F[_], +Representation, A](
      rel: Rel[F, Representation, A]
  ) extends RelFields[F, Representation, Option[A]]
  final case class Cons[F[_], +Representation, A, B](
      one: One[F, Representation, A],
      cons: RelFields[F, Representation, B]
  ) extends RelFields[F, Representation, (A, B)]

  type MapResolver[F[_], A, B, C] = Resolver[F, A, B] => Resolver[F, A, C]

  final case class ResolverFun[F[_], G[_], Representation, A, B, C](
      construct: MapResolver[F, A, B, C],
      rel: Rel[G, Representation, B]
  )
  def resolve[F[_], G[_], Representation, A, B](rel: Rel[G, Representation, B]): ResolverFun[F, G, Representation, ?, B, B] =
    ResolverFun[F, G, Representation, Any, B, B](fa => fa, rel)

  def resolve[F[_], G[_], Representation, A, B, C](rel: Rel[G, Representation, B])(
      map: MapResolver[F, ?, B, C]
  ): ResolverFun[F, G, Representation, ?, B, C] =
    map match {
      case r: MapResolver[F, a, B, C] =>
        ResolverFun[F, G, Representation, a, B, C](r, rel)
    }

  /*

  relTpe(
    "User",
    "user",
    "name" -> select("name", text),
    "age" -> select("age", int4)
  )

   */

  // skunk
  import skunk.codec.all._
  import skunk._
  type SkunkRel[A] = Rel[Decoder, List[String], A]

  def sel[F[_], A](column: String, dec: Decoder[A]): ResolverFun[F, Decoder, List[String], ?, A, A] =
    resolve(Rel(List(column), dec))

  sealed trait LL[F[_], A]
  final case class One2[F[_], A](fun: ResolverFun[F, Decoder, List[String], ?, A, ?]) extends LL[F, Option[A]]
  final case class Cons2[F[_], A, B](
      one: One2[F, A],
      tail: LL[F, B]
  ) extends LL[F, (Option[A], B)]

  final case class IndexedRF[F[_], A, B](
      rf: ResolverFun[F, Decoder, List[String], ?, B, ?],
      index: A => B
  )

  def fields2[F[_]](name: String, table: String, fields: (String, ResolverFun[F, Decoder, List[String], ?, ?, ?])*) = {
    def make(xs: NonEmptyList[ResolverFun[F, Decoder, List[String], ?, ?, ?]]): LL[F, ?] = xs match {
      case NonEmptyList(head, xs) =>
        val here = One2(head)
        xs.toNel match {
          case None     => here
          case Some(xs) => Cons2(here, make(xs))
        }
    }

    val ll = make(fields.toList.toNel.get.map(_._2))
    ll match {
      case ll: LL[F, a] =>
        def reassocLL[A](ll: LL[F, A], accum: a => A): List[IndexedRF[F, a, ?]] = ll match {
          case o: One2[F, b] => List(IndexedRF[F, a, A](null, a => accum(a)))
          case cons: Cons2[F, a2, b] =>
            reassocLL[Option[a2]](
              cons.one,
              a =>
                accum(a) match {
                  case (a2, _) => a2
                }
            ) ++ reassocLL[b](
              cons.tail,
              a =>
                accum(a) match {
                  case (_, b: b) => b
                }
            )
        }

        val reassoced = reassocLL(ll, identity)
        val hd = reassoced.head
        ???
      // hd.rf.construct(Resolver.lift[F, a](hd.index))
    }
    ???
  }

  sel("name", text)
  sel("age", int4)

  // def select[A](column: String, dec: Decoder[A]): SkunkRel[A] =
  //   Rel(List(column), dec)

  // (select("name", text), select("age", int4)).tupled
}

object Test2 {
  trait Rel[F[_], Representation, A] {
    def index: Representation

    def effect: F[A]
  }

  trait SkunkRel[A] extends Rel[Decoder, List[String], A]

  implicit val applicativeForSkunkRel: Applicative[SkunkRel] = new Applicative[SkunkRel] {
    override def ap[A, B](ff: SkunkRel[A => B])(fa: SkunkRel[A]): SkunkRel[B] = new SkunkRel[B] {
      def index = ff.index ++ fa.index
      def effect = ff.effect <*> fa.effect
    }

    override def pure[A](x: A): SkunkRel[A] = new SkunkRel[A] {
      def index: List[String] = Nil
      def effect: Decoder[A] = Applicative[Decoder].pure(x)
    }
  }

  // def select[A](column: String, dec: Decoder[A]): SkunkRel[A] = new SkunkRel[A] {
  //   def index = List(column)
  //   def alg = dec
  // }

  // import skunk.codec.all._
  // val o: SkunkRel[(Int, String)] = (select("id", int4), select("name", text)).tupled

  sealed trait Realized[A] {
    def fieldNames: List[String]
    def columns: List[String]
  }
  final case class Leaf[A](fieldName: String, rel: SkunkRel[A]) extends Realized[A] {
    def fieldNames: List[String] = List(fieldName)
    def columns: List[String] = rel.index
  }
  final case class Branch[A, B](fa: Realized[A], fb: Realized[B]) extends Realized[(A, B)] {
    def fieldNames: List[String] = fa.fieldNames ++ fb.fieldNames
    def columns: List[String] = fa.columns ++ fb.columns
  }

  // def collapse(real: (String, SkunkRel[?])*): Realized[?] =
  //   real.toList
  //     .map { case (fn, rel) => Leaf(fn, rel) }
  //     .reduceLeft[Realized[?]] { case (r1: Realized[a], r2: Realized[b]) => Branch(r1, r2) }
}

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

  def join[F[_]](
      table: String,
      originColumn: String,
      targetColumn: String
  )(impl: => Out[F, Joins]): Field[F, Joins, Joins] =
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
      // val m = queries.groupMap(_.joins)(_.projection).toList
      def runRound(queries: List[Query[Decoder, A]]) = {
        val (here, deeper) = queries.partitionMap(q => q.joins.uncons.toRight(q.projection))
      }
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
    "users" -> join("user", "id", "contract_id")(user),
    "manyJoinsOnSameType" -> build.from {
      select("myValue", text).resolve.contramap[Joins] { xs =>
        xs ++
          startJoin("table1", "id", "contract_id") ++
          startJoin("table2", "table1_id", "parent_id")
      }
    }
  )
}
