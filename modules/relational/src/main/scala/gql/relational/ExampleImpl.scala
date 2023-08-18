package gql.relational

import cats.effect._
import skunk.implicits._
import gql.ast._
import gql.dsl._
import cats.implicits._
import skunk._
import cats._
import java.util.UUID
import gql.Arg
import gql.EmptyableArg
import skunk.codec.all._
import cats.data._
import gql.resolver.Resolver

object SkunkSchema extends QueryAlgebra with QueryDsl {
  type Frag = skunk.AppliedFragment

  def stringToFrag(s: String): Frag = sql"#${s}".apply(Void)
  implicit def appliedFragmentMonoid: Monoid[Frag] = skunk.AppliedFragment.MonoidAppFragment

  override implicit def applicativeForDecoder: Applicative[Decoder] =
    Decoder.ApplicativeDecoder

  type Encoder[A] = skunk.Encoder[A]
  type Decoder[A] = skunk.Decoder[A]
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] = d.opt

  def runQuery[F[_]: MonadCancelThrow, G[_], I, B, ArgType](pool: Resource[F, Session[F]], toplevelArg: EmptyableArg[ArgType], q: (I, ArgType) => Query[G, B]) = {
    resolveQuery[F, G, I, B, ArgType](toplevelArg, q).evalMap { case (qc, d: Interpreter.Done[G, a, QueryResult[B]]) =>
      val af = Interpreter.renderQuery(qc)
      println(af.fragment.sql)
      val out: F[List[a]] = pool.use(_.execute(af.fragment.query(d.dec))(af.argument))

      out.map{ x => println(x.size);x}.map(xs => d.reassoc(xs).toIor)
    }.rethrow
  }

  def runField[F[_]: MonadCancelThrow, G[_], I, B, ArgType](pool: Resource[F, Session[F]], arg: Arg[ArgType])(q: (I, ArgType) => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ) = Field(runQuery(pool, EmptyableArg.Lift(arg), q), Eval.later(tpe))

  def runField[F[_]: MonadCancelThrow, G[_], I, B](pool: Resource[F, Session[F]])(q: I => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ) = Field(runQuery[F, G, I, B, Unit](pool, EmptyableArg.Empty, (i, _) => q(i)), Eval.later(tpe))

  trait SkunkTable[A] extends Table[A] {
    def aliased(x: Fragment[Void]): Fragment[Void] =
      sql"#${alias}.${x}"

    def sel[A](x: String, d: Decoder[A]): (Fragment[Void], Query.Select[A]) = {
      val col = aliased(sql"#${x}")
      col -> Query.Select(NonEmptyChain.one(col.apply(Void)), d)
    }
  }
}

class MySchema(pool: Resource[IO, Session[IO]]) {
  import SkunkSchema._

  case class EntityTable(alias: String) extends SkunkTable[UUID] {
    def table = void"entity"
    def groupingKey = void"id"
    def groupingKeyDecoder: Decoder[UUID] = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
    val (age, selAge) = sel("age", int4)
    val (height, selHeight) = sel("height", int4)
  }
  val entityTable = table(EntityTable)

  case class ContractTable(alias: String) extends SkunkTable[UUID] {
    def table = void"contract"
    def groupingKey = void"id"
    def groupingKeyDecoder: Decoder[UUID] = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
  }
  val contractTable = table(ContractTable)

  case class ContractEntityTable(alias: String) extends SkunkTable[UUID] {
    def table = void"contract_entity"
    def groupingKey = void"contract_id"
    def groupingKeyDecoder: Decoder[UUID] = uuid

    val (contractId, selContractId) = sel("contract_id", uuid)
    val (entityId, selEntityId) = sel("entity_id", uuid)
  }
  val contractEntityTable = table(ContractEntityTable)

  case class EntityTable2(alias: String) extends SkunkTable[UUID] {
    def table = void"(contract_entity join entity on contract_entity.entity_id = entity.id)"
    def groupingKey = void"entity_id"
    def groupingKeyDecoder: Decoder[UUID] = uuid

    val (id, selId) = sel("id", uuid)
    val (contractId, selContractId) = sel("contract_id", uuid)
    val (name, selName) = sel("name", text)
    val (age, selAge) = sel("age", int4)
    val (height, selHeight) = sel("height", int4)
  }
  val entityTable2 = table(EntityTable2)

  implicit lazy val entity2: Type[IO, QueryResult[EntityTable2]] = tpe[IO, QueryResult[EntityTable2]](
    "Entity",
    "name" -> query(_.selName),
    "id" -> query(_.selId),
    "age" -> query(_.selAge),
    "height" -> query(_.selHeight)
  )

  implicit lazy val entity: Type[IO, QueryResult[EntityTable]] = tpe[IO, QueryResult[EntityTable]](
    "Entity",
    "name" -> query(_.selName),
    "id" -> query(_.selId),
    "age" -> query(_.selAge),
    "height" -> query(_.selHeight)
  )

  // List[ResultSet] => G[A]
  // 1:1 List[ResultSet] => (PrimaryKey, List[ResultSet])
  // xs => xs.groupBy(_.id).require1Element: Either[String, (PrimaryKey, List[ResultSet])]
  // 1:n List[ResultSet] => Map[PrimaryKey, List[ResultSet]]
  // xs => xs.groupBy(_.id): Map[PrimaryKey, List[ResultSet]]
  // 1:{0,1} List[ResultSet] => Option[(PrimaryKey, List[ResultSet])]
  // xs => xs.groupBy(_.id).requireAtMost1Element: Either[String, Option[(PrimaryKey, List[ResultSet])]]
  /*

  val xs: List[(ContractName, ContractId, ContractEntityContractId, ContractEntityEntityId, EntityName, EntityId, EntityAge, EntityHeight)]
  val ys = xs.groupBy(x => x.contractId): Map[ContractId, List[(ContractEntityContractId, ContractEntityEntityId, EntityName, EntityId, EntityAge, EntityHeight)]]
  ...: Map[ContractId, Map[ContractEntityContractId, List[(EntityName, EntityId, EntityAge, EntityHeight)]]]
  ...: Map[ContractId, Map[ContractEntityContractId, Map[EntityId, List[(EntityName, EntityAge, EntityHeight)]]]]

   */
  implicit lazy val contract: Type[IO, QueryResult[ContractTable]] = tpe[IO, QueryResult[ContractTable]](
    "Contract",
    "name" -> query(c => (c.selName, c.selId).mapN(_ + _.toString())),
    "id" -> query(_.selId),
    "fastEntities" -> queryAndThen[IO, Lambda[X => X], ContractTable, UUID, List[QueryResult[EntityTable2]]]{ c =>
      //contractEntityTable.join[List](cet => sql"${c.id} = ${cet.contractId}".apply(Void)).map(_.selEntityId)
      c.selId
    } { (xs: Resolver[IO, UUID, UUID]) =>
      val res = resolveQuery[IO, List, UUID, EntityTable2, Unit](EmptyableArg.Empty, { (i, _) =>
        entityTable2.join[List](e => sql"${e.contractId} = ${uuid}".apply(i))
      })
      type K = (UUID, Interpreter.QueryContent, Interpreter.Done[List, _, QueryResult[EntityTable2]])
      type V = List[QueryResult[EntityTable2]]
      val ilb = Resolver.inlineBatch[IO, (UUID, Interpreter.QueryContent, Interpreter.Done[List, _, QueryResult[EntityTable2]]), V]{ (ys: Set[K]) =>
        ys.toList.headOption.traverse{ case (_, qc, d: Interpreter.Done[List, a, QueryResult[EntityTable2]]) =>
          val af = Interpreter.renderQuery(qc.copy(selections = Chain(void"t1.contract_id") ++ qc.selections))

          val done2 = Interpreter.Done[Lambda[X => Map[UUID, List[X]]], (UUID, a),  QueryResult[EntityTable2]](
            uuid ~ d.dec,
            xs => 
              xs.groupMap{ case (k, _) => k }{ case (_, v) => v }.toList.traverse{ case (k, vs) =>
                d.reassoc(vs) tupleLeft k
              }.map(_.toMap)
          )

          println(af.fragment.sql)
          val out = pool.use(_.execute(af.fragment.query(done2.dec))(af.argument))

          out
            .map{ x => println(x.size);x}.map(xs => done2.reassoc(xs).toIor)
            .map{ x =>
              x.toEither.toOption.get.map{ case (k, v) => (k, qc, d) -> v }
            }
        }.map(_.get.asInstanceOf[Map[K, V]])
      }

      val prepped = res.tupleIn.map{ case ((qc, d), id) => 
        Set((id, qc, d).asInstanceOf[K]): Set[K]
      }

      val combined = prepped.andThen(ilb).map(_.map{ case ((k, _, _), vs) => k -> vs })
        .tupleIn.map{ case (outs, id) => println(outs); outs(id) }

      combined
    },/*
    "fastEntities" -> cont(
      arg[Option[List[String]]]("entityNames")
    ) { (c, ens) =>
      entityTable2.join[List] { e =>
        val _ = ens.foldMap(xs => sql" and ${e.name} in (${text.list(xs)})".apply(xs))
        val extra = void""
        sql"${c.id} = ${e.contractId}${extra.fragment}".apply(extra.argument)
      }
      // for {
      //   cet <- contractEntityTable.join[List](cet => sql"${cet.contractId} = ${c.id}")
      //   e <- entityTable.join[List] { e =>
      //     val _ = ens.foldMap(xs => sql" and ${e.name} in (${text.list(xs)})".apply(xs))
      //     val extra = void""
      //     sql"${e.id} = ${cet.entityId}${extra.fragment} and ${c.parent.parent.pk} is not null".apply(extra.argument)
      //   }
      // } yield e
    }*/
  )

}
