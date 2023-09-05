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
import cats.effect.std.AtomicCell
import cats.effect.std.Supervisor
import cats.effect.std.Hotswap
import cats.effect.std.Mutex

object SkunkSchema extends QueryAlgebra with QueryDsl {
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

  trait SkunkTable[A] extends Table[A] {
    def aliased(x: Fragment[Void]): Fragment[Void] =
      sql"#${alias}.${x}"

    def sel[A](x: String, d: Decoder[A]): (Fragment[Void], Query.Select[A]) = {
      val col = aliased(sql"#${x}")
      col -> Query.Select(Chain(col.apply(Void)), d)
    }
  }

  type LazyConnection[F[_]] = LazyResource[F, Session[F]]
  def lazyPool[F[_]: Concurrent](pool: Resource[F, Session[F]]): Resource[F, LazyConnection[F]] =
    LazyResource.fromResource(pool)
}

class MySchema(pool: Resource[IO, Session[IO]]) {
  import SkunkSchema._
  import MySchema._


  implicit lazy val pet: Type[IO, QueryResult[PetTable]] = tpe[IO, QueryResult[PetTable]](
    "Pet",
    "name" -> query(_.selName),
    "id" -> query(_.selId)
  )

  implicit lazy val entity2: Type[IO, QueryResult[EntityTable2]] = relBuilder[IO, EntityTable2]{ b =>
    b.tpe(
      "Entity",
      "name" -> b.query(_.selName),
      "id" -> b.query(_.selId),
      "age" -> b.query(_.selAge),
      "height" -> b.query(_.selHeight),
      "pets" -> b.cont { e =>
        for {
          pe <- petEntityTable.join[List](pe => sql"${pe.entityId} = ${e.id}".apply(Void))
          p <- petTable.join(p => sql"${p.id} = ${pe.petId}".apply(Void))
        } yield p
      },
      "pets" -> b.contBoundary(pool)(_.selId) { is => 
        for {
          pe <- petEntityTable.join[List](pe => sql"${pe.entityId} in ${uuid.list(is.size).values}".apply(is.toList))
          p <- petTable.join(p => sql"${p.id} = ${pe.petId}".apply(Void))
        } yield (pe.selEntityId, p)
      }
    )
  }

  implicit lazy val entity: Type[IO, QueryResult[EntityTable]] = tpe[IO, QueryResult[EntityTable]](
    "Entity",
    "name" -> query(_.selName),
    "id" -> query(_.selId),
    "age" -> query(_.selAge),
    "height" -> query(_.selHeight)
  )

  implicit lazy val contract: Type[IO, QueryResult[ContractTable]] = tpe[IO, QueryResult[ContractTable]](
    "Contract",
    "name" -> query(c => (c.selName, c.selId).mapN(_ + _.toString())),
    "id" -> query(_.selId),
    "fastEntities" -> cont(arg[Option[List[String]]]("entityNames")) { (c, ens) =>
      entityTable2.join[List] { e =>
        val _ = ens.foldMap(xs => sql" and ${e.name} in (${text.list(xs)})".apply(xs))
        val extra = void""
        sql"${c.id} = ${e.contractId}${extra.fragment}".apply(extra.argument)
      }
    }
  )

}

object MySchema {
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

  case class PetTable(alias: String) extends SkunkTable[UUID] {
    def table = void"pet"
    def groupingKey = void"id"
    def groupingKeyDecoder: Decoder[UUID] = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
  }
  val petTable = table(PetTable)

  case class PetEntityTable(alias: String) extends SkunkTable[UUID] {
    def table = void"pet_entity"
    def groupingKey = void"pet_id"
    def groupingKeyDecoder: Decoder[UUID] = uuid

    val (petId, selPetId) = sel("pet_id", uuid)
    val (entityId, selEntityId) = sel("entity_id", uuid)
  }
  val petEntityTable = table(PetEntityTable)
}