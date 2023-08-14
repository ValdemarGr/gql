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

object SkunkSchema extends QueryAlgebra with QueryDsl {
  type Empty = Void
  val Empty = Void
  override implicit def contravariantSemigroupalForFrag: ContravariantSemigroupal[Frag] =
    skunk.Fragment.FragmentContravariantSemigroupal

  type AppliedFragment = skunk.AppliedFragment
  def extractFrag(af: AppliedFragment): ExtractedFrag[?] =
    ExtractedFrag[af.A](af.fragment, af.argument)

  import skunk.implicits._
  override def stringToFrag(s: String): Frag[Void] = sql"#${s}"

  def liftFrag[A0](frag: Frag[A0], arg: A0): AppliedFragment = frag.apply(arg)

  override implicit def applicativeForDecoder: Applicative[Decoder] =
    Decoder.ApplicativeDecoder

  type Frag[A] = skunk.Fragment[A]

  type Encoder[A] = skunk.Encoder[A]
  type Decoder[A] = skunk.Decoder[A]
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] = d.opt

  def runQuery[F[_]: Monad, G[_], I, B, ArgType](ses: Session[F], toplevelArg: EmptyableArg[ArgType], q: (I, ArgType) => Query[G, B]) = {
    resolveQuery[F, G, I, B, ArgType](toplevelArg, q).evalMap { case (qc, d: Interpreter.Done[G, a, QueryResult[B]]) =>
      val af = Interpreter.renderQuery(qc)
      val out: F[List[a]] = ses.execute(af.fragment.query(d.dec))(af.argument)

      out.map(xs => d.reassoc(xs).toIor)
    }.rethrow
  }

  def runField[F[_]: Monad, G[_], I, B, ArgType](ses: Session[F], arg: Arg[ArgType])(q: (I, ArgType) => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ) = Field(runQuery(ses, EmptyableArg.Lift(arg), q), Eval.later(tpe))

  def runField[F[_]: Monad, G[_], I, B](ses: Session[F])(q: I => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ) = Field(runQuery[F, G, I, B, Unit](ses, EmptyableArg.Empty, (i, _) => q(i)), Eval.later(tpe))
}

object MySchema {
  import SkunkSchema._

  case class EntityTable(alias: Frag[Void]) extends Table[UUID] {
    def table = void"entity"
    def pk = void"id"
    def pkDecoder: Decoder[UUID] = uuid
    def pkEncoder: Encoder[UUID] = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
    val (age, selAge) = sel("age", int4)
    val (height, selHeight) = sel("height", int4)
  }
  val entityTable = table(EntityTable)

  case class ContractTable(alias: Frag[Void]) extends Table[UUID] {
    def table = void"contract"
    def pk = void"id"
    def pkDecoder: Decoder[UUID] = uuid
    def pkEncoder: Encoder[UUID] = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
  }
  val contractTable = table(ContractTable)

  case class ContractEntityTable(alias: Frag[Void]) extends Table[UUID] {
    def table = void"contract_entity"
    def pk = void"entity_id"
    def pkDecoder: Decoder[UUID] = uuid
    def pkEncoder: Encoder[UUID] = uuid

    val (contractId, selContractId) = sel("contract_id", uuid)
    val (entityId, selEntityId) = sel("entity_id", uuid)
  }
  val contractEntityTable = table(ContractEntityTable)

  implicit lazy val entity: Type[IO, QueryResult[EntityTable]] = tpe[IO, QueryResult[EntityTable]](
    "Entity",
    "name" -> query(_.selName),
    "id" -> query(_.selId),
    "age" -> query(_.selAge),
    "height" -> query(_.selHeight)
  )

  implicit lazy val contract: Type[IO, QueryResult[ContractTable]] = tpe[IO, QueryResult[ContractTable]](
    "Contract",
    "name" -> query(_.selName),
    "id" -> query(_.selId),
    "entities" -> cont[IO, Lambda[X => List[Option[X]]], ContractTable, EntityTable, Option[List[String]]](
      arg[Option[List[String]]]("entityNames")
    ) { (c, ens) =>
      for {
        cet <- contractEntityTable.join[List](cet => sql"${cet.contractId} = ${c.id}")
        e <- entityTable.join[Option] { e =>
          val extra = ens.foldMap(xs => sql" and ${e.name} in (${text.list(xs)})".apply(xs))
          sql"${e.id} = ${cet.entityId}${extra.fragment}".apply(extra.argument)
        }
      } yield e
    }
  )

}
