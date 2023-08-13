package gql.relational

import cats.effect._
import skunk.implicits._
import gql.ast._
import gql.dsl._
import cats.implicits._
import fs2.Pure
import skunk._
import cats._
import gql.resolver.Resolver
import cats.data._
import scala.reflect.ClassTag
import java.util.UUID
import gql.Arg
import gql.EmptyableArg
import gql.resolver.FieldMeta
import cats.mtl.Stateful
import cats.mtl.Tell
import gql.{preparation => prep}
import cats.mtl.Raise
import gql.Schema
import gql.SchemaShape
import gql.Application
import natchez.TraceValue
import natchez.Kernel
import natchez.Span
import java.net.URI
import cats.arrow.FunctionK
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


    case class Done[G[_], A, B](
        dec: Decoder[A],
        reassoc: List[A] => Either[String, G[B]]
    )
    def runFull[F[_]: Monad, G[_], I, B, ArgType](ses: Session[F], toplevelArg: EmptyableArg[ArgType], q: (I, ArgType) => Query[G, B])(
      implicit tpe: => Out[F, G[QueryResult[B]]]
  ) = {
    def addArg[I2] = toplevelArg match {
      case EmptyableArg.Empty   => Resolver.id[F, I2]
      case EmptyableArg.Lift(y) => Resolver.id[F, I2].arg(y).map { case (_, i2) => i2 }
    }

    val resolver = Resolver
      .meta[F, I]
      .andThen(addArg)
      .tupleIn
      .emap { case (fm, i) =>
        def fiendPField[A](p: prep.PreparedField[F, A]): List[prep.PreparedDataField[F, ?, ?]] = p match {
          case prep.PreparedSpecification(_, _, xs) => xs
          case pdf: prep.PreparedDataField[F, A, ?] => List(pdf)
        }

        def findSel[A](p: prep.Prepared[F, A]): Option[prep.Selection[F, ?]] = p match {
          case sel: prep.Selection[F, A]        => Some(sel)
          case prep.PreparedList(of, _)         => findSel(of.cont)
          case po: prep.PreparedOption[F, ?, ?] => findSel(po.of.cont)
          case prep.PreparedLeaf(_, _)          => None
        }

        def goTba[G[_], A, B, ArgType, Q, O](
            pdf: prep.PreparedDataField[F, ?, ?],
            a: A,
            tfa: TableFieldAttribute[F, G, A, B, ArgType, Q, O]
        ): Interpreter.Effect[Done[G, ?, B]] = {
          val o = tfa.arg match {
            case EmptyableArg.Empty   => Some(tfa.query(a, ()))
            case EmptyableArg.Lift(y) => pdf.arg(y).map(tfa.query(a, _))
          }
          import Interpreter._
          o match {
            case None => R.raise(s"could not find argument for ${pdf.outputName}")
            case Some(q) =>
              val outEffect: Effect[QueryState[G, ?, Q]] = go(q)
              outEffect.flatMap { case (qs: QueryState[G, k, Q]) =>
                tfa.fieldVariant match {
                  case _: FieldVariant.Selection[a] =>
                    // implicitly[Select[B] =:= Q]
                    val sel: Query.Select[B] = qs.value
                    addSelection(sel.col).as {
                      Done[G, (k, Option[B]), B](
                        qs.decoder ~ sel.decoder.opt,
                        { xs =>
                          val ys = xs.collect { case (k, Some(v)) => k -> v }
                          qs.reassoc(ys)
                            .flatMap { gs =>
                              qs.reassoc.traverse.traverse(gs) {
                                case x :: _ => Right(x)
                                case xs     => Left(s"Expected 1 element, but got ${xs.size}")
                              }
                            }
                            .map(qs.fk(_))
                        }
                      )
                    }
                  case _: FieldVariant.SubSelection[a] =>
                    // implicitly[QueryResult[Q] =:= B]
                    val passthrough: Q = qs.value
                    val attrs = getNextAttributes(pdf)
                    val ys = attrs.traverse { case attr: FieldWithAttr[F, g, aa] =>
                      val tbaRes = goTba(attr.field, passthrough.asInstanceOf[aa], attr.attr)

                      val out: Effect[(Done[g, ?, ?], FieldWithAttr[F, Any, ?])] = tbaRes.map(done => (done, attr))
                      out
                    }

                    ys.map { dones =>
                      type K = TableFieldAttribute[F, Any, ?, ?, ?, ?, ?]
                      val decs = dones
                        .flatTraverse { case (done, attr) =>
                          done.dec.map { x => List[(K, Any)](attr.attr -> x) }
                        }
                        .map(_.toMap)

                      val doneMap = dones.map { case (v, k) => k.attr -> v }.toMap

                      val reassocNext = { (xs: List[Map[K, Any]]) =>
                        val keys = xs.flatMap(_.keySet).toSet
                        val grouped = keys.toList.map(k => k -> xs.flatMap(_.get(k))).toMap
                        new QueryResult[Q] {
                          def read[F[_], G[_], A0, B, ArgType, Q](
                              tfa: TableFieldAttribute[F, G, A0, B, ArgType, Q, ?]
                          ): Option[Either[String, G[B]]] =
                            doneMap.asInstanceOf[Map[TableFieldAttribute[F, G, A0, B, ArgType, Q, ?], Done[G, ?, ?]]].get(tfa).flatMap {
                              case (done: Done[G, a, ?]) =>
                                grouped.asInstanceOf[Map[TableFieldAttribute[F, G, A0, B, ArgType, Q, ?], List[Any]]].get(tfa).map { ys =>
                                  done.reassoc(ys.asInstanceOf[List[a]]).map(_.asInstanceOf[G[B]])
                                }
                            }
                        }
                      }

                      Done(
                        qs.decoder ~ decs,
                        { (xs: List[(k, Map[K, Any])]) =>
                          qs.reassoc(xs)
                            .map(gs =>
                              qs.fk {
                                qs.reassoc.traverse.map(gs) { (xs: List[Map[K, Any]]) =>
                                  reassocNext(xs)
                                }
                              }
                            )
                        }
                      )
                    }
                }
              }
          }
        }

        case class FieldWithAttr[F[_], G[_], A](
            field: prep.PreparedDataField[F, QueryResult[A], ?],
            attr: TableFieldAttribute[F, G, A, ?, ?, ?, ?]
        )

        def getNextAttributes[A, B](pdf: prep.PreparedDataField[F, A, B]) = {
          val selFields: List[prep.PreparedField[F, ?]] = findSel(pdf.cont.cont).toList.flatMap(_.fields)
          selFields
            .flatMap(pf => fiendPField(pf))
            .collect { case x: prep.PreparedDataField[F, ?, ?] => x }
            .map { x =>
              x.source.attributes.collectFirst { case a: TableFieldAttribute[F, g, a, ?, ?, ?, ?] @unchecked => a }.map {
                case tfa: TableFieldAttribute[F, g, a, ?, ?, ?, ?] =>
                  FieldWithAttr(x.asInstanceOf[prep.PreparedDataField[F, QueryResult[a], ?]], tfa)
              }
            }
            .collect { case Some(x) => x }
        }

        val tfa = new TableFieldAttribute[F, G, I, QueryResult[B], ArgType, B, G[QueryResult[B]]] {
          def arg = toplevelArg
          def query(value: I, argument: ArgType): Query[G, B] = q(value, argument)
          def fieldVariant = FieldVariant.SubSelection()
        }

        val eff = goTba(fm.astNode, i, tfa)

        val (qc, e) = eff.value.runA(1).run

        e.toIor.tupleLeft(qc)
      }
      .evalMap { case (qc, d: Done[G, a, QueryResult[B]]) =>
        val selections = qc.selections.intercalate(void", ")
        val base = qc.joins.headOption.get
        val nl = sql"#${"\n"}"
        val tl = qc.joins.toList.tail.foldMap(qj =>
          sql"${nl}left join ${qj.tbl.fragment} on ${qj.pred.fragment}".apply(qj.tbl.argument, qj.pred.argument)
        )
        val fullQuery =
          sql"""
        select ${selections.fragment}
        from ${base.tbl.fragment}
        ${tl.fragment}
        where ${base.pred.fragment}
        """.query(d.dec)

        println(fullQuery.sql)

        val out: F[List[a]] = ses.execute(fullQuery)(selections.argument, base.tbl.argument, tl.argument, base.pred.argument)

        out.map { xs => d.reassoc(xs).toIor }
      }
      .rethrow

    Field[F, I, G[QueryResult[B]]](
      resolver,
      Eval.later(tpe)
    )
  }
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

