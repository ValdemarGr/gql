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

object Main extends IOApp.Simple {
  override def run: IO[Unit] =
    Test7.testMe
}

object Test7 {
  sealed trait FieldVariant[Q, A]
  object FieldVariant {
    case class Selection[A]() extends FieldVariant[Select[A], A]
    case class SubSelection[A]() extends FieldVariant[A, QueryResult[A]]
  }

  trait TableFieldAttribute[F[_], G[_], A, B, ArgType, Q, O] extends FieldAttribute[F, QueryResult[A], O] {
    def arg: EmptyableArg[ArgType]
    def query(value: A, argument: ArgType): Query[G, Q]
    def fieldVariant: FieldVariant[Q, B]
  }

  trait QueryResult[A] {
    def read[F[_], G[_], A0, B, ArgType, Q](tfa: TableFieldAttribute[F, G, A0, B, ArgType, Q, ?]): Option[Either[String, G[B]]]
  }

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

  trait TableAlg[T <: Table[?]] {
    def make: Fragment[Void] => T

    def join[G[_]: JoinType](joinPred: T => AppliedFragment): Join[G, T] =
      Join(make, joinPred, implicitly[JoinType[G]])

    def join[G[_]: JoinType](joinPred: T => Fragment[Void])(implicit dummy: DummyImplicit): Join[G, T] =
      Join(make, joinPred.andThen(_.apply(Void)), implicitly[JoinType[G]])
  }
  def table[T <: Table[?]](f: Fragment[Void] => T): TableAlg[T] = new TableAlg[T] {
    def make: Fragment[Void] => T = f
  }

  abstract class TableRef[A](val td: TableDef[A]) extends Table[A] {
    def table = td.table
    def pk = td.pk
    def pkCodec = td.pkCodec
  }

  trait Reassoc[G[_], Key] {
    def traverse: Traverse[G]

    def apply[A](fa: List[(Key, A)]): Either[String, G[List[A]]]
  }
  trait ReassocGroup[G[_], Key] extends Reassoc[G, Key] {
    def groups[A](fa: List[List[A]]): Either[String, G[List[A]]]

    def apply[A](fa: List[(Key, A)]): Either[String, G[List[A]]] = {
      val m = fa.groupMap { case (k, _) => k } { case (_, v) => v }
      groups(fa.map { case (k, _) => k }.distinct.map(k => m(k)))
    }
  }
  case class ReassocOpt[G[_], Key](reassoc: Reassoc[G, Key]) extends Reassoc[G, Option[Key]] {
    def traverse = reassoc.traverse

    def apply[A](fa: List[(Option[Key], A)]): Either[String, G[List[A]]] =
      reassoc(fa.collect { case (Some(k), v) => (k, v) })
  }

  sealed trait JoinType[G[_]] {
    def reassoc[Key]: Reassoc[G, Key]
  }
  object JoinType extends LowPrioJoinTypeImplicits1 {
    // A => A instead of Id since scala doesn't want reduce Id to A => A, and Id is noisy
    case object One extends JoinType[Lambda[A => A]] {
      def reassoc[Key]: Reassoc[Lambda[A => A], Key] = new ReassocGroup[Lambda[X => X], Key] {
        def traverse = Traverse[Lambda[X => X]]
        override def groups[A](fa: List[List[A]]): Either[String, List[A]] = {
          fa match {
            case x :: Nil => Right(x)
            case _        => Left(s"Expected 1 element, but found ${fa.size}")
          }
        }
      }
    }
    case object Opt extends JoinType[Option] {
      def reassoc[Key]: Reassoc[Option, Key] = new ReassocGroup[Option, Key] {
        def traverse = Traverse[Option]
        override def groups[A](fa: List[List[A]]): Either[String, Option[List[A]]] = {
          fa match {
            case x :: Nil => Right(Some(x))
            case Nil      => Right(None)
            case _        => Left("Expected 0 or 1 element, but found more")
          }
        }
      }
    }

    case class Many[G[_]](
        fromListFK: List ~> Lambda[X => Either[String, G[X]]],
        traverse: Traverse[G]
    ) extends JoinType[G] {
      def traverse0 = traverse
      def reassoc[Key]: Reassoc[G, Key] = new ReassocGroup[G, Key] {
        def traverse = traverse0
        def groups[A](fa: List[List[A]]): Either[String, G[List[A]]] =
          fromListFK(fa)
      }
    }

    implicit val joinTypeOne: JoinType[Lambda[A => A]] = JoinType.One
  }
  trait LowPrioJoinTypeImplicits1 extends LowPrioJoinTypeImplicits2 {
    implicit val joinTypeOpt: JoinType[Option] = JoinType.Opt
  }
  trait LowPrioJoinTypeImplicits2 {
    def make[G[_]](fromList: List ~> Lambda[X => Either[String, G[X]]])(implicit G: Traverse[G]): JoinType[G] =
      JoinType.Many(fromList, G)
    implicit val joinTypeList: JoinType[List] = make {
      new (List ~> Lambda[X => Either[String, List[X]]]) {
        override def apply[A](fa: List[A]): Either[String, List[A]] = Right(fa)
      }
    }
  }

  sealed trait Query[G[_], A] {
    def flatMap[H[_], B](f: A => Query[H, B]): Query[Lambda[X => G[H[X]]], B] =
      FlatMap(this, f)

    def map[B](f: A => B): Query[G, B] = flatMap[Lambda[X => X], B](a => Pure(f(a)))

    def mapK[H[_]](fk: G ~> H): Query[H, A] = MapK(this, fk)
  }
  case class Join[G[_], T <: Table[?]](
      tbl: Fragment[Void] => T,
      joinPred: T => AppliedFragment,
      jt: JoinType[G]
  ) extends Query[G, T]
  case class Pure[A](a: A) extends Query[Lambda[X => X], A]
  case class FlatMap[G[_], H[_], A, B](
      fa: Query[G, A],
      f: A => Query[H, B]
  ) extends Query[Lambda[X => G[H[X]]], B]
  case class MapK[G[_], H[_], A](
      fa: Query[G, A],
      f: G ~> H
  ) extends Query[H, A]

  case class Select[A](col: AppliedFragment, decoder: Decoder[A]) extends Query[Lambda[X => X], Select[A]]
  implicit val applyForSelect: Apply[Select] = new Apply[Select] {
    override def map[A, B](fa: Select[A])(f: A => B): Select[B] =
      fa.copy(decoder = fa.decoder.map(f))

    override def ap[A, B](ff: Select[A => B])(fa: Select[A]): Select[B] =
      Select(sql"${ff.col.fragment}, ${fa.col.fragment}".apply(ff.col.argument, fa.col.argument), ff.decoder.ap(fa.decoder))
  }

  def query[F[_], G[_], A, B](f: A => Query[G, Select[B]])(implicit
      tpe: => Out[F, G[B]]
  ): Field[F, QueryResult[A], G[B]] =
    queryFull(EmptyableArg.Empty)((a, _) => f(a), Resolver.id[F, G[B]])(tpe)

  def query[F[_], G[_], A, B, C](a: Arg[C])(f: (A, C) => Query[G, Select[B]])(implicit
      tpe: => Out[F, G[B]]
  ): Field[F, QueryResult[A], G[B]] =
    queryFull(EmptyableArg.Lift(a))((a, c) => f(a, c), Resolver.id[F, G[B]])(tpe)

  def queryAndThen[F[_], G[_], A, B, D](f: A => Query[G, Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], D])(implicit
      tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] =
    queryFull(EmptyableArg.Empty)((a, _) => f(a), g(Resolver.id[F, G[B]]))(tpe)

  def queryAndThen[F[_], G[_], A, B, C, D](a: Arg[C])(f: (A, C) => Query[G, Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], D])(
      implicit tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] =
    queryFull(EmptyableArg.Lift(a))((a, c) => f(a, c), g(Resolver.id[F, G[B]]))(tpe)

  def cont[F[_], G[_], A, B](f: A => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ): Field[F, QueryResult[A], G[QueryResult[B]]] =
    contFull(EmptyableArg.Empty)((a, _) => f(a))(tpe)

  def cont[F[_], G[_], A, B, C](a: Arg[C])(f: (A, C) => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ): Field[F, QueryResult[A], G[QueryResult[B]]] =
    contFull(EmptyableArg.Lift(a))((a, c) => f(a, c))(tpe)

  def queryFull[F[_], G[_], A, B, C, D](a: EmptyableArg[C])(f: (A, C) => Query[G, Select[B]], resolverCont: Resolver[F, G[B], D])(implicit
      tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] = {
    val tfa: TableFieldAttribute[F, G, A, B, C, Select[B], D] = new TableFieldAttribute[F, G, A, B, C, Select[B], D] {
      def arg = a
      def query(value: A, argument: C): Query[G, Select[B]] = f(value, argument)
      def fieldVariant = FieldVariant.Selection()
    }

    build
      .from(
        Resolver
          .id[F, QueryResult[A]]
          .emap { qa =>
            qa.read(tfa).toRight("internal query association error, could not read result from query result").flatten.toIor
          }
          .andThen(resolverCont)
      )(tpe)
      .addAttributes(tfa)
  }

  def contFull[F[_], G[_], A, B, C](a: EmptyableArg[C])(f: (A, C) => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ): Field[F, QueryResult[A], G[QueryResult[B]]] = {
    def addArg[I2] = a match {
      case EmptyableArg.Empty   => Resolver.id[F, I2]
      case EmptyableArg.Lift(y) => Resolver.id[F, I2].arg(y).map { case (_, i2) => i2 }
    }

    val tfa: TableFieldAttribute[F, G, A, QueryResult[B], C, B, G[QueryResult[B]]] =
      new TableFieldAttribute[F, G, A, QueryResult[B], C, B, G[QueryResult[B]]] {
        def arg = a
        def query(value: A, argument: C): Query[G, B] = f(value, argument)
        def fieldVariant = FieldVariant.SubSelection()
      }

    build
      .from(Resolver.id[F, QueryResult[A]].andThen(addArg).emap { qa =>
        qa.read(tfa).toRight("internal query association error, could not read result from query result").flatten.toIor
      })(tpe)
      .addAttributes(tfa)
  }

  object Internal {
    case class QueryJoin(
        tbl: AppliedFragment,
        pred: AppliedFragment
    )
    case class QueryContent(
        selections: Chain[AppliedFragment],
        joins: Chain[QueryJoin]
    )
    implicit val monoidForQueryContent: Monoid[QueryContent] = new Monoid[QueryContent] {
      override def combine(x: QueryContent, y: QueryContent): QueryContent =
        QueryContent(x.selections ++ y.selections, x.joins ++ y.joins)

      override def empty: QueryContent = QueryContent(Chain.empty, Chain.empty)
    }

    type Effect[A] = EitherT[StateT[Writer[QueryContent, *], Int, *], String, A]
    val Effect = Monad[Effect]
    val S = Stateful[Effect, Int]
    val T = Tell[Effect, QueryContent]
    val R = Raise[Effect, String]
    val nextId = S.get.map(i => sql"t#${i.toString()}") <* S.modify(_ + 1)
    def addJoin(tbl: AppliedFragment, pred: AppliedFragment): Effect[Unit] = T.tell(QueryContent(Chain.empty, Chain(QueryJoin(tbl, pred))))
    def addSelection(f: AppliedFragment): Effect[Unit] = T.tell(QueryContent(Chain(f), Chain.empty))

    trait QueryState[G[_], Key, C] {
      type T[_]
      def reassoc: Reassoc[T, Key]
      def decoder: Decoder[Key]
      def value: C
      def fk: T ~> G
    }
    final case class QueryStateImpl[G[_], I[_], Key, C](
        reassoc: Reassoc[I, Key],
        decoder: Decoder[Key],
        value: C,
        fk: I ~> G
    ) extends QueryState[G, Key, C] {
      type T[A] = I[A]
    }

    def mergeFlatMap[G[_], H[_], A, B, AK, BK](
        qsa: QueryState[G, AK, A],
        qsb: QueryState[H, BK, B]
    ): QueryState[Lambda[X => G[H[X]]], (AK, BK), B] = {
      type N[A] = qsa.T[qsb.T[A]]
      val reassoc: Reassoc[N, (AK, BK)] = new Reassoc[N, (AK, BK)] {
        def nestedTraverse = Nested.catsDataTraverseForNested[qsa.T, qsb.T](qsa.reassoc.traverse, qsb.reassoc.traverse)
        def traverse = new Traverse[N] {
          override def foldLeft[A, B](fa: N[A], b: B)(f: (B, A) => B): B =
            nestedTraverse.foldLeft(Nested(fa), b)(f)
          override def foldRight[A, B](fa: N[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
            nestedTraverse.foldRight(Nested(fa), lb)(f)
          override def traverse[G[_]: Applicative, A, B](fa: N[A])(f: A => G[B]): G[N[B]] =
            nestedTraverse.traverse(Nested(fa))(f).map(_.value)
        }

        override def apply[A](fa: List[((AK, BK), A)]): Either[String, N[List[A]]] = {
          val ys = fa.map { case ((ak, bk), a) => (ak, (bk, a)) }
          qsa.reassoc(ys).flatMap { gs =>
            qsa.reassoc.traverse.traverse(gs) { bs =>
              qsb.reassoc(bs)
            }
          }
        }
      }

      val fk = new (Lambda[X => qsa.T[qsb.T[X]]] ~> Lambda[X => G[H[X]]]) {
        def apply[A](fa: qsa.T[qsb.T[A]]): G[H[A]] =
          qsa.fk(qsa.reassoc.traverse.map(fa)(t2 => qsb.fk(t2)))
      }

      QueryStateImpl(
        reassoc,
        qsa.decoder ~ qsb.decoder,
        qsb.value,
        fk
      )
    }

    def handleFlatMap[G[_], H[_], A, B](fm: FlatMap[G, H, A, B]): Effect[QueryState[Lambda[X => G[H[X]]], ?, B]] = {
      for {
        qsa <- go(fm.fa)
        qsb <- go(fm.f(qsa.value))
      } yield mergeFlatMap(qsa, qsb)
    }

    def go[G[_], C](q: Query[G, C]): Effect[QueryState[G, ?, C]] = q match {
      case p: Pure[a]              => Effect.pure(QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], p.a, FunctionK.id[G]))
      case s: Select[a]            => Effect.pure(QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], s, FunctionK.id[G]))
      case fm: FlatMap[g, h, a, b] => handleFlatMap(fm)
      case j: Join[g, t] =>
        for {
          n <- nextId
          t = j.tbl(n)
          jp = j.joinPred(t)
          tbl = t.table
          _ <- addJoin(sql"${tbl.fragment} as ${n}".apply(tbl.argument), jp)
          _ <- addSelection(t.selPk.col)
        } yield {
          t.pkCodec match {
            case d: Decoder[a] =>
              QueryStateImpl(
                ReassocOpt[G, a](j.jt.reassoc),
                d.opt,
                t.asInstanceOf[C],
                FunctionK.id[G]
              ) // TODO figure out why this is necessary
          }
        }
      case mapK: MapK[g, h, a] =>
        go(mapK.fa).map { case (qs: QueryState[g, k, a]) =>
          QueryStateImpl(
            qs.reassoc,
            qs.decoder,
            qs.value,
            qs.fk.andThen(mapK.f)
          )
        }
    }
  }

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
        ): Internal.Effect[Done[G, ?, B]] = {
          val o = tfa.arg match {
            case EmptyableArg.Empty   => Some(tfa.query(a, ()))
            case EmptyableArg.Lift(y) => pdf.arg(y).map(tfa.query(a, _))
          }
          import Internal._
          o match {
            case None => R.raise(s"could not find argument for ${pdf.outputName}")
            case Some(q) =>
              val outEffect: Effect[QueryState[G, ?, Q]] = go(q)
              outEffect.flatMap { case (qs: QueryState[G, k, Q]) =>
                tfa.fieldVariant match {
                  case _: FieldVariant.Selection[a] =>
                    // implicitly[Select[B] =:= Q]
                    val sel: Select[B] = qs.value
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

  import skunk.implicits._
  import skunk.codec.all._

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
    "entities" -> cont(arg[Option[List[String]]]("entityNames")) { (c, ens) =>
      val q = for {
        cet <- contractEntityTable.join[List](cet => sql"${cet.contractId} = ${c.id}")
        e <- entityTable.join[Option] { e =>
          val extra = ens.foldMap(xs => sql" and ${e.name} in (${text.list(xs)})".apply(xs))
          sql"${e.id} = ${cet.entityId}${extra.fragment}".apply(extra.argument)
        }
      } yield e

      q.mapK(FunctionK.liftFunction[Lambda[X => List[Option[X]]], List](_.collect { case Some(x) => x }))
    }
  )

  def testMe = {
    implicit val emptyIOTrace = new natchez.Trace[IO] {
      override def put(fields: (String, TraceValue)*): IO[Unit] = IO.unit
      override def log(fields: (String, TraceValue)*): IO[Unit] = IO.unit
      override def log(event: String): IO[Unit] = IO.unit
      override def attachError(err: Throwable, fields: (String, TraceValue)*): IO[Unit] = IO.unit
      override def kernel: IO[Kernel] = IO.pure(Kernel(Map.empty))
      override def spanR(name: String, options: Span.Options): Resource[IO, IO ~> IO] = Resource.pure(FunctionK.id[IO])
      override def span[A](name: String, options: Span.Options)(k: IO[A]): IO[A] = k
      override def traceId: IO[Option[String]] = IO.pure(None)
      override def traceUri: IO[Option[URI]] = IO.pure(None)
    }
    Session
      .single[IO](
        host = "127.0.0.1",
        user = "postgres",
        database = "postgres",
        password = "1234".some
      )
      .use { ses =>
        ses.transaction.surround {
          import gql.relational.MySchema
          import gql.relational.SkunkSchema
          val ss = SchemaShape.unit[IO](
            fields[IO, Unit](
              "name" -> lift(_ => "edlav"),
              "contract" -> SkunkSchema.runField(ses, arg[UUID]("contractId"))(
                (_: Unit, a: UUID) => MySchema.contractTable.join[Option](c => sql"${c.id} = ${uuid}".apply(a))
              )
            )
          )

          Schema.simple(ss).flatMap { schema =>
            gql
              .Compiler[IO]
              .compile(
                schema,
                """
        query {
          contract(contractId: "1ff0ca77-c13f-4af8-9166-72373f309247") {
            name
            id
            entities(entityNames: ["John"]) {
              name
              age
            }
          }
        }
        """
              ) match {
              case Right(Application.Query(run)) => run.flatMap(IO.println)
              case x                             => IO.println(x)
            }
          }
        }
      }
  }

  case class EntityTable(alias: Fragment[Void]) extends Table[UUID] {
    def table = void"entity"
    def pk = void"id"
    def pkCodec = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
    val (age, selAge) = sel("age", int4)
    val (height, selHeight) = sel("height", int4)
  }
  val entityTable = table(EntityTable)

  case class ContractTable(alias: Fragment[Void]) extends Table[UUID] {
    def table = void"contract"
    def pk = void"id"
    def pkCodec = uuid

    val (id, selId) = sel("id", uuid)
    val (name, selName) = sel("name", text)
  }
  val contractTable = table(ContractTable)

  case class ContractEntityTable(alias: Fragment[Void]) extends Table[UUID] {
    def table = void"contract_entity"
    def pk = void"entity_id"
    def pkCodec = uuid

    val (contractId, selContractId) = sel("contract_id", uuid)
    val (entityId, selEntityId) = sel("entity_id", uuid)
  }
  val contractEntityTable = table(ContractEntityTable)
}

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

  object Oi {
    sealed trait Query2[F[_], A]
    case object End extends Query2[Lambda[X => X], Unit]
    final case class J[F[_], G[_], A](jt: JoinType[F], j: Query2[G, A]) extends Query2[Lambda[X => F[G[X]]], String]
    def unify[F[_], A](q: Query2[F, A]) = q
    val o = unify {
      J(
        JoinType.Traversable[List](implicitly),
        J(
          JoinType.Traversable[List](implicitly),
          J(
            JoinType.Opt,
            End
          )
        )
      )
    }
  }

  object Hmmm {
    sealed trait Query2[F[_], A]
    case class Join2[F[_], G[_], A, T <: Table[?]](
        tbl: Fragment[Void] => T,
        joinPred: T => AppliedFragment,
        jt: JoinType[G],
        f: T => Either[Query2[F, A], F[A]]
    ) extends Query2[F, G[A]] {
      def flatMap[H[_], B](f: T => Query2[H, B]): Query2[H, G[B]] =
        Join2(tbl, joinPred, jt, (t: T) => Left(f(t)))

      def map[H[_], B](f: T => H[B])(implicit dummy: DummyImplicit): Query2[H, G[B]] =
        Join2(tbl, joinPred, jt, (t: T) => Right(f(t)))
    }

    sealed trait Cont[A]
    protected case class ContImpl[A](a: A) extends Cont[QueryResult[A]]

    case class Select[A](col: AppliedFragment, decoder: Decoder[A])
    implicit val applyForSelect: Apply[Select] = ???

    def cont[A](a: A): Cont[QueryResult[A]] = ContImpl(a)

    def join[G[_]: JoinType, T <: Table[?]](
        f: Fragment[Void] => T
    )(pred: T => AppliedFragment) =
      Join2[Cont, G, QueryResult[T], T](f, pred, implicitly[JoinType[G]], (t: T) => ContImpl(t).asRight)

    def query[F[_], A, B](f: A => Cont[B])(implicit tpe: => Out[F, B]): Field[F, QueryResult[A], B] =
      ???

    def query[F[_], A, B](
        f: A => Query2[Cont, B]
    )(implicit tpe: => Out[F, B], dummy: DummyImplicit, dummy1: DummyImplicit, dummy2: DummyImplicit): Field[F, QueryResult[A], B] =
      ???

    def query[F[_], A, B](f: A => Query2[Select, B])(implicit tpe: => Out[F, B], dummy: DummyImplicit): Field[F, QueryResult[A], B] =
      ???

    def query[F[_], A, B](
        f: A => Select[B]
    )(implicit tpe: => Out[F, B], dummy: DummyImplicit, dummy1: DummyImplicit): Field[F, QueryResult[A], B] =
      ???

    // implicit def et[F[_]]: Out[F, QueryResult[(EntityTable, String)]] = ???

    // val entitiesField = query { (a: String) =>
    //   for {
    //     c <- join(ContractTable(_))(c => void"")
    //     e <- join[Option, EntityTable](EntityTable(_))(e => void"")
    //   } yield cont((e, a))
    // }

    import skunk.codec.all._
    implicit lazy val et: Out[IO, QueryResult[EntityTable]] = tpe[IO, QueryResult[EntityTable]](
      "Entity",
      "id" -> query { (t: EntityTable) => Select(sql"${t.id}".apply(Void), text) },
      "name" -> query { (t: EntityTable) => Select(sql"${t.name}".apply(Void), text) }
    )

    tpe[IO, QueryResult[ContractTable]](
      "Contract",
      "id" -> query { (t: ContractTable) => Select(sql"${t.id}".apply(Void), text) },
      "name" -> query { (t: ContractTable) => Select(sql"${t.name}".apply(Void), text) },
      "entities" -> query { (t: ContractTable) =>
        for {
          e <- join[List, EntityTable](EntityTable(_))(e => sql"${e.contractId} = ${t.id}".apply(Void))
        } yield cont(e)
      }
    )
    val q2: Field[Pure, QueryResult[String], Option[(String, String, String, String)]] =
      query { (contractId: String) =>
        import skunk.codec.all._
        for {
          c <- join(ContractTable(_))(c => sql"${c.id} = ${text}".apply(contractId))
          e <- join[Option, EntityTable](EntityTable(_))(e => sql"${e.contractId} = ${c.id}".apply(Void))
        } yield (
          Select(sql"${c.id}".apply(Void), text),
          Select(sql"${c.name}".apply(Void), text),
          Select(sql"${e.id}".apply(Void), text),
          Select(sql"${e.name}".apply(Void), text)
        ).tupled
      }

    sealed trait Query[G[_], A]
    case class Join[G[_], T <: Table[?]](
        tbl: Fragment[Void] => T,
        joinPred: T => AppliedFragment,
        jt: JoinType[G]
    ) extends Query[G, T]
    // case class Pure[A]()
  }

  trait QueryResult[A] {
    def read[F[_], A0, B, ArgType, Q](tfa: TableFieldAttribute[F, A0, B, ArgType, Q]): Option[Q]
  }

  sealed trait Query2[G[_], Now, Later] {
    def flatMap[H[_], Now2](f: Now => Query2[H, Now2, Later]): Query2[Lambda[X => G[H[X]]], Now2, Later] =
      Query2.FlatMap(this, f)

    def map[Now2](f: Now => Now2): Query2[G, Now2, Later] =
      flatMap(n => Query2.Pure(f(n)))
  }
  object Query2 {
    case class Pure[Now, Later](a: Now) extends Query2[Lambda[X => X], Now, Later]
    case class Select[Later](col: AppliedFragment, decoder: Decoder[Later]) extends Query2[Lambda[X => X], Unit, Later]
    implicit val applyForSelect: Apply[Select] = ???
    case class Join[G[_], Later, T <: Table[?]](
        tbl: Fragment[Void] => T,
        joinPred: T => AppliedFragment,
        jt: JoinType[G]
    ) extends Query2[G, T, Later]
    case class FlatMap[G[_], H[_], Now, Now2, Later](
        fa: Query2[G, Now, Later],
        f: Now => Query2[H, Now2, Later]
    ) extends Query2[Lambda[X => G[H[X]]], Now2, Later]

    val q2 = Join(
      ContractTable(_),
      (t: ContractTable) => sql"${t.id} = 42".apply(Void),
      JoinType.One
    )

    val q3 = q2.flatMap { c =>
      Join(
        EntityTable(_),
        (e: EntityTable) => sql"${e.id} = ${c.alias}.entity_id".apply(Void),
        JoinType.Traversable[List](implicitly)
      )
    }

    /*val q4 = q3.flatMap{ e =>
      Select(void"data", skunk.codec.all.text)
    }*/
    val out = for {
      c <- Join(
        ContractTable(_),
        (t: ContractTable) => sql"${t.id} = 42".apply(Void),
        JoinType.One
      )
      e <- Join(
        EntityTable(_),
        (e: EntityTable) => sql"${e.id} = ${c.alias}.entity_id".apply(Void),
        JoinType.Traversable[List](implicitly)
      )
      _ <- (
        Select(sql"${e.alias}.data".apply(Void), skunk.codec.all.text),
        Select(sql"${e.alias}.data".apply(Void), skunk.codec.all.int4)
      ).tupled
    } yield ()
  }

  /*
  joinAnd(ContractTable(_))(c => sql"${c.contractId} = 42"-apply(Void)) { c =>
    join(EntityTable(_))(e => sql"${e.id} = ${c.entityId}".apply(Void))
  }
   */

  sealed trait QueryBase[A]
  // case class Continue[A](q: Query[A]) extends QueryBase[QueryResult[A]]

  sealed trait Query[A] extends QueryBase[A]
  case class Continue[A](passthrough: A) extends Query[QueryResult[A]]
  case class Pure[A](a: A) extends Query[A]
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
  ) extends Query[G[A]]

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
  /*
  def runQuery[F[_], A, B](pool: Resource[F, Session[F]])(f: A => Query[B])(implicit tpe: => Out[F, B]): Field[F, A, B] = {
    var name = 0
    def write[A](a: A): Unit = ()
    def nextName: String = {
      name += 1
      s"t$name"
    }
    def findField[C](passthrough: C, m: FieldMeta[F]) = {
      def findQueryNodes[ArgType, Q](hd: TableFieldAttribute[F, C, ?, ArgType, Q], tl: List[TableFieldAttribute[F, ?, ?, ?, ?]]) = {
        val input: Either[String, ArgType] = hd.arg match {
          case EmptyableArg.Empty => Right(())
          case EmptyableArg.Lift(a) =>
            m
              .arg(a)
              .toRight(
                s"query error, coudln't find argument while querying field ${m.astNode.alias.getOrElse(m.astNode.name)}"
              )
        }

        input.map { i =>
          def evalQuery[Q](q: Query[Q]): Decoder[Q] = q match {
            // run next
            case cont: Continue[q] =>
              ???
            case Select(col, dec) =>
              write(col)
              dec
            case ap: Ap[a, b] => evalQuery(ap.f).ap(evalQuery(ap.a))
            case j: Join[g, a, t] =>
              val n = sql"#$nextName"
              val t = j.tbl(n)
              val jp = j.joinPred(t)
              val q2 = j.sq(t)
              evalQuery(q2)
              null
          }

          val q = hd.query(passthrough, i)
        }

        val an = m.astNode
        // an.source.attributes.collect {
        //   case tfa: TableFieldAttribute[F, ?, ?, Arg, ?] =>
        //     tfa
        // }

        ???
      }
    }

    ???
  }
   */
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
    val (contractId, selContractId) = sel("contractId", text)
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
  /*
  case class Portfolio(id: String)
  implicit val portfolio = tpe[IO, Portfolio](
    "Portfolio",
    "id" -> lift(_.id),
    "contracts" -> runQuery(null) { p =>
      join(ContractTable(_))(c => sql"${c.portfolioId} = $text".apply(p.id))
    }
  )*/

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
