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

trait QueryAlgebra {
  type Empty
  def Empty: Empty

  type Frag[A]
  implicit def contravariantSemigroupalForFrag: ContravariantSemigroupal[Frag]
  def stringToFrag(s: String): Frag[Empty]

  type Encoder[A]
  type Decoder[A]
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]]
  implicit def applicativeForDecoder: Applicative[Decoder]

  type AppliedFragment
  def liftFrag[A0](frag: Frag[A0], arg: A0): AppliedFragment

  case class ExtractedFrag[A](frag: Frag[A], arg: A)
  def extractFrag(af: AppliedFragment): ExtractedFrag[?]

  sealed trait FieldVariant[Q, A]
  object FieldVariant {
    case class Selection[A]() extends FieldVariant[Query.Select[A], A]
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

    implicit lazy val joinTypeOne: JoinType[Lambda[A => A]] = JoinType.One
  }
  trait LowPrioJoinTypeImplicits1 extends LowPrioJoinTypeImplicits2 {
    implicit lazy val joinTypeOpt: JoinType[Option] = JoinType.Opt
  }
  trait LowPrioJoinTypeImplicits2 {
    def make[G[_]](fromList: List ~> Lambda[X => Either[String, G[X]]])(implicit G: Traverse[G]): JoinType[G] =
      JoinType.Many(fromList, G)
    implicit lazy val joinTypeList: JoinType[List] = make {
      new (List ~> Lambda[X => Either[String, List[X]]]) {
        override def apply[A](fa: List[A]): Either[String, List[A]] = Right(fa)
      }
    }
  }

  sealed trait Query[G[_], A] {
    def flatMap[H[_], B](f: A => Query[H, B]): Query[Lambda[X => G[H[X]]], B] =
      Query.FlatMap(this, f)

    def map[B](f: A => B): Query[G, B] = flatMap[Lambda[X => X], B](a => Query.Pure(f(a)))

    def mapK[H[_]](fk: G ~> H): Query[H, A] = Query.MapK(this, fk)
  }
  object Query {
    case class Join[G[_], T <: Table[?]](
        tbl: Frag[Empty] => T,
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
    implicit lazy val applyForSelect: Apply[Select] = new Apply[Select] {
      override def map[A, B](fa: Select[A])(f: A => B): Select[B] =
        fa.copy(decoder = fa.decoder.map(f))

      override def ap[A, B](ff: Select[A => B])(fa: Select[A]): Select[B] = {
        (extractFrag(ff.col), extractFrag(fa.col)) match {
          case (eff: ExtractedFrag[a], efa: ExtractedFrag[b]) =>
            Select(
              liftFrag((eff.frag, stringToFrag(", "), efa.frag).tupled, (eff.arg, Empty, efa.arg)),
              ff.decoder ap fa.decoder
            )
        }
      }
    }
  }

  trait TableDef[A] {
    def table: AppliedFragment
    def pk: AppliedFragment
    def pkEncoder: Encoder[A]
    def pkDecoder: Decoder[A]
  }

  trait Table[A] extends TableDef[A] {
    def alias: Frag[Empty]

    def aliased[A](x: Frag[A]): Frag[A] =
      (alias, stringToFrag("."), x).tupled.contramap { x => (Empty, Empty, x) }

    def aliased(x: AppliedFragment)(implicit dummy: DummyImplicit): AppliedFragment =
      extractFrag(x) match {
        case ef: ExtractedFrag[a] => liftFrag(aliased[a](ef.frag), ef.arg)
      }

    def select[A](name: AppliedFragment, dec: Decoder[A]): Query.Select[A] =
      Query.Select(aliased(name), dec)

    def col(name: String): Frag[Empty] =
      aliased(stringToFrag(name))

    def sel[A](name: String, dec: Decoder[A]): (Frag[Empty], Query.Select[A]) = {
      val c = col(name)
      c -> Query.Select(liftFrag(c, Empty), dec)
    }

    def selPk: Query.Select[A] = select(pk, pkDecoder)
  }

  trait TableAlg[T <: Table[?]] {
    def make: Frag[Empty] => T

    def join[G[_]: JoinType](joinPred: T => AppliedFragment): Query.Join[G, T] =
      Query.Join(make, joinPred, implicitly[JoinType[G]])

    def join[G[_]: JoinType](joinPred: T => Frag[Empty])(implicit dummy: DummyImplicit): Query.Join[G, T] =
      Query.Join(make, joinPred.andThen(x => liftFrag(x, Empty)), implicitly[JoinType[G]])
  }

  object Interpreter {
    case class QueryJoin(
        tbl: AppliedFragment,
        pred: AppliedFragment
    )
    case class QueryContent(
        selections: Chain[AppliedFragment],
        joins: Chain[QueryJoin]
    )
    implicit lazy val monoidForQueryContent: Monoid[QueryContent] = new Monoid[QueryContent] {
      override def combine(x: QueryContent, y: QueryContent): QueryContent =
        QueryContent(x.selections ++ y.selections, x.joins ++ y.joins)

      override def empty: QueryContent = QueryContent(Chain.empty, Chain.empty)
    }

    type Effect[A] = EitherT[StateT[Writer[QueryContent, *], Int, *], String, A]
    val Effect = Monad[Effect]
    val S = Stateful[Effect, Int]
    val T = Tell[Effect, QueryContent]
    val R = Raise[Effect, String]
    val nextId = S.get.map(i => stringToFrag(s"t${i.toString()}")) <* S.modify(_ + 1)
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
        (qsa.decoder, qsb.decoder).tupled,
        qsb.value,
        fk
      )
    }

    def handleFlatMap[G[_], H[_], A, B](fm: Query.FlatMap[G, H, A, B]): Effect[QueryState[Lambda[X => G[H[X]]], ?, B]] = {
      for {
        qsa <- go(fm.fa)
        qsb <- go(fm.f(qsa.value))
      } yield mergeFlatMap(qsa, qsb)
    }

    def go[G[_], C](q: Query[G, C]): Effect[QueryState[G, ?, C]] = q match {
      case p: Query.Pure[a]              => Effect.pure(QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], p.a, FunctionK.id[G]))
      case s: Query.Select[a]            => Effect.pure(QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], s, FunctionK.id[G]))
      case fm: Query.FlatMap[g, h, a, b] => handleFlatMap(fm)
      case j: Query.Join[g, t] =>
        for {
          n <- nextId
          t = j.tbl(n)
          jp = j.joinPred(t)
          tbl = t.table
          ef = extractFrag(tbl)
          _ <- ef match {
            case ef: ExtractedFrag[a] =>
              addJoin(
                liftFrag((ef.frag, stringToFrag(" as "), n).tupled, (ef.arg, Empty, Empty)),
                jp
              )
          }
          // sql"${ef.frag} as ${n}".apply(ef.arg), jp)
          _ <- addSelection(t.selPk.col)
        } yield {
            t match {
                case t: Table[a] => 
                                  QueryStateImpl(
                ReassocOpt[G, a](j.jt.reassoc),
                optDecoder(t.pkDecoder),
                t.asInstanceOf[C],
                FunctionK.id[G]
              ) // TODO figure out why this is necessary

            }
        }
      case mapK: Query.MapK[g, h, a] =>
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
}
