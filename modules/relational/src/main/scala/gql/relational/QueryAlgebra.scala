package gql.relational

import cats.implicits._
import skunk._
import cats._
import gql.resolver.Resolver
import cats.data._
import cats.mtl._
import cats.arrow.FunctionK
import gql.ast._
import gql.EmptyableArg
import gql.resolver.FieldMeta

trait QueryAlgebra {
  import QueryAlgebra.{QueryState => _, QueryStateImpl => _, _}
  type QueryState[G[_], Key, C] = QueryAlgebra.QueryState[Decoder, G, Key, C]
  val QueryStateImpl = QueryAlgebra.QueryStateImpl

  type Decoder[A]
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]]
  implicit def applicativeForDecoder: Applicative[Decoder]

  type Frag
  def stringToFrag(s: String): Frag
  implicit def appliedFragmentMonoid: Monoid[Frag]

  trait RunQuery[F[_]] {
    def apply[A](query: Frag, decoder: Decoder[A]): F[List[A]]
  }
  def resolveQuery[F[_], G[_], I, B, ArgType](
      toplevelArg: EmptyableArg[ArgType],
      q: (NonEmptyList[I], ArgType) => Query[G, (Query.Select[I], B)],
      runQuery: RunQuery[F]
  )(implicit F: Applicative[F]): Resolver[F, I, G[QueryResult[B]]] =
    compileToResolver[F, G, I, ArgType, Either[String, G[QueryResult[B]]]](toplevelArg) { (xs, at, fm) =>
      evalQuery(xs, fm, q(xs, at), runQuery)
    }.emap(_.toIor)

  def resolveQuerySingle[F[_], G[_], I, B, ArgType](
      toplevelArg: EmptyableArg[ArgType],
      q: (I, ArgType) => Query[G, B],
      runQuery: RunQuery[F]
  )(implicit F: Applicative[F]): Resolver[F, I, G[QueryResult[B]]] =
    compileToResolver[F, G, I, ArgType, Either[String, G[QueryResult[B]]]](toplevelArg) { (xs, at, fm) =>
      xs.toList
        .traverse { x =>
          val baseQuery = q(x, at)
          val moddedQuery = baseQuery.map(b => (Applicative[Query.Select].pure(x), b))
          evalQuery(NonEmptyList.one(x), fm, moddedQuery, runQuery)
        }
        .map(_.flatMap(_.toList).toMap)
    }.emap(_.toIor)

  def evalQuery[F[_], G[_], I, B, ArgType](
      xs: NonEmptyList[I],
      fm: FieldMeta[F],
      query: Query[G, (Query.Select[I], B)],
      runQuery: RunQuery[F]
  )(implicit F: Applicative[F]): F[Map[I, Either[String, G[QueryResult[B]]]]] = {
    val eff = Interpreter.collapseQuery(query).flatMap { qs =>
      val out = Interpreter.compileQueryState(fm.astNode, qs.map { case (_, b) => b }, FieldVariant.SubSelection[B]())
      out tupleLeft qs.map { case (sel, _) => sel }.value
    }

    val (qc, e) = eff.value.runA(1).run

    e match {
      case Left(e) => F.pure(xs.toList.tupleRight(Left(e)).toMap)
      case Right((sel, done: Interpreter.Done[G, a, QueryResult[B]])) =>
        val decoder = (sel.decoder, done.dec).tupled
        val qc2 = Interpreter.QueryContent(sel.cols ++ qc.selections, qc.joins)
        val frag = Interpreter.renderQuery(qc2)
        println(s"running:\n${frag.asInstanceOf[skunk.AppliedFragment].fragment.sql}\n")
        val result = runQuery(frag, decoder)
        result
          .map{ xs => println(s"got ${xs.size} results"); xs }
          .map(_.groupMap { case (k, _) => k } { case (_, v) => v })
          .map(_.fmap(done.reassoc))
    }
  }

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

  sealed trait Query[G[_], A] {
    def flatMap[H[_], B](f: A => Query[H, B]): Query[Lambda[X => G[H[X]]], B] =
      Query.FlatMap(this, f)

    def map[B](f: A => B): Query[G, B] = flatMap[Lambda[X => X], B](a => Query.Pure(f(a)))

    def mapK[H[_]](fk: G ~> H): Query[H, A] = Query.MapK(this, fk)
  }
  object Query {
    case class Join[G[_], T <: Table[?]](
        tbl: String => T,
        joinPred: T => Frag,
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
    case class Select[A](cols: Chain[Frag], decoder: Decoder[A]) extends Query[Lambda[X => X], Select[A]]
    implicit lazy val applicativeForSelect: Applicative[Select] = new Applicative[Select] {
      override def pure[A](x: A): Select[A] = 
        Select(Chain.empty, Applicative[Decoder].pure(x))

      override def ap[A, B](ff: Select[A => B])(fa: Select[A]): Select[B] =
        Select(ff.cols ++ fa.cols, ff.decoder ap fa.decoder)
    }
  }

  trait Table[A] {
    def alias: String

    def table: Frag

    def groupingKey: Frag
    def groupingKeyDecoder: Decoder[A]

    def aliasedFrag(x: Frag): Frag =
      stringToFrag(alias) |+| stringToFrag(".") |+| x

    def select[A](name: Frag, dec: Decoder[A]): Query.Select[A] =
      Query.Select(Chain(aliasedFrag(name)), dec)

    def selGroupKey: Query.Select[A] = select(groupingKey, groupingKeyDecoder)
  }

  trait TableAlg[T <: Table[?]] {
    def make: String => T

    def join[G[_]: JoinType](joinPred: T => Frag): Query.Join[G, T] =
      Query.Join(make, joinPred, implicitly[JoinType[G]])
  }

  final case class PreparedQuery[G[_], A, B, C](
      qc: Interpreter.QueryContent,
      done: Interpreter.Done[G, A, B],
      rootQueryValue: C
  )
  def compileToResolver[F[_], G[_], I, ArgType, O](toplevelArg: EmptyableArg[ArgType])(
      compiler: (NonEmptyList[I], ArgType, FieldMeta[F]) => F[Map[I, O]]
  )(implicit F: Applicative[F]): Resolver[F, I, O] = {
    type K = ((ArgType, FieldMeta[F]), I)
    Resolver
      .meta[F, I]
      .andThen(toplevelArg.addArg)
      .tupleIn
      .map(Set(_))
      .andThen(Resolver.inlineBatch[F, K, O] { xs =>
        val lst = xs.toList
        lst.toNel
          .traverse { nel =>
            val rev = nel.map { case (v, k) => k -> v }.toList.toMap
            val ((a, fm), _) = nel.head
            val inputs = nel.map { case (_, i) => i }
            compiler(inputs, a, fm)
              .map(_.toList.mapFilter { case (i, o) => rev.get(i).tupleRight(i).tupleRight(o) })
              .map(_.toMap)
          }
          .map(_.getOrElse(Map.empty))
      })
      .tupleIn
      .emap { case (o, i) => o.collectFirst { case ((_, k), v) if k == i => v }.toRightIor("Could not find query result") }
  }

  def resolvePreparedQuery[F[_], G[_], I, B, ArgType](
      toplevelArg: EmptyableArg[ArgType],
      q: (NonEmptyList[I], ArgType) => Query[G, B]
  )(implicit F: Applicative[F]): Resolver[F, I, PreparedQuery[G, ?, QueryResult[B], B]] = {
    Resolver
      .meta[F, I]
      .andThen(toplevelArg.addArg)
      .tupleIn
      .map(Set(_))
      .andThen(Resolver.inlineBatch[F, ((ArgType, FieldMeta[F]), I), Ior[String, PreparedQuery[G, ?, QueryResult[B], B]]] { xs =>
        F.pure {
          xs.toList.toNel
            .map { nel =>
              val ((a, fm), _) = nel.head
              val inputs = nel.map { case (_, i) => i }

              val eff = Interpreter.collapseQuery(q(inputs, a)).flatMap { qs =>
                Interpreter.compileQueryState(fm.astNode, qs, FieldVariant.SubSelection[B]()) tupleLeft qs.value
              }

              val (qc, e) = eff.value.runA(1).run

              val pq = e.map { case (b, done) => PreparedQuery(qc, done, b) }

              xs.map(k => k -> pq.toIor).toMap
            }
            .getOrElse(Map.empty)
        }
      })
      .map(_.values.head)
  }.rethrow

  object Interpreter {
    import gql.{preparation => prep}

    type Effect[A] = EitherT[StateT[Writer[QueryContent, *], Int, *], String, A]
    val Effect = Monad[Effect]
    val S = Stateful[Effect, Int]
    val T = Tell[Effect, QueryContent]
    val R = Raise[Effect, String]
    val nextId = S.get.map(i => s"t${i.toString()}") <* S.modify(_ + 1)
    def addJoin(tbl: Frag, pred: Frag): Effect[Unit] =
      T.tell(QueryContent(Chain.empty, Chain(QueryJoin(tbl, pred))))
    def addSelection(f: Chain[Frag]): Effect[Unit] =
      T.tell(QueryContent(f, Chain.empty))

    def renderQuery(qc: QueryContent): Frag = {
      val selections = qc.selections.intercalate(stringToFrag(", "))
      val nl = stringToFrag("\n")
      val suffix = NonEmptyChain.fromChain(qc.joins).foldMap { nec =>
        val (hd, tl) = nec.uncons
        val ys = tl.foldMap { x =>
          val p = x.pred
          val t = x.tbl
          nl |+| stringToFrag("left join ") |+| t |+| stringToFrag(" on ") |+| p
        }
        nl |+| stringToFrag("from ") |+| hd.tbl |+|
          ys |+| nl |+| stringToFrag("where ") |+| hd.pred
      }

      stringToFrag("select ") |+| selections |+| suffix
    }

    case class Done[G[_], A, B](
        dec: Decoder[A],
        reassoc: List[A] => Either[String, G[B]]
    )
    def getArg[F[_], ArgType](
        pdf: prep.PreparedDataField[F, ?, ?],
        a: EmptyableArg[ArgType]
    ): Interpreter.Effect[ArgType] = a match {
      case EmptyableArg.Empty => Effect.unit
      case EmptyableArg.Lift(y) =>
        pdf.arg(y) match {
          case None    => R.raise(s"could not find argument for ${pdf.outputName}")
          case Some(x) => Effect.pure(x)
        }
    }

    def compileNextField[F[_], G[_], A, B, ArgType, Q, O](
        pdf: prep.PreparedDataField[F, ?, ?],
        a: A,
        tfa: TableFieldAttribute[F, G, A, B, ArgType, Q, O]
    ): Interpreter.Effect[Done[G, ?, B]] =
      getArg(pdf, tfa.arg)
        .map(tfa.query(a, _))
        .flatMap(collapseQuery)
        .flatMap(compileQueryState(pdf, _, tfa.fieldVariant))

    def compileQueryState[F[_], G[_], B, ArgType, Q, O, Key](
        pdf: prep.PreparedDataField[F, ?, ?],
        qs: QueryState[G, Key, Q],
        variant: FieldVariant[Q, B]
    ): Interpreter.Effect[Done[G, ?, B]] =
      variant match {
        case _: FieldVariant.Selection[a] =>
          // implicitly[Select[B] =:= Q]
          val sel: Query.Select[B] = qs.value
          addSelection(sel.cols).as {
            Done[G, (Key, Option[B]), B](
              (qs.decoder, optDecoder(sel.decoder)).tupled,
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
            compileNextField(attr.field, passthrough.asInstanceOf[aa], attr.attr)
              .map(done => (done, attr)): Effect[(Done[g, ?, ?], FieldWithAttr[F, Any, ?])]
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

            Done[G, (Key, Map[K, Any]), QueryResult[Q]](
              (qs.decoder, decs).tupled,
              { (xs: List[(Key, Map[K, Any])]) =>
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

    case class FieldWithAttr[F[_], G[_], A](
        field: prep.PreparedDataField[F, QueryResult[A], ?],
        attr: TableFieldAttribute[F, G, A, ?, ?, ?, ?]
    )
    def getNextAttributes[F[_], A, B](pdf: prep.PreparedDataField[F, A, B]) = {
      val selFields: List[prep.PreparedField[F, ?]] = findNextSel(pdf.cont.cont).toList.flatMap(_.fields)
      selFields
        .flatMap(pf => findNextFields(pf))
        .collect { case x: prep.PreparedDataField[F, ?, ?] => x }
        .map { x =>
          x.source.attributes.collectFirst { case a: TableFieldAttribute[F, g, a, ?, ?, ?, ?] @unchecked => a }.map {
            case tfa: TableFieldAttribute[F, g, a, ?, ?, ?, ?] =>
              FieldWithAttr(x.asInstanceOf[prep.PreparedDataField[F, QueryResult[a], ?]], tfa)
          }
        }
        .collect { case Some(x) => x }
    }

    def findNextFields[F[_], A](p: prep.PreparedField[F, A]): List[prep.PreparedDataField[F, ?, ?]] = p match {
      case prep.PreparedSpecification(_, _, xs) => xs
      case pdf: prep.PreparedDataField[F, A, ?] => List(pdf)
    }

    def findNextSel[F[_], A](p: prep.Prepared[F, A]): Option[prep.Selection[F, ?]] = p match {
      case sel: prep.Selection[F, A]        => Some(sel)
      case prep.PreparedList(of, _)         => findNextSel(of.cont)
      case po: prep.PreparedOption[F, ?, ?] => findNextSel(po.of.cont)
      case prep.PreparedLeaf(_, _)          => None
    }

    case class QueryJoin(
        tbl: Frag,
        pred: Frag
    )
    case class QueryContent(
        selections: Chain[Frag],
        joins: Chain[QueryJoin]
    )
    implicit lazy val monoidForQueryContent: Monoid[QueryContent] = new Monoid[QueryContent] {
      override def combine(x: QueryContent, y: QueryContent): QueryContent =
        QueryContent(x.selections ++ y.selections, x.joins ++ y.joins)

      override def empty: QueryContent = QueryContent(Chain.empty, Chain.empty)
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
        qsa <- collapseQuery(fm.fa)
        qsb <- collapseQuery(fm.f(qsa.value))
      } yield mergeFlatMap(qsa, qsb)
    }

    def collapseQuery[G[_], C](q: Query[G, C]): Effect[QueryState[G, ?, C]] = q match {
      case p: Query.Pure[a]              => Effect.pure(QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], p.a, FunctionK.id[G]))
      case s: Query.Select[a]            => Effect.pure(QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], s, FunctionK.id[G]))
      case fm: Query.FlatMap[g, h, a, b] => handleFlatMap(fm)
      case j: Query.Join[g, t] =>
        for {
          n <- nextId
          t = j.tbl(n)
          jp = j.joinPred(t)
          tbl = t.table
          _ <- addJoin(tbl |+| stringToFrag(" as ") |+| stringToFrag(n), jp)
          _ <- addSelection(t.selGroupKey.cols)
        } yield {
          t match {
            case t: Table[a] =>
              QueryStateImpl(
                ReassocOpt[G, a](j.jt.reassoc),
                optDecoder(t.groupingKeyDecoder),
                t.asInstanceOf[C],
                FunctionK.id[G]
              ) // TODO figure out why this is necessary

          }
        }
      case mapK: Query.MapK[g, h, a] =>
        collapseQuery(mapK.fa).map { case (qs: QueryState[g, k, a]) =>
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

object QueryAlgebra {
  trait QueryState[Decoder[_], G[_], Key, C] {
    type T[_]
    def reassoc: Reassoc[T, Key]
    def decoder: Decoder[Key]
    def value: C
    def fk: T ~> G
    def map[B](f: C => B): QueryState[Decoder, G, Key, B] =
      QueryStateImpl(reassoc, decoder, f(value), fk)
  }

  final case class QueryStateImpl[Decoder[_], G[_], I[_], Key, C](
      reassoc: Reassoc[I, Key],
      decoder: Decoder[Key],
      value: C,
      fk: I ~> G
  ) extends QueryState[Decoder, G, Key, C] {
    type T[A] = I[A]
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
}
