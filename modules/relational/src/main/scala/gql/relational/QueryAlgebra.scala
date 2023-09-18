package gql.relational

import cats.implicits._
import cats._
import gql.resolver.Resolver
import cats.data._
import cats.mtl._
import cats.arrow.FunctionK
import gql.ast._
import gql.EmptyableArg
import gql.resolver.FieldMeta

trait QueryAlgebra {
  type QueryState[G[_], C] = QueryAlgebra.QueryState[Decoder, G, C]
  val QueryStateImpl = QueryAlgebra.QueryStateImpl

  type Decoder[A]
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]]
  implicit def applicativeForDecoder: Applicative[Decoder]

  type Frag
  def stringToFrag(s: String): Frag
  implicit def appliedFragmentMonoid: Monoid[Frag]

  type Connection[F[_]]
  trait Queryable[F[_]] {
    def apply[A](query: Frag, decoder: Decoder[A], connection: Connection[F]): F[List[A]]
  }

  def resolveQueryFull[F[_]: Queryable, G[_], H[_], I, B, ArgType](
      toplevelArg: EmptyableArg[ArgType],
      q: (NonEmptyList[I], ArgType) => Query[G, (Query.Select[I], B)],
      connection: Connection[F]
  )(implicit F: Applicative[F], H: Reassociateable[H]): Resolver[F, H[I], H[G[QueryResult[B]]]] = {
    implicit val T: Traverse[H] = H.traverse
    compileToResolver[F, G, H, I, ArgType, Either[String, G[QueryResult[B]]]](toplevelArg) { (xs, at, fm) =>
      evalQuery(xs, fm, q(xs, at), connection)
    }.emap(_.traverse(_.toIor))
  }

  def resolveQuery[F[_]: Queryable: Applicative, G[_], I, B, ArgType](
      toplevelArg: EmptyableArg[ArgType],
      q: (NonEmptyList[I], ArgType) => Query[G, (Query.Select[I], B)],
      connection: Connection[F]
  ): Resolver[F, I, G[QueryResult[B]]] =
    resolveQueryFull[F, G, Id, I, B, ArgType](toplevelArg, q, connection)

  def resolveQuerySingle[F[_]: Queryable, G[_], I, B, ArgType](
      toplevelArg: EmptyableArg[ArgType],
      q: (I, ArgType) => Query[G, B],
      connection: Connection[F]
  )(implicit F: Applicative[F]): Resolver[F, I, G[QueryResult[B]]] =
    compileToResolver[F, G, Id, I, ArgType, Either[String, G[QueryResult[B]]]](toplevelArg) { (xs, at, fm) =>
      xs.toList
        .traverse { x =>
          val baseQuery = q(x, at)
          val moddedQuery = baseQuery.map(b => (Applicative[Query.Select].pure(x), b))
          evalQuery(NonEmptyList.one(x), fm, moddedQuery, connection)
        }
        .map(_.flatMap(_.toList).toMap)
    }.emap(_.toIor)

  def evalQuery[F[_], G[_], I, B, ArgType](
      xs: NonEmptyList[I],
      fm: FieldMeta[F],
      query: Query[G, (Query.Select[I], B)],
      connection: Connection[F]
  )(implicit F: Applicative[F], queryable: Queryable[F]): F[Map[I, Either[String, G[QueryResult[B]]]]] = {
    val eff = collapseQuery(query).flatMap { qs =>
      val out = compileQueryState(fm.astNode, qs.map { case (_, b) => b }, FieldVariant.SubSelection[B]())
      out tupleLeft qs.map { case (sel, _) => sel }.value
    }

    val (qc, e) = eff.value.runA(1).run

    e match {
      case Left(e) => F.pure(xs.toList.tupleRight(Left(e)).toMap)
      case Right((sel, done: Done[G, a, QueryResult[B]])) =>
        val decoder = (sel.decoder, done.dec).tupled
        val qc2 = QueryContent(sel.cols ++ qc.selections, qc.joins)
        val frag = renderQuery(qc2)
        val result = queryable(frag, decoder, connection)
        result
          .map(_.groupMap { case (k, _) => k } { case (_, v) => v })
          .map(_.fmap(done.reassoc))
    }
  }

  sealed trait FieldVariant[Q, A]
  object FieldVariant {
    case class Selection[A]() extends FieldVariant[Query.Select[A], A]
    case class SubSelection[A]() extends FieldVariant[A, QueryResult[A]]
  }

  trait AnyQueryAttribute[G[_], A]

  trait TableFieldAttribute[G[_], A, B, ArgType, Q] extends FieldAttribute[fs2.Pure] with AnyQueryAttribute[G, B] {
    def arg: EmptyableArg[ArgType]
    def query(value: A, argument: ArgType): Query[G, Q]
    def fieldVariant: FieldVariant[Q, B]
  }

  trait VariantQueryAttribute[A, Q, B] extends VariantAttribute[fs2.Pure] with AnyQueryAttribute[λ[X => X], B] {
    def query(value: A): Query[λ[X => X], Q]
    def fieldVariant: FieldVariant[Q, B]
  }

  trait QueryResult[A] {
    def read[G[_], A0, B, ArgType, Q](tfa: TableFieldAttribute[G, A0, B, ArgType, Q]): Option[Either[String, G[B]]]
  }

  sealed trait Query[G[_], +A] {
    def flatMap[H[_], B](f: A => Query[H, B]): Query[λ[X => G[H[X]]], B] =
      Query.FlatMap(this, f)

    def map[B](f: A => B): Query[G, B] = flatMap[λ[X => X], B](a => Query.pure(f(a)))

    def mapK[H[_]](fk: G ~> H): Query[H, A] =
      Query.LiftEffect(compile[A].map(_.mapK(fk)))

    def widen[B >: A]: Query[G, B] = this.map(a => a)

    def compile[B >: A]: Effect[QueryState[G, B]] = collapseQuery(this)
  }
  object Query {
    case class FlatMap[G[_], H[_], A, B](
        fa: Query[G, A],
        f: A => Query[H, B]
    ) extends Query[λ[X => G[H[X]]], B]
    case class LiftEffect[G[_], A](fa: Effect[QueryState[G, A]]) extends Query[G, A]
    case class Select[A](cols: Chain[Frag], decoder: Decoder[A]) extends Query[λ[X => X], Select[A]]
    object Select {
      implicit lazy val applicativeForSelect: Applicative[Select] = new Applicative[Select] {
        override def pure[A](x: A): Select[A] =
          Select(Chain.empty, Applicative[Decoder].pure(x))

        override def ap[A, B](ff: Select[A => B])(fa: Select[A]): Select[B] =
          Select(ff.cols ++ fa.cols, ff.decoder ap fa.decoder)
      }
    }

    def pure[A](a: A): Query[λ[X => X], A] =
      liftState[λ[X => X], A](QueryAlgebra.QueryState.pure[Decoder, A](a))

    def liftState[G[_], A](state: QueryState[G, A]): Query[G, A] =
      liftEffect[G, A](Effect.pure(state))

    def liftEffect[G[_], A](effect: Effect[QueryState[G, A]]): Query[G, A] =
      LiftEffect(effect)

    def liftF[A](fa: Effect[A]): Query[λ[X => X], A] =
      liftEffect(fa.map(QueryAlgebra.QueryState.pure[Decoder, A](_)))
  }

  trait Table {
    def alias: String

    def table: Frag

    def tableKeys: (Chain[Frag], Decoder[?])

    def aliasedFrag(x: Frag): Frag =
      stringToFrag(alias) |+| stringToFrag(".") |+| x

    def keys(cols: (Frag, Decoder[?])*) =
      Chain.fromSeq(cols.map { case (f, _) => aliasedFrag(f) }) -> cols.traverse { case (_, d) => d.widen[Any] }

    def select[A](name: Frag, dec: Decoder[A]): Query.Select[A] =
      Query.Select(Chain(aliasedFrag(name)), dec)
  }

  case class PreparedQuery[G[_], A, B, C](
      qc: QueryContent,
      done: Done[G, A, B],
      rootQueryValue: C
  )
  def compileToResolver[F[_], G[_], H[_]: Traverse, I, ArgType, O](toplevelArg: EmptyableArg[ArgType])(
      compiler: (NonEmptyList[I], ArgType, FieldMeta[F]) => F[Map[I, O]]
  )(implicit F: Applicative[F]): Resolver[F, H[I], H[O]] = {
    type K[V[_]] = ((ArgType, FieldMeta[F]), V[I])
    Resolver
      .meta[F, H[I]]
      .andThen(toplevelArg.addArg)
      .tupleIn
      .andThen(
        Resolver
          .id[F, K[H]]
          .map { case (k, h) => (h.toList tupleLeft k).toSet }
          .andThen(Resolver.inlineBatch[F, K[Id], O] { xs =>
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
          .emap { case (o, (k, h)) =>
            h.traverse(i => o.get((k, i)).toRightIor("Could not find query result"))
          }
      )
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

              val eff = collapseQuery(q(inputs, a)).flatMap { qs =>
                compileQueryState(fm.astNode, qs, FieldVariant.SubSelection[B]()) tupleLeft qs.value
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

  import gql.{preparation => prep}

  type Effect[A] = EitherT[StateT[Writer[QueryContent, *], Int, *], String, A]
  val Effect = Monad[Effect]
  val S = Stateful[Effect, Int]
  val T = Tell[Effect, QueryContent]
  val R = Raise[Effect, String]
  val nextId: Effect[String] = S.get.map(i => s"t${i.toString()}") <* S.modify(_ + 1)
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
  ): Effect[ArgType] = a match {
    case EmptyableArg.Empty => Effect.unit
    case EmptyableArg.Lift(y) =>
      pdf.arg(y) match {
        case None    => R.raise(s"could not find argument for ${pdf.outputName}")
        case Some(x) => Effect.pure(x)
      }
  }

  def compileNextField[F[_], G[_], A, B, ArgType, Q](
      pdf: prep.PreparedDataField[F, ?, ?],
      a: A,
      tfa: TableFieldAttribute[G, A, B, ArgType, Q]
  ): Effect[Done[G, ?, B]] =
    getArg(pdf, tfa.arg)
      .map(tfa.query(a, _))
      .flatMap(collapseQuery)
      .flatMap(compileQueryState(pdf, _, tfa.fieldVariant))

  def compileQuery2[F[_], G[_], B, Q](
      qs: QueryState[G, Q],
      variant: FieldVariant[Q, B],
      nextTasks: Eval[List[QueryTask[F, ?]]]
  ): Effect[Done[G, ?, B]] =
    variant match {
      case _: FieldVariant.Selection[a] =>
        val sel: Query.Select[B] = qs.value
        addSelection(sel.cols).as {
          Done[G, (qs.Key, Option[B]), B](
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
        val passthrough: Q = qs.value
        val tasks: List[QueryTask[F, ?]] = nextTasks.value
        tasks.traverse{
          case f: QueryTask.Field[F, a] =>
            compileNextField(f.v, passthrough.asInstanceOf[a], f.attr)
              .map(done => (f.attr, done)): Effect[(AnyQueryAttribute[Any, ?], Done[Any, ?, ?])]
          case f: QueryTask.Variant[F, a, b] =>
            f.attr match { case vqa: VariantQueryAttribute[a, q, b] =>
              val fv: FieldVariant[q, b] = vqa.fieldVariant
              collapseQuery[λ[X => X], q](vqa.query(passthrough.asInstanceOf[a])).flatMap{ qs2 =>
                compileQuery2[F, λ[X => X], b, q](qs2, fv, Eval.later(f.fields.mapFilter(getPDFField(_))))
              }.map{ case d: Done[λ[X => X], o, b] =>
                Done[Option, Option[o], b](
                  optDecoder(d.dec),
                  { xs => ???
                  }
                )
              }
            }
            ???
        }
        ???
    }

  def compileQueryState[F[_], G[_], B, ArgType, Q, O](
      pdf: prep.PreparedDataField[F, ?, ?],
      qs: QueryState[G, Q],
      variant: FieldVariant[Q, B]
  ): Effect[Done[G, ?, B]] =
    variant match {
      case _: FieldVariant.Selection[a] =>
        // implicitly[Select[B] =:= Q]
        val sel: Query.Select[B] = qs.value
        addSelection(sel.cols).as {
          Done[G, (qs.Key, Option[B]), B](
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
          type K = TableFieldAttribute[Any, ?, ?, ?, ?]
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
              def read[G[_], A0, B, ArgType, Q](
                  tfa: TableFieldAttribute[G, A0, B, ArgType, Q]
              ): Option[Either[String, G[B]]] =
                doneMap.asInstanceOf[Map[TableFieldAttribute[G, A0, B, ArgType, Q], Done[G, ?, ?]]].get(tfa).flatMap {
                  case (done: Done[G, a, ?]) =>
                    grouped.asInstanceOf[Map[TableFieldAttribute[G, A0, B, ArgType, Q], List[Any]]].get(tfa).map { ys =>
                      done.reassoc(ys.asInstanceOf[List[a]]).map(_.asInstanceOf[G[B]])
                    }
                }
            }
          }

          Done[G, (qs.Key, Map[K, Any]), QueryResult[Q]](
            (qs.decoder, decs).tupled,
            { (xs: List[(qs.Key, Map[K, Any])]) =>
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

  sealed trait QueryTask[F[_], A]
  object QueryTask {
    case class Field[F[_], A](
        v: prep.PreparedDataField[F, QueryResult[A], ?],
        attr: TableFieldAttribute[Any, A, ?, ?, ?]
    ) extends QueryTask[F, A]
    case class Variant[F[_], A, B](
        v: gql.ast.Variant[F, QueryResult[A], B],
        attr: VariantQueryAttribute[A, ?, B],
        fields: List[prep.PreparedDataField[F, B, ?]]
    ) extends QueryTask[F, A]
  }

  def getPDFField[F[_], A, B](pdf: prep.PreparedDataField[F, A, B]): Option[QueryTask.Field[F, ?]] =
    pdf.source.attributes.collectFirst { case a: TableFieldAttribute[Any, a, ?, ?, ?] @unchecked => a }
          .map { case tfa: TableFieldAttribute[Any, a, ?, ?, ?] =>
            QueryTask.Field(pdf.asInstanceOf[prep.PreparedDataField[F, QueryResult[a], ?]], tfa)
          }

  def getNextAttributes2[F[_], A, B](pdf: prep.PreparedDataField[F, A, B]): List[QueryTask[F, ?]] = {
    val sel = findNextSel(pdf.cont.cont)
    val selFields: List[prep.PreparedField[F, ?]] = sel.toList.flatMap(_.fields)

    val dataFields: List[prep.PreparedDataField[F, ?, ?]] =
      selFields.collect { case pdf: prep.PreparedDataField[F, ?, ?] => pdf }

    val specs: List[prep.PreparedSpecification[F, ?, ?]] =
      selFields.collect { case ps: prep.PreparedSpecification[F, ?, ?] => ps }

    val variants = specs.flatMap[QueryTask[F, ?]] { case ps: prep.PreparedSpecification[F, a, b] =>
      ps.specialization match {
        case prep.Specialization.Union(_, v) =>
          v.attributes.collectFirst { case vqa: VariantQueryAttribute[a, q, b] @unchecked =>
            QueryTask.Variant[F, a, b](
              v.asInstanceOf[gql.ast.Variant[F, QueryResult[a], b]],
              vqa,
              ps.selection.asInstanceOf[List[prep.PreparedDataField[F, b, ?]]]
            )
          }.toList
        case prep.Specialization.Type(_) => ps.selection.mapFilter(getPDFField(_))
        case _ => Nil
      }
    }

    val dataTypeFields = dataFields.mapFilter(getPDFField(_))

    dataTypeFields ++ variants
  }

  case class FieldWithAttr[F[_], G[_], A](
      field: prep.PreparedDataField[F, QueryResult[A], ?],
      attr: TableFieldAttribute[G, A, ?, ?, ?]
  )
  def getNextAttributes[F[_], A, B](pdf: prep.PreparedDataField[F, A, B]) = {
    val sel = findNextSel(pdf.cont.cont)
    val selFields: List[prep.PreparedField[F, ?]] = sel.toList.flatMap(_.fields)

    val dataFields: List[prep.PreparedDataField[F, ?, ?]] =
      selFields.collect { case pdf: prep.PreparedDataField[F, ?, ?] => pdf }

    val specs: List[prep.PreparedSpecification[F, ?, ?]] =
      selFields.collect { case ps: prep.PreparedSpecification[F, ?, ?] => ps }

    val typeFields: List[prep.PreparedDataField[F, ?, ?]] =
      dataFields ++ specs.collect { case prep.PreparedSpecification(prep.Specialization.Type(_), xs) => xs }.flatten

    val interfaces: List[(Implementation[F, ?, ?], List[prep.PreparedDataField[F, ?, ?]])] =
      selFields.collect { case prep.PreparedSpecification(prep.Specialization.Interface(_, impl), xs) => (impl, xs) }

    val _ = interfaces

    typeFields
      .flatMap(findNextFields(_))
      .map { x =>
        x.source.attributes.collectFirst { case a: TableFieldAttribute[g, a, ?, ?, ?] @unchecked => a }.map {
          case tfa: TableFieldAttribute[g, a, ?, ?, ?] =>
            FieldWithAttr(x.asInstanceOf[prep.PreparedDataField[F, QueryResult[a], ?]], tfa)
        }
      }
      .collect { case Some(x) => x }
  }

  def findNextFields[F[_], A](p: prep.PreparedField[F, A]): List[prep.PreparedDataField[F, ?, ?]] = p match {
    case prep.PreparedSpecification(_, xs)    => xs
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

  def collapseQuery[G[_], C](q: Query[G, C]): Effect[QueryState[G, C]] = q match {
    case s: Query.Select[a]         => Effect.pure(QueryAlgebra.QueryState.pure(s))
    case le: Query.LiftEffect[g, a] => le.fa
    case fm: Query.FlatMap[g, h, a, b] =>
      for {
        qsa <- collapseQuery(fm.fa)
        qsb <- collapseQuery(fm.f(qsa.value))
      } yield QueryAlgebra.QueryState.meld(qsa, qsb)
  }
}

object QueryAlgebra {
  trait QueryState[Decoder[_], G[_], C] { self =>
    type Key
    type T[_]
    def reassoc: Reassoc[T, Key]
    def decoder: Decoder[Key]
    def value: C
    def fk: T ~> G
    def map[B](f: C => B): QueryState[Decoder, G, B] =
      QueryStateImpl(reassoc, decoder, f(value), fk)
    def mapK[H[_]](fk: G ~> H): QueryState[Decoder, H, C] =
      QueryStateImpl(reassoc, decoder, value, fk compose self.fk)
  }

  object QueryState {
    type Aux[Decoder[_], G[_], C, K0, T0[_]] = QueryState[Decoder, G, C] {
      type T[A] = T0[A]
      type Key = K0
    }

    def apply[Decoder[_], G[_], Key, C, T0[_]](
        reassoc0: Reassoc[T0, Key],
        decoder0: Decoder[Key],
        value0: C,
        fk0: T0 ~> G
    ): QueryState.Aux[Decoder, G, C, Key, T0] =
      QueryStateImpl(reassoc0, decoder0, value0, fk0)

    def pure[Decoder[_]: Applicative, A](c: A): QueryState[Decoder, λ[X => X], A] =
      QueryStateImpl(JoinType.One.reassoc[Unit], ().pure[Decoder], c, FunctionK.id[Id])

    def meld[Decoder[_]: Applicative, G[_], H[_], A, B](
        qsa: QueryState[Decoder, G, A],
        qsb: QueryState[Decoder, H, B]
    ): QueryState[Decoder, λ[X => G[H[X]]], B] = {
      type N[A] = qsa.T[qsb.T[A]]
      type AK = qsa.Key
      type BK = qsb.Key

      val reassoc: Reassoc[N, (AK, BK)] = new Reassoc[N, (AK, BK)] {
        def traverse = Reassociateable
          .reassociateStep(
            Reassociateable.reassocaiteForAnyTraverse(qsa.reassoc.traverse),
            Reassociateable.reassocaiteForAnyTraverse(qsb.reassoc.traverse)
          )
          .traverse

        override def apply[A](fa: List[((AK, BK), A)]): Either[String, N[List[A]]] = {
          val ys = fa.map { case ((ak, bk), a) => (ak, (bk, a)) }
          qsa.reassoc(ys).flatMap { gs =>
            qsa.reassoc.traverse.traverse(gs) { bs =>
              qsb.reassoc(bs)
            }
          }
        }
      }

      val fk = new (λ[X => qsa.T[qsb.T[X]]] ~> λ[X => G[H[X]]]) {
        def apply[A](fa: qsa.T[qsb.T[A]]): G[H[A]] =
          qsa.fk(qsa.reassoc.traverse.map(fa)(t2 => qsb.fk(t2)))
      }

      QueryStateImpl(reassoc, (qsa.decoder, qsb.decoder).tupled, qsb.value, fk)
    }
  }

  // Trivial implementation of QueryState
  final case class QueryStateImpl[Decoder[_], G[_], I[_], K, C](
      reassoc: Reassoc[I, K],
      decoder: Decoder[K],
      value: C,
      fk: I ~> G
  ) extends QueryState[Decoder, G, C] {
    type T[A] = I[A]
    type Key = K
  }

  // Structure that aids re-construction of hierarchical data from flat data
  trait Reassoc[G[_], Key] {
    def traverse: Traverse[G]

    def apply[A](fa: List[(Key, A)]): Either[String, G[List[A]]]
  }
  object Reassoc {
    def id: Reassoc[λ[X => X], Unit] = new Reassoc[λ[X => X], Unit] {
      def traverse = Traverse[Id]
      def apply[A](fa: List[(Unit, A)]): Either[String, Id[List[A]]] = Right(fa.map { case (_, a) => a })
    }
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

  // A convinient structure where you can request a result re-association for a given G (List, Option, Id)
  sealed trait JoinType[G[_]] {
    def reassoc[Key]: Reassoc[G, Key]
  }
  object JoinType extends LowPrioJoinTypeImplicits1 {
    // A => A instead of Id since scala doesn't want reduce Id to A => A, and Id is noisy
    case object One extends JoinType[λ[A => A]] {
      def reassoc[Key]: Reassoc[λ[A => A], Key] = new ReassocGroup[λ[X => X], Key] {
        def traverse = Traverse[λ[X => X]]
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
        fromListFK: List ~> λ[X => Either[String, G[X]]],
        traverse: Traverse[G]
    ) extends JoinType[G] {
      def traverse0 = traverse
      def reassoc[Key]: Reassoc[G, Key] = new ReassocGroup[G, Key] {
        def traverse = traverse0
        def groups[A](fa: List[List[A]]): Either[String, G[List[A]]] =
          fromListFK(fa)
      }
    }

    implicit lazy val joinTypeOne: JoinType[λ[A => A]] = JoinType.One
  }
  trait LowPrioJoinTypeImplicits1 extends LowPrioJoinTypeImplicits2 {
    implicit lazy val joinTypeOpt: JoinType[Option] = JoinType.Opt
  }
  trait LowPrioJoinTypeImplicits2 {
    def make[G[_]](fromList: List ~> λ[X => Either[String, G[X]]])(implicit G: Traverse[G]): JoinType[G] =
      JoinType.Many(fromList, G)
    implicit lazy val joinTypeList: JoinType[List] = make {
      new (List ~> λ[X => Either[String, List[X]]]) {
        override def apply[A](fa: List[A]): Either[String, List[A]] = Right(fa)
      }
    }
  }
}

// A typeclass that exists for any traversable which is subject to derivation for nested effects
trait Reassociateable[F[_]] {
  def traverse: Traverse[F]
}

object Reassociateable extends ReassociateableLowPrio1 {
  implicit def reassociateStep[F[_], G[_]](implicit F: Reassociateable[F], G: Reassociateable[G]): Reassociateable[λ[X => F[G[X]]]] = {
    type H[A] = F[G[A]]
    new Reassociateable[H] {
      val instance = Nested.catsDataTraverseForNested(F.traverse, G.traverse)
      def traverse = new Traverse[H] {
        override def foldLeft[A, B](fa: H[A], b: B)(f: (B, A) => B): B =
          instance.foldLeft(Nested(fa), b)(f)
        override def foldRight[A, B](fa: H[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
          instance.foldRight(Nested(fa), lb)(f)
        override def traverse[G[_]: Applicative, A, B](fa: H[A])(f: A => G[B]): G[H[B]] =
          instance.traverse(Nested(fa))(f).map(_.value)
      }
    }
  }
}

trait ReassociateableLowPrio1 extends ReassociateableLowPrio2 {
  implicit lazy val reassociateForId: Reassociateable[Id] = new Reassociateable[Id] {
    def traverse = Traverse[Id]
  }
}

trait ReassociateableLowPrio2 {
  implicit def reassocaiteForAnyTraverse[F[_]](implicit F: Traverse[F]): Reassociateable[F] = new Reassociateable[F] {
    def traverse = F
  }
}
