/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql.relational

import gql.ast._
import gql.dsl.all._
import cats.implicits._
import cats._
import gql.resolver.Resolver
import cats.data._
import gql.Arg
import gql.EmptyableArg
import cats.arrow.FunctionK
import org.typelevel.scalaccompat.annotation._

// For all query algebras this dsl can exist
abstract class QueryDsl[QA <: QueryAlgebra](val algebra: QA) { self =>
  import algebra._

  private type QueryResult[A] = algebra.QueryResult[A]

  def select[A](decoder: algebra.Decoder[A], colHd: Frag, colTl: Frag*): Query.Select[A] =
    Query.Select(Chain(colHd) ++ Chain.fromSeq(colTl), decoder)

  def query[F[_], G[_], A, B](f: A => Query[G, Query.Select[B]])(implicit
      tpe: => Out[F, G[B]]
  ): Field[F, QueryResult[A], G[B]] =
    queryFull[F, G, A, B, Unit, G[B]](EmptyableArg.Empty)((a, _) => f(a), Resolver.id[F, G[B]])(tpe)

  def query[F[_], G[_], A, B, C](a: Arg[C])(f: (A, C) => Query[G, Query.Select[B]])(implicit
      tpe: => Out[F, G[B]]
  ): Field[F, QueryResult[A], G[B]] =
    queryFull[F, G, A, B, C, G[B]](EmptyableArg.Lift(a))((a, c) => f(a, c), Resolver.id[F, G[B]])(tpe)

  def cont[F[_], G[_], A, B](f: A => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ): Field[F, QueryResult[A], G[QueryResult[B]]] =
    contFull[F, G, A, B, Unit](EmptyableArg.Empty)((a, _) => f(a))(tpe)

  def cont[F[_], G[_], A, B, C](a: Arg[C])(f: (A, C) => Query[G, B])(implicit
      tpe: => Out[F, G[QueryResult[B]]]
  ): Field[F, QueryResult[A], G[QueryResult[B]]] =
    contFull[F, G, A, B, C](EmptyableArg.Lift(a))((a, c) => f(a, c))(tpe)

  def runField[F[_]: Queryable: Applicative, G[_], I, B, ArgType](connection: Connection[F], arg: Arg[ArgType])(
      q: (NonEmptyList[I], ArgType) => Query[G, (Query.Select[I], B)]
  )(implicit tpe: => Out[F, G[QueryResult[B]]]) =
    Field(resolveQuery(EmptyableArg.Lift(arg), q, connection), Eval.later(tpe))

  def runField[F[_]: Queryable: Applicative, G[_], I, B](connection: Connection[F])(
      q: NonEmptyList[I] => Query[G, (Query.Select[I], B)]
  )(implicit tpe: => Out[F, G[QueryResult[B]]]) =
    Field(resolveQuery[F, G, I, B, Unit](EmptyableArg.Empty, (i, _) => q(i), connection), Eval.later(tpe))

  def runFieldSingle[F[_]: Queryable: Applicative, G[_], I, B, ArgType](connection: Connection[F], arg: Arg[ArgType])(
      q: (I, ArgType) => Query[G, B]
  )(implicit tpe: => Out[F, G[QueryResult[B]]]): Field[F, I, G[QueryResult[B]]] =
    Field(resolveQuerySingle(EmptyableArg.Lift(arg), q, connection), Eval.later(tpe))

  def runFieldSingle[F[_]: Queryable: Applicative, G[_], I, B](connection: Connection[F])(
      q: I => Query[G, B]
  )(implicit tpe: => Out[F, G[QueryResult[B]]]): Field[F, I, G[QueryResult[B]]] =
    Field(resolveQuerySingle[F, G, I, B, Unit](EmptyableArg.Empty, (i, _) => q(i), connection), Eval.later(tpe))

  final class BuildWithBuilder[F[_], A] {
    def apply[B](f: RelationalFieldBuilder[F, A] => B): B = f(new RelationalFieldBuilder[F, A]())
  }

  def relBuilder[F[_], A] = new BuildWithBuilder[F, A]

  def newAlias: Query[λ[X => X], String] = algebra.Query.liftF(algebra.nextId)

  def joinFull[A](make: String => A, pred: A => Frag, join: A => Frag): algebra.Query[λ[X => X], A] =
    for {
      n <- newAlias
      a = make(n)
      p = pred(a)
      j = join(a)
      _ <- algebra.Query.liftF(algebra.addJoin(j, p))
    } yield a

  // This is potentially unsafe and pretty low-level
  // Take a look at [[reassociate]] for a safer version and ideas of how to use this properly
  def reassociateFull[G[_], Key](reassoc: QueryAlgebra.Reassoc[G, Key], dec: algebra.Decoder[Key], cols: Frag*): Query[G, Unit] =
    algebra.Query.liftEffect[G, Unit](
      algebra.addSelection(Chain.fromSeq(cols)).as {
        QueryAlgebra.QueryState[algebra.Decoder, G, Key, Unit, G](
          reassoc,
          dec,
          (),
          FunctionK.id[G]
        )
      }
    )

  def reassociate[G[_]: QueryAlgebra.JoinType](sel: Query.Select[?]) = {
    def go[B](sel: Query.Select[B]) =
      reassociateFull[G, Option[B]](
        QueryAlgebra.ReassocOpt(implicitly[QueryAlgebra.JoinType[G]].reassoc[B]),
        algebra.optDecoder(sel.decoder),
        sel.cols.toList: _*
      )
    go(sel)
  }

  trait TableAlg[T <: Table] {
    def make: String => T

    def simpleJoin(joinPred: T => Frag): algebra.Query[λ[X => X], T] =
      joinFull[T](make, joinPred, t => t.table |+| stringToFrag(" as ") |+| stringToFrag(t.alias))

    def join[G[_]: QueryAlgebra.JoinType](joinPred: T => Frag): algebra.Query[G, T] =
      for {
        t <- simpleJoin(joinPred)
        _ <- reassociate[G](t.tableKey)
      } yield t
  }

  def table[T <: Table](f: String => T): TableAlg[T] = new TableAlg[T] {
    def make: String => T = f
  }

  def queryFull[F[_], G[_], A, B, C, D](a: EmptyableArg[C])(f: (A, C) => Query[G, Query.Select[B]], resolverCont: Resolver[F, G[B], D])(
      implicit tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] = {
    val tfa: TableFieldAttribute[G, A, B, C, Query.Select[B]] = new TableFieldAttribute[G, A, B, C, Query.Select[B]] {
      def arg = a
      def query(value: A, argument: C): Query[G, Query.Select[B]] = f(value, argument)
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

    val tfa: TableFieldAttribute[G, A, QueryResult[B], C, B] =
      new TableFieldAttribute[G, A, QueryResult[B], C, B] {
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

  def contVariant[F[_], A, B](f: A => Query[Option, B])(implicit tpe: => Type[F, QueryResult[B]]) = {
    val attr = new UnificationQueryAttribute[A, B, QueryResult[B]] {
      def fieldVariant: FieldVariant[B, QueryResult[B]] = FieldVariant.SubSelection[B]()
      def query(value: A): Query[Option, B] = f(value)
    }
    gql.ast.Variant[F, QueryResult[A], QueryResult[B]](Eval.later(tpe), List(attr)) { qr =>
      qr.read(attr).sequence.map(_.flatten).toIor
    }
  }

  def queryVariant[F[_], A, B](f: A => Query[Option, Query.Select[B]])(implicit tpe: => Type[F, B]) = {
    val attr = new UnificationQueryAttribute[A, Query.Select[B], B] {
      def fieldVariant: FieldVariant[Query.Select[B], B] = FieldVariant.Selection[B]()
      def query(value: A): Query[Option, Query.Select[B]] = f(value)
    }
    gql.ast.Variant[F, QueryResult[A], B](Eval.later(tpe), List(attr)) { qr =>
      qr.read(attr).sequence.map(_.flatten).toIor
    }
  }

  final class PartiallyAppliedRelationalUnion0[F[_], A](private val name: String) {
    def contVariant[B](f: A => Query[Option, B])(implicit tpe: => Type[F, QueryResult[B]]) =
      new PartiallyAppliedRelationalUnion1(name, self.contVariant[F, A, B](f)(tpe))

    def queryVariant[B](f: A => Query[Option, Query.Select[B]])(implicit tpe: => Type[F, B]) =
      new PartiallyAppliedRelationalUnion1(name, self.queryVariant[F, A, B](f)(tpe))
  }

  final class PartiallyAppliedRelationalUnion1[F[_], A](private val name: String, hd: Variant[F, QueryResult[A], ?]) {
    def contVariant[B](f: A => Query[Option, B])(implicit tpe: => Type[F, QueryResult[B]]) =
      Union[F, QueryResult[A]](name, NonEmptyList.of(hd, self.contVariant[F, A, B](f)(tpe)))

    def queryVariant[B](f: A => Query[Option, Query.Select[B]])(implicit tpe: => Type[F, B]) =
      Union[F, QueryResult[A]](name, NonEmptyList.of(hd, self.queryVariant[F, A, B](f)(tpe)))
  }

  final class RelationalUnionOps[F[_], A](private val u: Union[F, QueryResult[A]]) {
    def contVariant[B](f: A => Query[Option, B])(implicit tpe: => Type[F, QueryResult[B]]): Union[F, QueryResult[A]] =
      u.copy(types = u.types :+ self.contVariant[F, A, B](f)(tpe))

    def queryVariant[B](f: A => Query[Option, Query.Select[B]])(implicit tpe: => Type[F, B]): Union[F, QueryResult[A]] =
      u.copy(types = u.types :+ self.queryVariant[F, A, B](f)(tpe))
  }

  implicit def relationalUnionDslOps[F[_], A](u: Union[F, QueryResult[A]]): RelationalUnionOps[F, A] =
    new RelationalUnionOps(u)

  def contImplementation[F[_], A, B](f: B => Query[Option, A])(implicit interface: => Interface[F, QueryResult[B]]) = {
    val attr = new UnificationQueryAttribute[B, A, QueryResult[A]] {
      def fieldVariant: FieldVariant[A, QueryResult[A]] = FieldVariant.SubSelection[A]()
      def query(value: B): Query[Option, A] = f(value)
    }
    gql.ast.Implementation[F, QueryResult[A], QueryResult[B]](Eval.later(interface), List(attr)) { qr =>
      qr.read(attr).sequence.map(_.flatten).toIor
    }
  }

  def queryImplementation[F[_], A, B](f: B => Query[Option, Query.Select[A]])(implicit interface: => Interface[F, QueryResult[B]]) = {
    val attr = new UnificationQueryAttribute[B, Query.Select[A], A] {
      def fieldVariant: FieldVariant[Query.Select[A], A] = FieldVariant.Selection[A]()
      def query(value: B): Query[Option, Query.Select[A]] = f(value)
    }
    gql.ast.Implementation[F, A, QueryResult[B]](Eval.later(interface), List(attr)) { qr =>
      qr.read(attr).sequence.map(_.flatten).toIor
    }
  }

  final class RelationalTypeOps1[F[_], A](private val tpe: Type[F, QueryResult[A]]) {
    def contImplements[B](f: B => Query[Option, A])(implicit interface: => Interface[F, QueryResult[B]]): Type[F, QueryResult[A]] =
      tpe.copy(implementations = self.contImplementation[F, A, B](f)(interface) :: tpe.implementations)
  }

  implicit def relationalTypeDslOps1[F[_], A](tpe: Type[F, QueryResult[A]]): RelationalTypeOps1[F, A] =
    new RelationalTypeOps1(tpe)

  final class RelationalTypeOps0[F[_], A](private val tpe: Type[F, A]) {
    def queryImplements[B](f: B => Query[Option, Query.Select[A]])(implicit interface: => Interface[F, QueryResult[B]]): Type[F, A] =
      tpe.copy(implementations = self.queryImplementation[F, A, B](f)(interface) :: tpe.implementations)
  }

  implicit def relationalTypeDslOps0[F[_], A](tpe: Type[F, A]): RelationalTypeOps0[F, A] =
    new RelationalTypeOps0(tpe)

  final class RelationalFieldBuilder[F[_], A](@unused private val dummy: Boolean = false) {
    def tpe(name: String, hd: (String, Field[F, QueryResult[A], ?]), tl: (String, Field[F, QueryResult[A], ?])*): Type[F, QueryResult[A]] =
      gql.dsl.all.tpe[F, QueryResult[A]](name, hd, tl: _*)

    def union(name: String) = new PartiallyAppliedRelationalUnion0[F, A](name)

    def queryAndThen[G[_], B, C](f: A => Query[G, Query.Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], C])(
        tpe: => Out[F, C]
    ): Field[F, QueryResult[A], C] =
      queryFull[F, G, A, B, Unit, C](EmptyableArg.Empty)((a, _) => f(a), g(Resolver.id[F, G[B]]))(tpe)

    def queryAndThen[G[_], B, C, D](
        a: Arg[C]
    )(f: (A, C) => Query[G, Query.Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], D])(implicit
        tpe: => Out[F, D]
    ): Field[F, QueryResult[A], D] =
      queryFull[F, G, A, B, C, D](EmptyableArg.Lift(a))((a, c) => f(a, c), g(Resolver.id[F, G[B]]))(tpe)

    def contBoundaryFull[G[_]: Reassociateable, H[_], B, C, D, Arg1, Arg2](ea1: EmptyableArg[Arg1], connection: Connection[F])(
        f: (A, Arg1) => Query[G, Query.Select[B]]
    )(ea2: EmptyableArg[Arg2])(continue: (NonEmptyList[B], Arg2) => Query[H, (Query.Select[B], C)])(implicit
        F: Applicative[F],
        Q: Queryable[F],
        tpe: => Out[F, G[H[QueryResult[C]]]]
    ) =
      queryFull[F, G, A, B, Arg1, G[H[QueryResult[C]]]](ea1)(
        f,
        Resolver
          .id[F, G[B]]
          .andThen(
            resolveQueryFull[F, H, G, B, C, Arg2](ea2, continue, connection)
          )
      )(tpe)

    def contBoundary[G[_]: Reassociateable, H[_], B, C, D, ArgType](a: Arg[ArgType], connection: Connection[F])(
        f: (A, ArgType) => Query[G, Query.Select[B]]
    )(continue: (NonEmptyList[B], ArgType) => Query[H, (Query.Select[B], C)])(implicit
        F: Applicative[F],
        Q: Queryable[F],
        tpe: => Out[F, G[H[QueryResult[C]]]]
    ) =
      contBoundaryFull[G, H, B, C, D, ArgType, ArgType](EmptyableArg.Lift(a), connection)(f)(EmptyableArg.Lift(a))(continue)(
        implicitly,
        implicitly,
        implicitly,
        tpe
      )

    def contBoundary[G[_]: Reassociateable, H[_], B, C, D](connection: Connection[F])(
        f: A => Query[G, Query.Select[B]]
    )(continue: NonEmptyList[B] => Query[H, (Query.Select[B], C)])(implicit
        F: Applicative[F],
        Q: Queryable[F],
        tpe: => Out[F, G[H[QueryResult[C]]]]
    ) =
      contBoundaryFull[G, H, B, C, D, Unit, Unit](EmptyableArg.Empty, connection)((i, _) => f(i))(EmptyableArg.Empty)((i, _) =>
        continue(i)
      )(
        implicitly,
        implicitly,
        implicitly,
        tpe
      )

    def query[G[_], B](f: A => Query[G, Query.Select[B]])(implicit
        tpe: => Out[F, G[B]]
    ): Field[F, QueryResult[A], G[B]] =
      self.query(f)(tpe)

    def query[G[_], B, C](a: Arg[C])(f: (A, C) => Query[G, Query.Select[B]])(implicit
        tpe: => Out[F, G[B]]
    ): Field[F, QueryResult[A], G[B]] =
      self.query(a)(f)(tpe)

    def cont[G[_], B](f: A => Query[G, B])(implicit
        tpe: => Out[F, G[QueryResult[B]]]
    ): Field[F, QueryResult[A], G[QueryResult[B]]] =
      self.cont(f)(tpe)

    def cont[G[_], B, C](a: Arg[C])(f: (A, C) => Query[G, B])(implicit
        tpe: => Out[F, G[QueryResult[B]]]
    ): Field[F, QueryResult[A], G[QueryResult[B]]] =
      self.cont(a)(f)(tpe)

    def runField[G[_], I, B, ArgType](connection: Connection[F], arg: Arg[ArgType])(
        q: (NonEmptyList[I], ArgType) => Query[G, (Query.Select[I], B)]
    )(implicit F: Applicative[F], Q: Queryable[F], tpe: => Out[F, G[QueryResult[B]]]) =
      self.runField(connection, arg)(q)(Q, F, tpe)

    def runField[G[_], I, B](connection: Connection[F])(
        q: NonEmptyList[I] => Query[G, (Query.Select[I], B)]
    )(implicit F: Applicative[F], Q: Queryable[F], tpe: => Out[F, G[QueryResult[B]]]) =
      self.runField(connection)(q)(Q, F, tpe)

    def runFieldSingle[G[_], I, B, ArgType](connection: Connection[F], arg: Arg[ArgType])(
        q: (I, ArgType) => Query[G, B]
    )(implicit F: Applicative[F], Q: Queryable[F], tpe: => Out[F, G[QueryResult[B]]]) =
      self.runFieldSingle(connection, arg)(q)(Q, F, tpe)

    def runFieldSingle[G[_], I, B](connection: Connection[F])(
        q: I => Query[G, B]
    )(implicit F: Applicative[F], Q: Queryable[F], tpe: => Out[F, G[QueryResult[B]]]) =
      self.runFieldSingle(connection)(q)(Q, F, tpe)
  }
}
