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
import cats.arrow.FunctionK

trait QueryDsl extends QueryAlgebra {
  def query[F[_], G[_], A, B](f: A => Query[G, Query.Select[B]])(implicit
      tpe: => Out[F, G[B]]
  ): Field[F, QueryResult[A], G[B]] =
    queryFull(EmptyableArg.Empty)((a, _) => f(a), Resolver.id[F, G[B]])(tpe)

  def query[F[_], G[_], A, B, C](a: Arg[C])(f: (A, C) => Query[G, Query.Select[B]])(implicit
      tpe: => Out[F, G[B]]
  ): Field[F, QueryResult[A], G[B]] =
    queryFull(EmptyableArg.Lift(a))((a, c) => f(a, c), Resolver.id[F, G[B]])(tpe)
  /*
  def queryAndThen[F[_], G[_], A, B, D](f: A => Query[G, Query.Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], D])(implicit
      tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] =
    queryFull(EmptyableArg.Empty)((a, _) => f(a), g(Resolver.id[F, G[B]]))(tpe)

  def queryAndThen[F[_], G[_], A, B, C, D](
      a: Arg[C]
  )(f: (A, C) => Query[G, Query.Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], D])(implicit
      tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] =
    queryFull(EmptyableArg.Lift(a))((a, c) => f(a, c), g(Resolver.id[F, G[B]]))(tpe)*/

  def queryAndThen[G[_], A, B](f: A => Query[G, Query.Select[B]]): PartiallyAppliedQueryAndThen[G, A, Unit, B, A] =
    new PartiallyAppliedQueryAndThen[G, A, Unit, B, A](EmptyableArg.Empty, f, (a, _) => a)

  def queryAndThen[G[_], A, B, C](a: Arg[C])(f: (A, C) => Query[G, Query.Select[B]]): PartiallyAppliedQueryAndThen[G, A, C, B, A] =
    new PartiallyAppliedQueryAndThen[G, A, C, B, (A, C)](EmptyableArg.Lift(a), f, (a, c) => (a, c))

  def queryAndThen[F[_], G[_], A, B, C, D](
      a: Arg[C]
  )(f: (A, C) => Query[G, Query.Select[B]])(g: Resolver[F, G[B], G[B]] => Resolver[F, G[B], D])(implicit
      tpe: => Out[F, D]
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

  def table[T <: Table[?]](f: String => T): TableAlg[T] = new TableAlg[T] {
    def make: String => T = f
  }

  def queryFull[F[_], G[_], A, B, C, D](a: EmptyableArg[C])(f: (A, C) => Query[G, Query.Select[B]], resolverCont: Resolver[F, G[B], D])(
      implicit tpe: => Out[F, D]
  ): Field[F, QueryResult[A], D] = {
    val tfa: TableFieldAttribute[F, G, A, B, C, Query.Select[B], D] = new TableFieldAttribute[F, G, A, B, C, Query.Select[B], D] {
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

  final class PartiallyAppliedQueryAndThen[G[_], A, B, C, D](
      ea: EmptyableArg[B],
      g: D => Query[G, Query.Select[C]],
      f: (A, B) => D
  ) {
    def apply[F[_], E](h: Resolver[F, G[C], G[C]] => Resolver[F, G[C], E])(implicit
        tpe: => Out[F, E]
    ): Field[F, QueryResult[A], E] =
      queryFull[F, G, A, C, B, E](ea)((a, b) => g(f(a, b)), h(Resolver.id[F, G[C]]))(tpe)
  }
}
