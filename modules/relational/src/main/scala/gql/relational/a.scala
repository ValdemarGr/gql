package gql.relational

import gql.ast._
import gql.dsl._
import cats.implicits._
import fs2.Pure
import skunk.codec._
import skunk._
import cats._

object Testttt {
    trait Rel[Alg[_], Representation, A] {
        def index: Representation

        def alg: Alg[A]
    }

    trait SkunkRel[A] extends Rel[Decoder, List[String], A]

    implicit val applicativeForSkunkRel: Applicative[SkunkRel] = new Applicative[SkunkRel] {
      override def ap[A, B](ff: SkunkRel[A => B])(fa: SkunkRel[A]): SkunkRel[B] = new SkunkRel[B] {
        def index = ff.index ++ fa.index
        def alg = ff.alg <*> fa.alg
      }

      override def pure[A](x: A): SkunkRel[A] = new SkunkRel[A] {
        def index: List[String] = Nil
        def alg: Decoder[A] = Applicative[Decoder].pure(x)
      }
    }

    def select[A](column: String, dec: Decoder[A]): SkunkRel[A] = new SkunkRel[A] {
        def index = List(column)
        def alg = dec
    }

    import skunk.codec.all._
    val o: SkunkRel[(Int, String)] = (select("id", int4), select("name", text)).tupled

    sealed trait Realized[A] { 
        def fieldNames: List[String]
        def columns: List[String]
        def decoder: Decoder[A]
    }
    final case class Leaf[A](fieldName: String, rel: SkunkRel[A]) extends Realized[A] {
        def fieldNames: List[String] = List(fieldName)
        def columns: List[String] = rel.index
        def decoder: Decoder[A] = rel.alg
    }
    final case class Branch[A, B](fa: Realized[A], fb: Realized[B]) extends Realized[(A, B)] {
        def fieldNames: List[String] = fa.fieldNames ++ fb.fieldNames
        def columns: List[String] = fa.columns ++ fb.columns
        def decoder: Decoder[(A, B)] = fa.decoder.product(fb.decoder)
    }

    def collapse(real: (String, SkunkRel[?])*): Realized[?] =
        real.toList.map{ case (fn, rel) => Leaf(fn, rel) }
            .reduceLeft[Realized[?]]{ case (r1: Realized[a], r2: Realized[b]) => Branch(r1, r2) }
}