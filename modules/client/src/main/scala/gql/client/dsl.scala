package gql.client

import cats.data._
import cats.implicits._

object dsl {
    def sel[A](fieldName: String, alias: Option[String] = None)(implicit sq: SubQuery[A]): Query[A] =
    Query.lift(Selection.Field(fieldName, alias, Nil, sq))

  def inlineFrag[A](on: String, matchAlso: String*)(implicit q: Query[A]): Query[Option[A]] =
    Query.lift(Selection.InlineFragment(on, Chain.fromSeq(matchAlso), q))

  def fragment[A](name: String, on: String, matchAlso: String*)(implicit q: Query[A]): Query[Option[A]] =
    Query.lift(Selection.Fragment(name, on, Chain.fromSeq(matchAlso), q))

  def oneOf[A](hd: Query[Option[A]], tl: Query[Option[A]]*) =
    NonEmptyChain
      .of(hd, tl: _*)
      .nonEmptySequence
      .emap { xs =>
        val hd = xs.collect { case Some(x) => x }
        if (hd.length > 1) Left("More than one sub-query matched")
        else Right(hd.headOption)
      }

  implicit class SyntaxForOptionalQuery[A](q: Query[Option[A]]) {
    def required: Query[A] =
      q.emap(_.toRight("Required field was null"))
  }
}
