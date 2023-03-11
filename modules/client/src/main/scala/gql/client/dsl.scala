package gql.client

import cats.data._
import cats.implicits._

object dsl {
    def sel[A](fieldName: String, alias: Option[String] = None)(implicit sq: SubQuery[A]): SelectionSet[A] =
    SelectionSet.lift(Selection.Field(fieldName, alias, Nil, sq))

  def inlineFrag[A](on: String, matchAlso: String*)(implicit q: SelectionSet[A]): SelectionSet[Option[A]] =
    SelectionSet.lift(Selection.InlineFragment(on, Chain.fromSeq(matchAlso), q))

  def fragment[A](name: String, on: String, matchAlso: String*)(implicit q: SelectionSet[A]): SelectionSet[Option[A]] =
    SelectionSet.lift(Selection.Fragment(name, on, Chain.fromSeq(matchAlso), q))

  def oneOf[A](hd: SelectionSet[Option[A]], tl: SelectionSet[Option[A]]*) =
    NonEmptyChain
      .of(hd, tl: _*)
      .nonEmptySequence
      .emap { xs =>
        val hd = xs.collect { case Some(x) => x }
        if (hd.length > 1) Left("More than one sub-SelectionSet matched")
        else Right(hd.headOption)
      }

  implicit class SyntaxForOptionalSelectionSet[A](q: SelectionSet[Option[A]]) {
    def required: SelectionSet[A] =
      q.emap(_.toRight("Required field was null"))
  }
}
