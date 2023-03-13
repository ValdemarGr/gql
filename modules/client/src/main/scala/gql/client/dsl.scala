package gql.client

import cats.data._
import cats.implicits._
import gql._
import io.circe.Json
import gql.parser.{Value => V, QueryAst => P}

object dsl {
  type V = gql.parser.Value[gql.parser.AnyValue]
  def sel[A](fieldName: String, alias: Option[String] = None)(implicit sq: SubQuery[A]): SelectionSet[A] =
    SelectionSet.lift(Selection.Field(fieldName, alias, Nil, sq))

  def sel[A](fieldName: String, argHd: P.Argument, argTl: P.Argument*)(implicit sq: SubQuery[A]): SelectionSet[A] =
    SelectionSet.lift(Selection.Field(fieldName, None, argHd :: argTl.toList, sq))

  def sel[A](fieldName: String, alias: String, argHd: P.Argument, argTl: P.Argument*)(implicit sq: SubQuery[A]): SelectionSet[A] =
    SelectionSet.lift(Selection.Field(fieldName, Some(alias), argHd :: argTl.toList, sq))

  def inlineFrag[A](on: String, matchAlso: String*)(implicit q: SelectionSet[A]): SelectionSet[Option[A]] =
    SelectionSet.lift(Selection.InlineFragment(on, Chain.fromSeq(matchAlso), q))

  def fragment[A](name: String, on: String, matchAlso: String*)(implicit q: SelectionSet[A]): SelectionSet[Option[A]] =
    SelectionSet.lift(Selection.Fragment(name, on, Chain.fromSeq(matchAlso), q))

  def list[A](implicit sq: SubQuery[A]): SubQuery[List[A]] = ListModifier(sq)

  def opt[A](implicit sq: SubQuery[A]): SubQuery[Option[A]] = OptionModifier(sq)

  def oneOf[A](hd: SelectionSet[Option[A]], tl: SelectionSet[Option[A]]*) =
    NonEmptyChain
      .of(hd, tl: _*)
      .nonEmptySequence
      .emap { xs =>
        val hd = xs.collect { case Some(x) => x }
        if (hd.length > 1) Left("More than one sub-SelectionSet matched")
        else Right(hd.headOption)
      }

  def variable[A](name: String, default: Option[V] = None)(implicit
      tn: Typename[A],
      encoder: io.circe.Encoder[A]
  ): Var[A, VariableName[A]] = Var[A](name, tn.stack.invert.show(identity), default)

  def value[A](a: A)(implicit enc: io.circe.Encoder[A]) =
    V.fromJson(enc(a))

  def arg(name: String, value: V): P.Argument =
    P.Argument(name, value)

  def arg(name: String, vn: VariableName[?]): P.Argument =
    P.Argument(name, vn.asValue)

  def arg[A](name: String, a: A)(implicit enc: io.circe.Encoder[A]): P.Argument =
    P.Argument(name, value(a))

  final case class Typename[A](stack: InverseModifierStack[String]) {
    def push[B](m: InverseModifier): Typename[B] = Typename(stack.push(m))
  }
  object Typename {
    implicit def typenameStackForList[A](implicit ta: Typename[A]): Typename[List[A]] =
      ta.push(InverseModifier.List)

    implicit def typenameStackForOption[A](implicit ta: Typename[A]): Typename[Option[A]] =
      ta.push(InverseModifier.Nullable)

    implicit lazy val typenameForString: Typename[String] =
      Typename(InverseModifierStack(Nil, "String"))

    implicit lazy val typenameForInt: Typename[Int] =
      Typename(InverseModifierStack(Nil, "Int"))

    implicit lazy val typenameForFloat: Typename[Float] =
      Typename(InverseModifierStack(Nil, "Float"))

    implicit lazy val typenameForBoolean: Typename[Boolean] =
      Typename(InverseModifierStack(Nil, "Boolean"))
  }

  implicit class SyntaxForOptionalSelectionSet[A](q: SelectionSet[Option[A]]) {
    def required: SelectionSet[A] =
      q.emap(_.toRight("Required field was null"))
  }

  val vc: VariableClosure[(String, String), Option[String]] = variable[Option[String]]("id").introduce { id =>
    (
      sel[String]("userName", arg("id", id)),
      sel[String]("email", arg("id", id))
    ).tupled
  }

  val q: ParameterizedQuery[(String, String), Option[String]] =
    Query.parameterized(gql.parser.QueryAst.OperationType.Query, "coolQuery", vc)

  val cq: Query.Compiled[(String, String)] = q.compile(Some("42"))

  val str = cq.query

  val vars: Option[Json] = cq.variables

  val dec: io.circe.Decoder[(String, String)] = cq.decoder
}
