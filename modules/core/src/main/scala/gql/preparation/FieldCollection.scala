package gql.preparation

import gql.parser.{QueryAst => QA, Value => V, AnyValue, Const}
import cats.data._
import io.circe._
import cats.mtl._
import cats._
import cats.implicits._
import gql.parser.QueryAst
import gql.parser.Pos
import gql.ast._
import gql.Arg
import gql.InverseModifierStack

trait FieldCollection[F[_], G[_], P[_], C] {
    import FieldCollection._

  def matchType(
      name: String,
      sel: Selectable[G, ?],
      caret: C
  ): F[Selectable[G, ?]]

  def collectSelectionInfo(
      sel: Selectable[G, ?],
      ss: QA.SelectionSet[P]
  ): F[NonEmptyList[FieldCollection.SelectionInfo[G]]]

  def collectFieldInfo(
      qf: AbstractField[G, ?],
      f: QA.Field[P],
      caret: C
  ): F[FieldInfo[G]]
}

object FieldCollection {
  sealed trait TypeInfo[+G[_]]
  object TypeInfo {
    final case class Scalar(name: String) extends TypeInfo[Nothing]
    final case class Enum(name: String) extends TypeInfo[Nothing]
    final case class Selectable[G[_]](name: String, selection: NonEmptyList[SelectionInfo[G]]) extends TypeInfo[G]
  }

  final case class SelectionInfo[G[_]](
      s: gql.ast.Selectable[G, ?],
      fields: NonEmptyList[FieldInfo[G]],
      fragmentName: Option[String]
  )

  final case class FieldInfo[G[_]](
      name: String,
      alias: Option[String],
      args: Option[QA.Arguments],
      tpe: InverseModifierStack[TypeInfo[G]]
  )
}
