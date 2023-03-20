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

trait FieldMerging[F[_], G[_], C] {
  import FieldCollection._

  def checkSelectionsMerge(xs: NonEmptyList[SelectionInfo[G]]): F[Unit]

  def checkFieldsMerge(
      a: FieldInfo[G],
      asi: SelectionInfo[G],
      b: FieldInfo[G],
      bsi: SelectionInfo[G]
  ): F[Unit]

  // These technically don't need to be in the trait, but it's convenient because of error handling
  // If needed, they can always be moved
  def compareArguments(name: String, aa: QA.Arguments, ba: QA.Arguments, caret: Option[C]): F[Unit]

  def compareValues(av: V[AnyValue], bv: V[AnyValue], caret: Option[C]): F[Unit]
}