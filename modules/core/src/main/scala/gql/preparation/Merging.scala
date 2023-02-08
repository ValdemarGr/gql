package gql.preparation

import gql.parser.{QueryParser => P, Pos}
import cats.data._
import gql.ast._

object Merging {
  final case class MergedSelectionInfo[G[_], A, B](
    tpe: Type[G, B],
    specify: A => Option[B],
    selections: NonEmptyList[MergedFieldInfo[G, B]]
  )

  final case class MergedFieldInfo[G[_], A](
    name: String,
    alias: Option[String],
    args: Option[P.Argument],
    selection: List[MergedSelectionInfo[G, A, ?]]
  )

  def mergeFields[F[_], G[_]] = {
    
  }
}
