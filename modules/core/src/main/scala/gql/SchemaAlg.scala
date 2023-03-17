package gql

import gql.parser.{QueryAst => P, Value => V, AnyValue}
import cats.data._
import cats.parse.Caret

// This is an attempt to generalize a graphql schema

trait SchemaAlg {
  type Out
  type In
  type Toplevel
  type OutToplevel // <: Out with Toplevel
  type InTopLevel // <: In with Toplevel
  type Selectable // <: OutToplevel
  type ObjectLike // <: Selectable
  type Implementation
  type Type // <: ObjectLike
  type Input // <: InTopLevel
  type Variant
  type Interface // <: ObjectLike
  type Scalar // <: OutToplevel with InTopLevel
  type Enum // <: OutToplevel with InTopLevel
  type Field
  type Arg
}

trait SchemaQueryOps[F[_]] {
  val schemaAlg: SchemaAlg

  type SelectionInfo = SchemaQueryOps.SelectionInfo[schemaAlg.Selectable]

  type FieldInfo = SchemaQueryOps.FieldInfo[schemaAlg.Selectable]

  def matchType(
      name: String,
      sel: schemaAlg.Selectable,
      caret: Caret
  ): F[schemaAlg.Selectable]

  def collectSelectionInfo(
      sel: schemaAlg.Selectable,
      ss: P.SelectionSet
  ): F[NonEmptyList[SelectionInfo]]

  def collectFieldInfo(
      qf: schemaAlg.Field,
      f: P.Field,
      caret: Caret
  ): F[FieldInfo]

  def checkSelectionsMerge(xs: NonEmptyList[SchemaQueryOps.SelectionInfo[schemaAlg.Selectable]]): F[Unit]

  def checkFieldsMerge(
      a: FieldInfo,
      asi: SelectionInfo,
      b: FieldInfo,
      bsi: SelectionInfo
  ): F[Unit]

  // These technically don't need to be in the trait, but it's convenient because of error handling
  // If needed, they can always be moved
  def compareArguments(name: String, aa: P.Arguments, ba: P.Arguments, caret: Option[Caret]): F[Unit]

  def compareValues(av: V[AnyValue], bv: V[AnyValue], caret: Option[Caret]): F[Unit]
}

object SchemaQueryOps {
  sealed trait TypeInfo[+S]
  object TypeInfo {
    final case class Scalar(name: String) extends TypeInfo[Nothing]
    final case class Enum(name: String) extends TypeInfo[Nothing]
    final case class Selectable[S](name: String, selection: NonEmptyList[SelectionInfo[S]]) extends TypeInfo[S]
  }

  final case class FieldInfo[S](
      name: String,
      alias: Option[String],
      args: Option[P.Arguments],
      tpe: InverseModifierStack[TypeInfo[S]]
  )

  final case class SelectionInfo[S](
      s: S,
      fields: NonEmptyList[FieldInfo[S]],
      fragmentName: Option[String]
  )
}

// trait OutAlg[A]

// trait InAlg[A]

// trait ToplevelAlg[A] {
//   def name(a: A): String

//   def description(a: A): Option[String]
// }

// trait OutToplevelAlg[A] extends OutAlg[A] with ToplevelAlg[A] {
//   def description(a: A): Option[String]
// }

// trait InTopLevelAlg[A] extends InAlg[A] with ToplevelAlg[A]

// trait SelectableAlg[A, Field] extends OutToplevelAlg[A] {
//   def fields(a: A): List[Field]
// }

// trait ObjectLikeAlg[A, Field, Implementation] extends SelectableAlg[A, Field] {
//   def implementations(a: A): Map[String, Implementation]
// }

// trait ImplementationAlg[A, Interface] {
//   def implementation(a: A): Interface
// }

// trait TypeAlg[A, Field, Implementation] extends ObjectLikeAlg[A, Field, Implementation]

// trait InputAlg[A] extends InTopLevelAlg[A]

// trait VariantAlg[A]

// trait InterfaceAlg[A, Field, Implementation] extends ObjectLikeAlg[A, Field, Implementation]

// trait ScalarAlg[A] extends OutToplevelAlg[A] with InTopLevelAlg[A]

// trait EnumAlg[A] extends OutToplevelAlg[A] with InTopLevelAlg[A]

// trait FieldAlg[A]

