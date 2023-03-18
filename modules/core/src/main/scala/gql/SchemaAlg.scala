package gql

import gql.parser.{QueryAst => P, Value => V, AnyValue, Const}
import cats.data._
import cats.parse.Caret
import io.circe._
import cats.mtl._
import cats._
import cats.implicits._
import gql.parser.QueryAst

// This is an attempt to generalize a graphql schema

trait SchemaAlg {
  type Out
  type In
  type Toplevel
  type OutToplevel // <: Out with Toplevel
  trait OutToplevelAlg {
    def name: String
  }
  implicit def outToplevelAlg(ot: OutToplevel): OutToplevelAlg

  type InTopLevel // <: In with Toplevel

  type Selectable // <: OutToplevel
  trait SelectableAlg extends OutToplevelAlg {
    def fold[C](
        onType: Type => C,
        onInterface: Interface => C,
        onUnion: Union => C
    ): C

    def asSelectable: Selectable
  }
  implicit def selectableAlg(sel: Selectable): SelectableAlg

  type ObjectLike // <: Selectable
  trait ObjectLikeAlg extends SelectableAlg {
    def implements: Map[String, Interface]
  }
  implicit def objectLikeAlg(ol: ObjectLike): ObjectLikeAlg

  type Implementation

  type Type // <: ObjectLike
  trait TypeAlg extends ObjectLikeAlg
  implicit def typeAlg(t: Type): TypeAlg

  type Union // <: Selectable
  trait UnionAlg extends SelectableAlg {
    def variants: Map[String, Type]
  }
  implicit def unionAlg(u: Union): UnionAlg

  type Input // <: InTopLevel
  type Variant

  type Interface // <: ObjectLike
  trait InterfaceAlg extends ObjectLikeAlg
  implicit def interfaceAlg(t: Interface): InterfaceAlg

  type Scalar // <: OutToplevel with InTopLevel
  type Enum // <: OutToplevel with InTopLevel
  type Field
  type Arg
}

trait SchemaQueryOps[F[_], Alg <: SchemaAlg] {
  type SelectionInfo = SchemaQueryOps.SelectionInfo[Alg#Selectable]

  type FieldInfo = SchemaQueryOps.FieldInfo[Alg#Selectable]

  def matchType(
      name: String,
      sel: Alg#Selectable,
      caret: Caret
  ): F[Alg#Selectable]

  def collectSelectionInfo(
      sel: Alg#Selectable,
      ss: P.SelectionSet
  ): F[NonEmptyList[SelectionInfo]]

  def collectFieldInfo(
      qf: Alg#Field,
      f: P.Field,
      caret: Caret
  ): F[FieldInfo]

  def variables(op: P.OperationDefinition): F[SchemaQueryOps.VariableMap]

  def collectRoot(
      op: P.OperationDefinition,
      root: Alg#Type
  ): F[NonEmptyList[SelectionInfo]]

  def checkSelectionsMerge(xs: NonEmptyList[SchemaQueryOps.SelectionInfo[Alg#Selectable]]): F[Unit]

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

  type VariableMap = Map[String, Either[Json, V[Const]]]

  final case class Prep(
      cycleSet: Set[String],
      cursor: Cursor
  ) {
    def addEdge(edge: GraphArc): Prep = copy(cursor = cursor add edge)

    def addCycle(s: String): Prep = copy(cycleSet = cycleSet + s)
  }

  object Prep {
    val empty: Prep = Prep(Set.empty, Cursor.empty)
  }

  final case class PositionalError(position: Cursor, caret: List[Caret], message: String)

  object PositionalError {
    import io.circe.syntax._
    implicit val encoder: Encoder.AsObject[PositionalError] = Encoder.AsObject.instance[PositionalError] { pe =>
      Map(
        "message" -> Some(pe.message.asJson),
        "locations" -> pe.caret.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
        "path" -> NonEmptyChain.fromChain(pe.position.path.map(_.asString)).map(_.asJson)
      ).collect { case (k, Some(v)) => k -> v }.asJsonObject
    }
  }

  def apply[F[_]: Parallel, Alg <: SchemaAlg](alg: Alg)(
      implementations: Map[String, Map[String, Either[alg.Interface, alg.Type]]]
  )(implicit
      L: Local[F, Prep],
      S: Stateful[F, Int],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ) = {

    def raise[A](message: String, caret: Option[Caret]): F[A] = ???

    def raiseOpt[A](oa: Option[A], message: String, caret: Option[Caret]): F[A] = ???

    import alg._
    new SchemaQueryOps[F, alg.type] {

      override def matchType(
          name: String,
          sel: alg.Selectable,
          caret: Caret
      ): F[alg.Selectable] = {
        if (sel.name == name) F.pure(sel)
        else {
          sel.fold(
            onType = t =>
              // Check downcast
              t.implements.get(name) match {
                case None =>
                  raise(s"Tried to match with type `$name` on type object type `${sel.name}`.", Some(caret))
                case Some(i) => F.pure(i)
              },
            i =>
              // What types implement this interface?
              // We can both downcast and up-match
              i.implements.get(name) match {
                case Some(i) => F.pure(i)
                case None =>
                  raiseOpt(
                    implementations.get(i.name),
                    s"The interface `${i.name}` is not implemented by any type.",
                    caret.some
                  ).flatMap { m =>
                    raiseOpt(
                      m.get(name).map(e => e.fold(_.asSelectable, _.asSelectable)),
                      s"`$name` does not implement interface `${i.name}`, possible implementations are ${m.keySet.mkString(", ")}.",
                      caret.some
                    )
                  }
              },
            u =>
              // Can match to any type or any of it's types' interfacees
              u.variants.get(name) match {
                case Some(i) => F.pure(i)
                case None =>
                  raiseOpt(
                    u.variants.values.toList.collectFirstSome(_.implements.get(name)),
                    s"`$name` is not a member of the union `${u.name}` (or any of the union's types' implemented interfaces), possible members are ${u.variants.keySet
                      .mkString(", ")}.",
                    caret.some
                  )
              }
          )
        }
        ???
      }

      override def collectSelectionInfo(sel: alg.Selectable, ss: QueryAst.SelectionSet): F[NonEmptyList[SelectionInfo]] = ???

      override def collectFieldInfo(qf: alg.Field, f: QueryAst.Field, caret: Caret): F[FieldInfo] = ???

      override def variables(op: QueryAst.OperationDefinition): F[VariableMap] = ???

      override def collectRoot(op: QueryAst.OperationDefinition, root: alg.Type): F[NonEmptyList[SelectionInfo]] = ???

      override def checkSelectionsMerge(xs: NonEmptyList[SelectionInfo]): F[Unit] = ???

      override def checkFieldsMerge(a: FieldInfo, asi: SelectionInfo, b: FieldInfo, bsi: SelectionInfo): F[Unit] = ???

      override def compareArguments(name: String, aa: QueryAst.Arguments, ba: QueryAst.Arguments, caret: Option[Caret]): F[Unit] = ???

      override def compareValues(av: V[AnyValue], bv: V[AnyValue], caret: Option[Caret]): F[Unit] = ???
    }
  }
}