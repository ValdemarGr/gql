package gql

import gql.parser.{QueryAst => QA, Value => V, AnyValue, Const}
import cats.data._
import io.circe._
import cats.mtl._
import cats._
import cats.implicits._
import gql.parser.QueryAst
import gql.parser.Pos

// This is an attempt to generalize a graphql schema

trait SchemaAlg {
  type Out
  trait OutAlg {}
  implicit def outAlg(ot: Out): OutAlg

  type In
  type Toplevel
  type OutToplevel // <: Out with Toplevel
  trait OutToplevelAlg {
    def name: String

    def foldOutToplevel[C](
        onSelectable: Selectable => C,
        onScalar: Scalar => C,
        onEnum: Enum => C
    ): C
  }
  implicit def outToplevelAlg(ot: OutToplevel): OutToplevelAlg

  type InTopLevel // <: In with Toplevel

  type Selectable // <: OutToplevel
  trait SelectableAlg extends OutToplevelAlg {
    def foldSel[C](
        onType: Type => C,
        onInterface: Interface => C,
        onUnion: Union => C
    ): C

    def asSelectable: Selectable

    def fields: Map[String, Field]
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
  trait InputAlg {
    def args: Map[String, Arg]
  }

  type Variant

  type Interface // <: ObjectLike
  trait InterfaceAlg extends ObjectLikeAlg
  implicit def interfaceAlg(t: Interface): InterfaceAlg

  type Scalar // <: OutToplevel with InTopLevel
  type Enum // <: OutToplevel with InTopLevel

  type Field
  trait FieldAlg {
    def args: Map[String, Arg]

    def tpe: InverseModifierStack[OutToplevel]
  }
  implicit def fieldAlg(f: Field): FieldAlg
  def typename: Field

  type Arg
}

trait SchemaQueryOps[F[_], Alg <: SchemaAlg, P[_], C] {
  type SelectionInfo = SchemaQueryOps.SelectionInfo[Alg#Selectable]

  type FieldInfo = SchemaQueryOps.FieldInfo[Alg#Selectable]

  def inFragment[A](
      fragmentName: String,
      caret: Option[C]
  )(faf: P[QA.FragmentDefinition[P]] => F[A]): F[A]

  def matchType(
      name: String,
      sel: Alg#Selectable,
      caret: C
  ): F[Alg#Selectable]

  def collectSelectionInfo(
      sel: Alg#Selectable,
      ss: QA.SelectionSet[P]
  ): F[NonEmptyList[SelectionInfo]]

  def collectFieldInfo(
      qf: Alg#Field,
      f: QA.Field[P],
      caret: C
  ): F[FieldInfo]

  def variables(op: QA.OperationDefinition[P]): F[SchemaQueryOps.VariableMap]

  def collectRoot(
      op: QA.OperationDefinition[P],
      root: Alg#Type
  ): F[NonEmptyList[SelectionInfo]]

  def checkArg(
      a: Alg#Arg,
      value: V[AnyValue],
      inVariableResolution: Boolean
  ): F[Unit]

  def checkInput(
      field: Map[String, Alg#Arg],
      values: Map[String, V[AnyValue]],
      inVariableResolution: Boolean
  ): F[Unit] = ???

  def checkSelectionsMerge(xs: NonEmptyList[SchemaQueryOps.SelectionInfo[Alg#Selectable]]): F[Unit]

  def checkFieldsMerge(
      a: FieldInfo,
      asi: SelectionInfo,
      b: FieldInfo,
      bsi: SelectionInfo
  ): F[Unit]

  // These technically don't need to be in the trait, but it's convenient because of error handling
  // If needed, they can always be moved
  def compareArguments(name: String, aa: QA.Arguments, ba: QA.Arguments, caret: Option[C]): F[Unit]

  def compareValues(av: V[AnyValue], bv: V[AnyValue], caret: Option[C]): F[Unit]
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
      args: Option[QA.Arguments],
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

  final case class PositionalError[C](position: Cursor, caret: List[C], message: String)

  trait Positioned[P[_], C] {
    def apply[A](p: P[A]): A

    def position[A](p: P[A]): C
  }

  def apply[F[_]: Parallel, Alg <: SchemaAlg, P[_], C](alg: Alg)(
      implementations: Map[String, Map[String, Either[alg.Interface, alg.Type]]]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F],
      P: Positioned[P, C]
  ) = {
    def raise[A](message: String, caret: Option[C]): F[A] = ???

    def raiseOpt[A](oa: Option[A], message: String, caret: Option[C]): F[A] = ???

    import alg._
    new SchemaQueryOps[F, alg.type, P, C] {
      override def inFragment[A](
          fragmentName: String,
          caret: Option[C]
      )(faf: P[QA.FragmentDefinition[P]] => F[A]): F[A] = ???

      override def checkArg(
          a: alg.Arg,
          value: V[AnyValue],
          inVariableResolution: Boolean
      ): F[Unit] = ???

      override def checkInput(
          fields: Map[String, alg.Arg],
          values: Map[String, V[AnyValue]],
          inVariableResolution: Boolean
      ): F[Unit] = ???

      override def matchType(
          name: String,
          sel: alg.Selectable,
          caret: C
      ): F[alg.Selectable] = {
        if (sel.name == name) F.pure(sel)
        else {
          sel.foldSel(
            onType = t =>
              // Check downcast
              t.implements.get(name) match {
                case None =>
                  raise(s"Tried to match with type `$name` on type object type `${sel.name}`.", Some(caret))
                case Some(i) => F.pure(i.asSelectable)
              },
            i =>
              // What types implement this interface?
              // We can both downcast and up-match
              i.implements.get(name) match {
                case Some(i) => F.pure(i.asSelectable)
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
                case Some(i) => F.pure(i.asSelectable)
                case None =>
                  raiseOpt(
                    u.variants.values.toList.collectFirstSome(_.implements.get(name)),
                    s"`$name` is not a member of the union `${u.name}` (or any of the union's types' implemented interfaces), possible members are ${u.variants.keySet
                      .mkString(", ")}.",
                    caret.some
                  ).map(_.asSelectable)
              }
          )
        }
      }

      override def collectSelectionInfo(sel: alg.Selectable, ss: QueryAst.SelectionSet[P]): F[NonEmptyList[SelectionInfo]] = {
        val all = ss.selections.map(p => (P.position(p), P(p)))
        val fields = all.collect { case (caret, QA.Selection.FieldSelection(field)) => (caret, field) }

        val actualFields = sel.fields + ("__typename" -> alg.typename)

        val validateFieldsF = fields
          .parTraverse { case (caret, field) =>
            actualFields.get(field.name) match {
              case None    => raise[FieldInfo](s"Field '${field.name}' is not a member of `${sel.name}`.", Some(caret))
              case Some(f) => collectFieldInfo(f, field, caret)
            }
          }
          .map(_.toNel.toList.map(SelectionInfo(sel, _, None)))

        val realInlines = all
          .collect { case (caret, QA.Selection.InlineFragmentSelection(f)) => (caret, f) }
          .parFlatTraverse { case (caret, f) =>
            f.typeCondition.traverse(matchType(_, sel, caret)).map(_.getOrElse(sel)).flatMap { t =>
              collectSelectionInfo(t, f.selectionSet).map(_.toList)
            }
          }

        val realFragments = all
          .collect { case (caret, QA.Selection.FragmentSpreadSelection(f)) => (caret, f) }
          .parFlatTraverse { case (caret, f) =>
            val fn = f.fragmentName
            inFragment(fn, caret.some) { p =>
              val caret = P.position(p)
              val f = P(p)
              matchType(f.typeCnd, sel, caret).flatMap { t =>
                collectSelectionInfo(t, f.selectionSet)
                  .map(_.toList.map(_.copy(fragmentName = Some(fn))))
              }
            }
          }

        (validateFieldsF :: realInlines :: realFragments :: Nil).parFlatSequence
          // Unfortunate, but is always safe here since nel is input
          .map(_.toNel.get)
      }

      override def collectFieldInfo(qf: alg.Field, f: QueryAst.Field[P], caret: C): F[FieldInfo] = {
        val fields = f.arguments.toList.flatMap(_.nel.toList).map(x => x.name -> x.value).toMap
        val verifyArgsF = checkInput(qf.args, fields, inVariableResolution = false)

        val c = P.position(f.selectionSet)
        val x = P(f.selectionSet)
        val name = qf.tpe.inner.name
        val i: F[TypeInfo[alg.Selectable]] = qf.tpe.inner.foldOutToplevel(
          onSelectable = s =>
            raiseOpt(
              x,
              s"Field `${f.name}` of type `${name}` must have a selection set.",
              Some(c)
            ).flatMap(ss => collectSelectionInfo(s, ss))
              .map(TypeInfo.Selectable(name, _)),
          onEnum = _ =>
            if (x.isEmpty)
              F.pure(TypeInfo.Enum(name))
            else raise(s"Field `${f.name}` of enum type `${name}` must not have a selection set.", Some(c)),
          onScalar = _ =>
            if (x.isEmpty)
              F.pure(TypeInfo.Scalar(name))
            else raise(s"Field `${f.name}` of scalar type `${name}` must not have a selection set.", Some(c))
        )

        verifyArgsF &> i.map(fi => FieldInfo(name, f.alias, f.arguments, qf.tpe.copy(inner = fi)))
      }

      override def variables(op: QueryAst.OperationDefinition[P]): F[VariableMap] = ???

      override def collectRoot(op: QueryAst.OperationDefinition[P], root: alg.Type): F[NonEmptyList[SelectionInfo]] = ???

      override def checkSelectionsMerge(xs: NonEmptyList[SelectionInfo]): F[Unit] = ???

      override def checkFieldsMerge(a: FieldInfo, asi: SelectionInfo, b: FieldInfo, bsi: SelectionInfo): F[Unit] = ???

      override def compareArguments(name: String, aa: QueryAst.Arguments, ba: QueryAst.Arguments, caret: Option[C]): F[Unit] = ???

      override def compareValues(av: V[AnyValue], bv: V[AnyValue], caret: Option[C]): F[Unit] = ???
    }
  }
}
