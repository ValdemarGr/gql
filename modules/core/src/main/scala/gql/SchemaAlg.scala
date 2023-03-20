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
  trait ToplevelAlg {
    def name: String
  }
  implicit def toplevelAlg(tl: Toplevel): ToplevelAlg

  type OutToplevel // <: Out with Toplevel
  trait OutToplevelAlg extends ToplevelAlg {
    def foldOutToplevel[C](
        onSelectable: Selectable => C,
        onScalar: Scalar => C,
        onEnum: Enum => C
    ): C
  }
  implicit def outToplevelAlg(ot: OutToplevel): OutToplevelAlg

  type InToplevel // <: In with Toplevel
  trait InToplevelAlg extends ToplevelAlg {
    def foldInToplevel[C](
        onInput: Input => C,
        onScalar: Scalar => C,
        onEnum: Enum => C
    ): C
  }
  implicit def inToplevelAlg(it: InToplevel): InToplevelAlg

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

  type Input // <: InToplevel
  trait InputAlg {
    def args: Map[String, Arg]
  }
  implicit def inputAlg(i: Input): InputAlg

  type Variant

  type Interface // <: ObjectLike
  trait InterfaceAlg extends ObjectLikeAlg
  implicit def interfaceAlg(t: Interface): InterfaceAlg

  type Scalar // <: OutToplevel with InToplevel
  trait ScalarAlg extends OutToplevelAlg with InToplevelAlg
  implicit def scalarAlg(s: Scalar): ScalarAlg

  type Enum // <: OutToplevel with InToplevel
  trait EnumAlg extends OutToplevelAlg with InToplevelAlg {
    def values: NonEmptySet[String]
  }
  implicit def enumAlg(e: Enum): EnumAlg

  type Field
  trait FieldAlg {
    def args: Map[String, Arg]

    def tpe: InverseModifierStack[OutToplevel]
  }
  implicit def fieldAlg(f: Field): FieldAlg
  def typename: Field

  type Arg
  trait ArgAlg {
    def name: String

    def tpe: InverseModifierStack[InToplevel]

    def defaultValue: Option[V[Const]]
  }
  implicit def argAlg(a: Arg): ArgAlg
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

  def checkScalar(
      s: Alg#Scalar,
      value: V[AnyValue]
  ): F[Unit]

  def checkArg(
      a: Alg#Arg,
      value: V[AnyValue],
      ambigiousEnum: Boolean
  ): F[Unit]

  def checkInput(
      field: Map[String, Alg#Arg],
      values: Map[String, V[AnyValue]],
      ambigiousEnum: Boolean
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
      implementations: Map[String, Map[String, Either[alg.Interface, alg.Type]]],
      vars: VariableMap
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

      override def checkScalar(
          s: alg.Scalar,
          value: V[AnyValue]
      ): F[Unit] = {
        val known = Set("String", "Int", "Float", "Boolean")
        val illigal: Set[String] = value match {
          case V.IntValue(_)     => known - "Int"
          case V.FloatValue(_)   => known - "Float"
          case V.StringValue(_)  => known - "String"
          case V.BooleanValue(_) => known - "Boolean"
          case _                 => Set.empty
        }
        if (illigal.contains(s.name))
          raise(
            s"When supplying a value of type `${s.name}`, then a value of type ${illigal.toList.map(n => s"`${n}`").mkString(" or ")} is illigal",
            None
          )
        else F.unit
      }

      override def checkArg(
          a: alg.Arg,
          value: V[AnyValue],
          ambigiousEnum: Boolean
      ): F[Unit] = {
        val stack = a.tpe
        def verifyModifierStack(ms: List[InverseModifier], current: V[AnyValue], ambigiousEnum: Boolean): F[Unit] = (ms, current) match {
          case (_, V.VariableValue(v)) =>
            vars.get(v) match {
              case None =>
                raise(
                  s"Variable '$$$v' was not declared and provided as a possible variable for this operation. Hint add the variable to the variables list of the operation '(..., $$$v: ${a.tpe.invert
                    .show(_.name)})' and provide a value in the variables parameter.",
                  None
                )
              case Some(Right(pval)) => verifyModifierStack(ms, pval, ambigiousEnum = false)
              case Some(Left(j))     => verifyModifierStack(ms, V.fromJson(j), ambigiousEnum = true)
            }
          case (InverseModifier.List :: xs, V.ListValue(vs)) =>
            vs.parTraverse_(verifyModifierStack(xs, _, ambigiousEnum))
          case (InverseModifier.Optional :: _, V.NullValue()) => F.unit
          case (InverseModifier.Optional :: xs, v)            => verifyModifierStack(xs, v, ambigiousEnum)
          case (Nil, _) =>
            val fo: F[Unit] = stack.inner.foldInToplevel(
              onInput = i =>
                current match {
                  case V.ObjectValue(vs) => checkInput(i.args, vs.toMap, ambigiousEnum)
                  case _                 => raise(s"Expected input object", None)
                },
              onScalar = s => checkScalar(s, current),
              onEnum = { e =>
                val fa: F[String] = current match {
                  case V.EnumValue(s)                    => F.pure(s)
                  case V.StringValue(s) if ambigiousEnum => F.pure(s)
                  case _                                 => raise(s"Enum value expected for `${e.name}`.", None)
                }

                fa.flatMap[Unit] { s =>
                  val names = e.values.toList
                  if (e.values.contains(s)) F.unit
                  else
                    raise(
                      s"Enum value `$s` does not occur in enum type `${e.name}`, possible enum values are ${names.map(s => s"`$s`").mkString_(", ")}.",
                      None
                    )
                }
              }
            )
            fo
        }

        verifyModifierStack(stack.modifiers, value, ambigiousEnum)
      }

      override def checkInput(
          fields: Map[String, alg.Arg],
          values: Map[String, V[AnyValue]],
          ambigiousEnum: Boolean
      ): F[Unit] = {
        val tooMany = fields.keySet -- values.keySet
        val tooManyF: F[Unit] =
          if (tooMany.isEmpty) F.unit
          else raise(s"Too many arguments provided for input object: ${tooMany.mkString(", ")}", None)

        val matched = fields.toList.parTraverse_ { case (k, a) =>
          raiseOpt(
            values
              .get(k)
              .orElse(a.defaultValue)
              .orElse(a.tpe.modifiers.headOption.collect { case InverseModifier.Optional => V.NullValue() }),
            s"Missing required argument `$k` and no default value was found",
            None
          ).flatMap(checkArg(a, _, ambigiousEnum))
        }

        tooManyF &> matched
      }

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
        val verifyArgsF = checkInput(qf.args, fields, ambigiousEnum = false)

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
