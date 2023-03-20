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

trait SchemaAlg { self =>
  type Out

  type In
  type Toplevel
  trait ToplevelAlg {
    def variant: SchemaAlg.ToplevelVariant[self.type]

    def name: String
  }
  implicit def toplevelAlg(tl: Toplevel): ToplevelAlg

  type OutToplevel // <: Out with Toplevel
  trait OutToplevelAlg extends ToplevelAlg {
    override def variant: SchemaAlg.OutToplevelVariant[self.type]
  }
  implicit def outToplevelAlg(ot: OutToplevel): OutToplevelAlg

  type InToplevel // <: In with Toplevel
  trait InToplevelAlg extends ToplevelAlg {
    override def variant: SchemaAlg.InToplevelVariant[self.type]
  }
  implicit def inToplevelAlg(it: InToplevel): InToplevelAlg

  type Selectable // <: OutToplevel
  trait SelectableAlg extends OutToplevelAlg {
    override def variant: SchemaAlg.SelectableVariant[self.type]

    def asSelectable: Selectable

    def fields: Map[String, Field]
  }
  implicit def selectableAlg(sel: Selectable): SelectableAlg
  def embedSelectable(v: SchemaAlg.SelectableVariant[self.type]): Selectable

  type ObjectLike // <: Selectable
  trait ObjectLikeAlg extends SelectableAlg {
    override def variant: SchemaAlg.ObjectLikeVariant[self.type]

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
  trait ScalarAlg extends OutToplevelAlg with InToplevelAlg {
    override def variant: SchemaAlg.ScalarVariant[self.type]
  }
  implicit def scalarAlg(s: Scalar): ScalarAlg

  type Enum // <: OutToplevel with InToplevel
  trait EnumAlg extends OutToplevelAlg with InToplevelAlg {
    override def variant: SchemaAlg.EnumVariant[self.type]
    
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

object SchemaAlg {
  sealed trait Embeddable[T] {
    def embed: T
  }

  sealed trait ToplevelVariant[Alg <: SchemaAlg]

  sealed trait InToplevelVariant[Alg <: SchemaAlg] extends ToplevelVariant[Alg]

  final case class InputVariant[Alg <: SchemaAlg](x: Alg#Input) extends InToplevelVariant[Alg]

  sealed trait OutToplevelVariant[Alg <: SchemaAlg] extends ToplevelVariant[Alg]

  sealed trait SelectableVariant[Alg <: SchemaAlg] extends OutToplevelVariant[Alg]

  sealed trait ObjectLikeVariant[Alg <: SchemaAlg] extends SelectableVariant[Alg]

  final case class TypeVariant[Alg <: SchemaAlg](x: Alg#Type) extends ObjectLikeVariant[Alg]
  final case class InterfaceVariant[Alg <: SchemaAlg](x: Alg#Interface) extends ObjectLikeVariant[Alg]
  final case class UnionVariant[Alg <: SchemaAlg](x: Alg#Union) extends SelectableVariant[Alg]

  final case class EnumVariant[Alg <: SchemaAlg](x: Alg#Enum) extends OutToplevelVariant[Alg] with InToplevelVariant[Alg]
  final case class ScalarVariant[Alg <: SchemaAlg](x: Alg#Scalar) extends OutToplevelVariant[Alg] with InToplevelVariant[Alg]
}

trait SchemaQueryOps[F[_], Alg <: SchemaAlg, P[_], C] {
  type SelectionInfo = SchemaQueryOps.SelectionInfo[Alg#Selectable]

  type FieldInfo = SchemaQueryOps.FieldInfo[Alg#Selectable]

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

  def pValueName(v: V[AnyValue]): String = {
    import V._
    v match {
      case ObjectValue(_)   => "object"
      case StringValue(_)   => "string"
      case ListValue(_)     => "list"
      case V.EnumValue(_)   => "enum"
      case BooleanValue(_)  => "boolean"
      case NullValue()      => "null"
      case FloatValue(_)    => "float"
      case IntValue(_)      => "int"
      case VariableValue(_) => "variable"
    }
  }

  def apply[F[_]: Parallel, Alg <: SchemaAlg, P[_], C](alg: Alg)(
      implementations: Map[String, Map[String, Either[alg.Interface, alg.Type]]],
      vars: VariableMap,
      fragments: Map[String, P[QA.FragmentDefinition[P]]]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F],
      P: Positioned[P, C]
  ) = {
    def raise[A](message: String, caret: Option[C]): F[A] = ???

    def raiseOpt[A](oa: Option[A], message: String, caret: Option[C]): F[A] = ???

    def ambientEdge[A](edge: GraphArc)(fa: F[A]): F[A] = ???

    def ambientField[A](name: String)(fa: F[A]): F[A] = ???

    def ambientIndex[A](i: Int)(fa: F[A]): F[A] = ???

    def modifyError[A](f: PositionalError[C] => PositionalError[C])(fa: F[A]): F[A] = ???

    def appendMessage[A](message: String)(fa: F[A]): F[A] = ???

    def fieldName(f: FieldInfo[?]): String =
      s"'${f.alias.getOrElse(f.name)}'${f.alias.map(x => s" (alias for '$x')").mkString}"

    import alg._
    new SchemaQueryOps[F, alg.type, P, C] {
      def inFragment[A](
          fragmentName: String,
          caret: Option[C]
      )(faf: P[QA.FragmentDefinition[P]] => F[A]): F[A] =
        L.ask.flatMap[A] {
          case c if c.cycleSet(fragmentName) =>
            raise(s"Fragment by '$fragmentName' is cyclic. Hint: graphql queries must be finite.", caret)
          case _ =>
            fragments.get(fragmentName) match {
              case None    => raise(s"Unknown fragment name '$fragmentName'.", caret)
              case Some(f) => L.local(faf(f))(_ addCycle fragmentName)
            }
        }

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
            vs.zipWithIndex.parTraverse_ { case (x, i) =>
              ambientIndex(i) {
                verifyModifierStack(xs, x, ambigiousEnum)
              }
            }
          case (InverseModifier.List :: _, _) => raise(s"Expected list value for `${a.name}`, but got ${pValueName(current)}.", None)
          case (InverseModifier.Optional :: _, V.NullValue()) => F.unit
          case (InverseModifier.Optional :: xs, v)            => verifyModifierStack(xs, v, ambigiousEnum)
          case (Nil, _) =>
            val fo: F[Unit] = stack.inner.variant match {
              case SchemaAlg.InputVariant(i) =>
                current match {
                  case V.ObjectValue(vs) => checkInput(i.args, vs.toMap, ambigiousEnum)
                  case _                 => raise(s"Expected input object", None)
                }
              case SchemaAlg.ScalarVariant(s) => checkScalar(s, current)
              case SchemaAlg.EnumVariant(e) =>
                val fa: F[String] = current match {
                  case V.EnumValue(s)                    => F.pure(s)
                  case V.StringValue(s) if ambigiousEnum => F.pure(s)
                  case _ => raise(s"Enum value expected for `${e.name}`, but got ${pValueName(current)}.", None)
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
          ).flatMap(x => ambientField(k)(checkArg(a, x, ambigiousEnum)))
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
          sel.variant match {
            case SchemaAlg.TypeVariant(t) =>
              // Check downcast
              t.implements.get(name) match {
                case None =>
                  raise(s"Tried to match with type `$name` on type object type `${sel.name}`.", Some(caret))
                case Some(i) => F.pure(i.asSelectable)
              }
            case SchemaAlg.InterfaceVariant(i) =>
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
              }
            case SchemaAlg.UnionVariant(u) =>
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
          }
        }
      }

      override def collectSelectionInfo(sel: alg.Selectable, ss: QueryAst.SelectionSet[P]): F[NonEmptyList[SelectionInfo]] = D.defer {
        val all = ss.selections.map(p => (P.position(p), P(p)))
        val fields = all.collect { case (caret, QA.Selection.FieldSelection(field)) => (caret, field) }

        val actualFields = sel.fields + ("__typename" -> alg.typename)

        val validateFieldsF = fields
          .parTraverse { case (caret, field) =>
            actualFields.get(field.name) match {
              case None    => raise[FieldInfo](s"Field '${field.name}' is not a member of `${sel.name}`.", Some(caret))
              case Some(f) => ambientField(field.name)(collectFieldInfo(f, field, caret))
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
        val i: F[TypeInfo[alg.Selectable]] = qf.tpe.inner.variant match {
          case s: SchemaAlg.SelectableVariant[alg.type] =>
            raiseOpt(
              x,
              s"Field `${f.name}` of type `${name}` must have a selection set.",
              Some(c)
            ).flatMap(ss => collectSelectionInfo(alg.embedSelectable(s), ss)).map(TypeInfo.Selectable(name, _))
          case SchemaAlg.EnumVariant(_) =>
            if (x.isEmpty) F.pure(TypeInfo.Enum(name))
            else raise(s"Field `${f.name}` of enum type `${name}` must not have a selection set.", Some(c))
          case SchemaAlg.ScalarVariant(_) =>
            if (x.isEmpty)
              F.pure(TypeInfo.Scalar(name))
            else raise(s"Field `${f.name}` of scalar type `${name}` must not have a selection set.", Some(c))
        }

        verifyArgsF &> i.map(fi => FieldInfo(name, f.alias, f.arguments, qf.tpe.copy(inner = fi)))
      }

      override def variables(op: QueryAst.OperationDefinition[P]): F[VariableMap] = ???

      override def collectRoot(op: QueryAst.OperationDefinition[P], root: alg.Type): F[NonEmptyList[SelectionInfo]] = ???

      override def checkSelectionsMerge(xs: NonEmptyList[SelectionInfo]): F[Unit] = ???

      override def checkFieldsMerge(a: FieldInfo, asi: SelectionInfo, b: FieldInfo, bsi: SelectionInfo): F[Unit] = ??? /*{
    sealed trait EitherObject
    object EitherObject {
      case object FirstIsObject extends EitherObject
      case object SecondIsObject extends EitherObject
      case object NeitherIsObject extends EitherObject
      case object BothAreObjects extends EitherObject
    }
    lazy val objectPair = (asi.s, bsi.s) match {
      case (_: Type[G, ?], _: Type[G, ?]) => EitherObject.BothAreObjects
      case (_: Type[G, ?], _)             => EitherObject.FirstIsObject
      case (_, _: Type[G, ?])             => EitherObject.SecondIsObject
      case _                              => EitherObject.NeitherIsObject
    }

    val parentNameSame = asi.s.name === bsi.s.name

    lazy val aIn = s"${fieldName(a)} in type `${asi.s.name}`"
    lazy val bIn = s"${fieldName(b)} in type `${bsi.s.name}`"

    lazy val whyMerge = {
      val why1 = if (parentNameSame) Some("they have the same parent type") else None
      val why2 = objectPair match {
        case EitherObject.FirstIsObject   => Some(s"the second field ${fieldName(a)} is not an object but the first was")
        case EitherObject.SecondIsObject  => Some(s"the first field ${fieldName(b)} is not an object but the second was")
        case EitherObject.NeitherIsObject => Some(s"neither field ${fieldName(a)} nor ${fieldName(b)} are objects")
        case EitherObject.BothAreObjects  => None
      }
      List(why1, why2).collect { case Some(err) => err }.mkString(" and ") + "."
    }

    // 2. in FieldsInSetCanMerge
    val thoroughCheckF = if (parentNameSame || objectPair != EitherObject.BothAreObjects) {
      val argsF = (a.args, b.args) match {
        case (None, None)    => F.unit
        case (Some(_), None) => raise[F, Unit](s"A selection of field ${fieldName(a)} has arguments, while another doesn't.", Some(b.caret))
        case (None, Some(_)) => raise[F, Unit](s"A selection of field ${fieldName(a)} has arguments, while another doesn't.", Some(b.caret))
        case (Some(aa), Some(ba)) => compareArguments[F](fieldName(a), aa, ba, Some(b.caret))
      }

      val nameSameF =
        if (a.name === b.name) F.unit
        else {
          raise[F, Unit](
            s"Field $aIn and $bIn must have the same name (not alias) when they are merged.",
            Some(a.caret)
          )
        }

      appendMessage(s"They were merged since $whyMerge") {
        argsF &> nameSameF
      }
    } else F.unit

    // 1. in FieldsInSetCanMerge
    val shapeCheckF = checkSimplifiedTypeShape[F, G](a.tpe, b.tpe, a.caret)

    thoroughCheckF &> shapeCheckF
  }*/
      override def compareArguments(name: String, aa: QueryAst.Arguments, ba: QueryAst.Arguments, caret: Option[C]): F[Unit] = {
        def checkUniqueness(x: QA.Arguments): F[Map[String, QA.Argument]] =
          x.nel.toList
            .groupBy(_.name)
            .toList
            .parTraverse {
              case (k, v :: Nil) => F.pure(k -> v)
              case (k, _) =>
                raise[(String, QA.Argument)](s"Argument '$k' of field $name was not unique.", caret)
            }
            .map(_.toMap)

        (checkUniqueness(aa), checkUniqueness(ba)).parTupled.flatMap { case (amap, bmap) =>
          (amap align bmap).toList.parTraverse_[F, Unit] {
            case (k, Ior.Left(_)) =>
              raise(s"Field $name is already selected with argument '$k', but no argument was given here.", caret)
            case (k, Ior.Right(_)) =>
              raise(s"Field $name is already selected without argument '$k', but an argument was given here.", caret)
            case (k, Ior.Both(l, r)) => ambientField(k)(compareValues(l.value, r.value, caret))
          }
        }
      }

      override def compareValues(av: V[AnyValue], bv: V[AnyValue], caret: Option[C]): F[Unit] = {
        (av, bv) match {
          case (V.VariableValue(avv), V.VariableValue(bvv)) =>
            if (avv === bvv) F.unit
            else raise(s"Variable '$avv' and '$bvv' are not equal.", caret)
          case (V.IntValue(ai), V.IntValue(bi)) =>
            if (ai === bi) F.unit
            else raise(s"Int '$ai' and '$bi' are not equal.", caret)
          case (V.FloatValue(af), V.FloatValue(bf)) =>
            if (af === bf) F.unit
            else raise(s"Float '$af' and '$bf' are not equal.", caret)
          case (V.StringValue(as), V.StringValue(bs)) =>
            if (as === bs) F.unit
            else raise(s"String '$as' and '$bs' are not equal.", caret)
          case (V.BooleanValue(ab), V.BooleanValue(bb)) =>
            if (ab === bb) F.unit
            else raise(s"Boolean '$ab' and '$bb' are not equal.", caret)
          case (V.EnumValue(ae), V.EnumValue(be)) =>
            if (ae === be) F.unit
            else raise(s"Enum '$ae' and '$be' are not equal.", caret)
          case (V.NullValue(), V.NullValue()) => F.unit
          case (V.ListValue(al), V.ListValue(bl)) =>
            if (al.length === bl.length) {
              al.zip(bl).zipWithIndex.parTraverse_ { case ((a, b), i) => ambientIndex(i)(compareValues(a, b, caret)) }
            } else
              raise(s"Lists are not af same size. Found list of length ${al.length} versus list of length ${bl.length}.", caret)
          case (V.ObjectValue(ao), V.ObjectValue(bo)) =>
            if (ao.size =!= bo.size)
              raise(
                s"Objects are not af same size. Found object of length ${ao.size} versus object of length ${bo.size}.",
                caret
              )
            else {
              def checkUniqueness(xs: List[(String, V[AnyValue])]) =
                xs.groupMap { case (k, _) => k } { case (_, v) => v }
                  .toList
                  .parTraverse {
                    case (k, v :: Nil) => F.pure(k -> v)
                    case (k, _)        => raise[(String, V[AnyValue])](s"Key '$k' is not unique in object.", caret)
                  }
                  .map(_.toMap)

              (checkUniqueness(ao), checkUniqueness(bo)).parTupled.flatMap { case (amap, bmap) =>
                // TODO test that verifies that order does not matter
                (amap align bmap).toList.parTraverse_[F, Unit] {
                  case (k, Ior.Left(_))    => raise(s"Key '$k' is missing in object.", caret)
                  case (k, Ior.Right(_))   => raise(s"Key '$k' is missing in object.", caret)
                  case (k, Ior.Both(l, r)) => ambientField(k)(compareValues(l, r, caret))
                }
              }
            }
          case _ => raise(s"Values are not same type, got ${pValueName(av)} and ${pValueName(bv)}.", caret)
        }
      }
    }
  }
}
