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
import gql.SchemaShape
import gql.ModifierStack
import gql.Cursor

trait FieldCollection[F[_], G[_], P[_], C] {
  def matchType(
      name: String,
      sel: Selectable[G, ?],
      caret: C
  ): F[Selectable[G, ?]]

  def collectSelectionInfo(
      sel: Selectable[G, ?],
      ss: QA.SelectionSet[P]
  ): F[NonEmptyList[SelectionInfo[G, C]]]

  def collectFieldInfo(
      qf: AbstractField[G, ?],
      f: QA.Field[P],
      caret: C
  ): F[FieldInfo[G, C]]
}

object FieldCollection {
  type CycleSet = Set[String]

  def apply[F[_]: Parallel, G[_], P[_], C](
      implementations: SchemaShape.Implementations[G],
      fragments: Map[String, P[QA.FragmentDefinition[P]]]
  )(implicit
      F: Monad[F],
      E: ErrorAlg[F, C],
      PA: PathAlg[F],
      L: Local[F, CycleSet],
      C: Local[F, Cursor],
      P: Positioned[P, C],
      A: ArgParsing[F]
  ) = {
    import E._
    import PA._

    new FieldCollection[F, G, P, C] {
      def inFragment[A](
          fragmentName: String,
          carets: List[C]
      )(faf: P[QA.FragmentDefinition[P]] => F[A]): F[A] =
        L.ask[CycleSet]
          .map(_.contains(fragmentName))
          .ifM(
            raise(s"Fragment by '$fragmentName' is cyclic. Hint: graphql queries must be finite.", carets),
            fragments.get(fragmentName) match {
              case None    => raise(s"Unknown fragment name '$fragmentName'.", carets)
              case Some(f) => L.local(faf(f))(_ + fragmentName)
            }
          )

      override def matchType(name: String, sel: Selectable[G, ?], caret: C): F[Selectable[G, ?]] = {
        if (sel.name == name) F.pure(sel)
        else {
          sel match {
            case t: Type[G, ?] =>
              // Check downcast
              t.implementsMap.get(t.name) match {
                case None =>
                  raise(s"Tried to match with type `$name` on type object type `${sel.name}`.", List(caret))
                case Some(i) => F.pure(i.value)
              }
            case i: Interface[G, ?] =>
              // What types implement this interface?
              // We can both downcast and up-match
              i.implementsMap.get(name) match {
                case Some(i) => F.pure(i.value)
                case None =>
                  raiseOpt(
                    implementations.get(i.name),
                    s"The interface `${i.name}` is not implemented by any type.",
                    List(caret)
                  ).flatMap { m =>
                    raiseOpt(
                      m.get(name).map {
                        case t: SchemaShape.InterfaceImpl.TypeImpl[G @unchecked, ?, ?]    => t.t
                        case i: SchemaShape.InterfaceImpl.OtherInterface[G @unchecked, ?] => i.i
                      },
                      s"`$name` does not implement interface `${i.name}`, possible implementations are ${m.keySet.mkString(", ")}.",
                      List(caret)
                    )
                  }
              }
            case u: Union[G, ?] =>
              // Can match to any type or any of it's types' interfacees
              u.instanceMap.get(name) match {
                case Some(i) => F.pure(i.tpe.value)
                case None =>
                  raiseOpt(
                    u.types.toList.map(_.tpe.value).collectFirstSome(_.implementsMap.get(name)),
                    s"`$name` is not a member of the union `${u.name}` (or any of the union's types' implemented interfaces), possible members are ${u.instanceMap.keySet
                      .mkString(", ")}.",
                    List(caret)
                  ).map(_.value)
              }
          }
        }
      }

      override def collectSelectionInfo(sel: Selectable[G, ?], ss: QueryAst.SelectionSet[P]): F[NonEmptyList[SelectionInfo[G, C]]] = {
        val all = ss.selections.map(p => (P.position(p), P(p)))
        val fields = all.collect { case (caret, QA.Selection.FieldSelection(field)) => (caret, field) }

        val actualFields =
          sel.abstractFieldMap + ("__typename" -> AbstractField(None, Eval.now(stringScalar), None))

        val validateFieldsF = fields
          .parTraverse { case (caret, field) =>
            actualFields.get(field.name) match {
              case None    => raise[FieldInfo[G, C]](s"Field '${field.name}' is not a member of `${sel.name}`.", List(caret))
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
            inFragment(fn, List(caret)) { p =>
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

      override def collectFieldInfo(qf: AbstractField[G, _], f: QueryAst.Field[P], caret: C): F[FieldInfo[G, C]] = {
        val fields = f.arguments.toList.flatMap(_.nel.toList).map(x => x.name -> x.value).toMap
        val verifyArgsF = qf.arg.traverse_ { case a: Arg[a] => A.decodeArg[a](a, fields, ambigiousEnum = false).void }

        val c = P.position(f.selectionSet)
        val x = P(f.selectionSet)
        val ims = InverseModifierStack.fromOut(qf.output.value)
        val tl = ims.inner
        val i: F[TypeInfo[G, C]] = tl match {
          case s: Selectable[G, ?] =>
            raiseOpt(
              x,
              s"Field `${f.name}` of type `${tl.name}` must have a selection set.",
              List(c)
            ).flatMap(ss => collectSelectionInfo(s, ss)).map(TypeInfo.Selectable(tl.name, _))
          case _: Enum[?] =>
            if (x.isEmpty) F.pure(TypeInfo.Enum(tl.name))
            else raise(s"Field `${f.name}` of enum type `${tl.name}` must not have a selection set.", List(c))
          case _: Scalar[?] =>
            if (x.isEmpty)
              F.pure(TypeInfo.Scalar(tl.name))
            else raise(s"Field `${f.name}` of scalar type `${tl.name}` must not have a selection set.", List(c))
        }

        verifyArgsF &> i.flatMap(fi => C.ask.map(c => FieldInfo[G, C](tl.name, f.alias, f.arguments, ims.copy(inner = fi), caret, c)))
      }
    }
  }
}

trait Positioned[P[_], C] {
  def apply[A](p: P[A]): A

  def position[A](p: P[A]): C
}

object Positioned {
  def apply[P[_], C](position: P[?] => C)(get: P ~> Id): Positioned[P, C] = {
    val p0 = position
    new Positioned[P, C] {
      def apply[A](p: P[A]): A = get(p)
      def position[A](p: P[A]): C = p0(p)
    }
  }
}

sealed trait TypeInfo[+G[_], +C] {
  def name: String
}
object TypeInfo {
  final case class Scalar(name: String) extends TypeInfo[Nothing, Nothing]
  final case class Enum(name: String) extends TypeInfo[Nothing, Nothing]
  final case class Selectable[G[_], C](name: String, selection: NonEmptyList[SelectionInfo[G, C]]) extends TypeInfo[G, C]
}

final case class SelectionInfo[G[_], C](
    s: Selectable[G, ?],
    fields: NonEmptyList[FieldInfo[G, C]],
    fragmentName: Option[String]
)

final case class FieldInfo[G[_], C](
    name: String,
    alias: Option[String],
    args: Option[QA.Arguments],
    tpe: InverseModifierStack[TypeInfo[G, C]],
    caret: C,
    path: Cursor
) {
    lazy val outputName: String = alias.getOrElse(name)
}
