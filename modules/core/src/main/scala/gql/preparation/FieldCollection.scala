/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql.preparation

import cats._
import cats.data._
import cats.implicits._
import gql.Arg
import gql.Cursor
import gql.InverseModifierStack
import gql.SchemaShape
import gql.ast._
import gql.parser.{QueryAst => QA}
import gql.parser.AnyValue
import gql.Position

class FieldCollection[F[_], C](
    implementations: SchemaShape.Implementations[F],
    fragments: Map[String, QA.FragmentDefinition[C]],
    ap: ArgParsing[C],
    da: DirectiveAlg[F, C]
) {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]

  def inFragment[A](
      fragmentName: String,
      carets: List[C]
  )(faf: QA.FragmentDefinition[C] => G[A]): G[A] =
    G.cycleAsk
      .map(_.contains(fragmentName))
      .ifM(
        G.raise(s"Fragment by '$fragmentName' is cyclic. Hint: graphql queries must be finite.", carets),
        fragments.get(fragmentName) match {
          case None    => G.raise(s"Unknown fragment name '$fragmentName'.", carets)
          case Some(f) => G.cycleOver(fragmentName, faf(f))
        }
      )

  def matchType(
      name: String,
      sel: Selectable[F, ?],
      caret: C
  ): Alg[C, Selectable[F, ?]] = {
    if (sel.name == name) G.pure(sel)
    else {
      sel match {
        case t: Type[F, ?] =>
          // Check downcast
          t.implementsMap.get(name) match {
            case None =>
              G.raise(s"Tried to match with type `$name` on type object type `${sel.name}`.", List(caret))
            case Some(i) => G.pure(i.value)
          }
        case i: Interface[F, ?] =>
          // What types implement this interface?
          // We can both downcast and up-match
          i.implementsMap.get(name) match {
            case Some(i) => G.pure(i.value)
            case None =>
              G.raiseOpt(
                implementations.get(i.name),
                s"The interface `${i.name}` is not implemented by any type.",
                List(caret)
              ).flatMap { m =>
                G.raiseOpt(
                  m.get(name).map {
                    case t: SchemaShape.InterfaceImpl.TypeImpl[F @unchecked, ?, ?]    => t.t
                    case i: SchemaShape.InterfaceImpl.OtherInterface[F @unchecked, ?] => i.i
                  },
                  s"`$name` does not implement interface `${i.name}`, possible implementations are ${m.keySet.mkString(", ")}.",
                  List(caret)
                )
              }
          }
        case u: Union[F, ?] =>
          // Can match to any type or any of it's types' interfacees
          u.instanceMap.get(name) match {
            case Some(i) => G.pure(i.tpe.value)
            case None =>
              G.raiseOpt(
                u.types.toList.map(_.tpe.value).collectFirstSome(_.implementsMap.get(name)),
                s"`$name` is not a member of the union `${u.name}` (or any of the union's types' implemented interfaces), possible members are ${u.instanceMap.keySet
                    .mkString(", ")}.",
                List(caret)
              ).map(_.value)
          }
      }
    }
  }

  def collectSelectionInfo(
      sel: Selectable[F, ?],
      ss: QA.SelectionSet[C]
  ): Alg[C, List[SelectionInfo[F, C]]] = {
    val all = ss.selections
    val fields = all.collect { case QA.Selection.FieldSelection(field, c) => (c, field) }

    val actualFields =
      sel.abstractFieldMap + ("__typename" -> AbstractField(None, Eval.now(stringScalar), None))

    val validateFieldsF = fields
      .parTraverse { case (caret, field) =>
        actualFields.get(field.name) match {
          case None    => G.raise[FieldInfo[F, C]](s"Field '${field.name}' is not a member of `${sel.name}`.", List(caret))
          case Some(f) => G.ambientField(field.name)(collectFieldInfo(f, field, caret))
        }
      }
      .map(_.toNel.toList.map(SelectionInfo(sel, _, None)))

    val realInlines =
      all
        .collect { case QA.Selection.InlineFragmentSelection(f, c) => (c, f) }
        .parFlatTraverse { case (caret, f) =>
          da
            .parseProvidedSubtype[Position.InlineFragmentSpread](f.directives, List(caret))
            .flatMap(_.foldLeftM(List(f)) { case (fs, d) => 
              fs.parFlatTraverse(f => G.raiseEither(d.p.handler(d.a, f), List(caret)))
            })
            .map(_.tupleLeft(caret))
        }
        .flatMap(_.parFlatTraverse { case (caret, f) =>
          f.typeCondition.traverse(matchType(_, sel, caret)).map(_.getOrElse(sel)).flatMap { t =>
            collectSelectionInfo(t, f.selectionSet).map(_.toList)
          }
        })

    val realFragments = all
      .collect { case QA.Selection.FragmentSpreadSelection(f, c) => (c, f) }
      .parFlatTraverse { case (caret, f) =>
        da
          .parseProvidedSubtype[Position.FragmentSpread](f.directives, List(caret))
          .flatMap(_.foldLeftM(List(f)) { case (fs, d) => 
            fs.parFlatTraverse(f => G.raiseEither(d.p.handler(d.a, f), List(caret)))
          })
          .map(_.tupleLeft(caret))
      }
      .flatMap(_.parFlatTraverse { case (caret, f) =>
        val fn = f.fragmentName
        inFragment(fn, List(caret)) { f =>
          matchType(f.typeCnd, sel, f.caret).flatMap { t =>
            collectSelectionInfo(t, f.selectionSet)
              .map(_.toList.map(_.copy(fragmentName = Some(fn))))
          }
        }
      })

    (validateFieldsF :: realInlines :: realFragments :: Nil).parFlatSequence
  }

  def collectFieldInfo(
      qf: AbstractField[F, ?],
      f: QA.Field[C],
      caret: C
  ): Alg[C, FieldInfo[F, C]] = {
    val fields = f.arguments.toList.flatMap(_.nel.toList).map(x => x.name -> x.value).toMap
    val verifyArgsF = qf.arg.parTraverse_ { case a: Arg[a] =>
      ap.decodeArg[a](a, fields.fmap(_.map(List(_))), ambigiousEnum = false, context = List(caret)).void
    }

    val c = f.caret
    val x = f.selectionSet
    val ims = InverseModifierStack.fromOut(qf.output.value)
    val tl = ims.inner
    val i: G[TypeInfo[F, C]] = tl match {
      case s: Selectable[F, ?] =>
        G.raiseOpt(
          x,
          s"Field `${f.name}` of type `${tl.name}` must have a selection set.",
          List(c)
        ).flatMap(ss => collectSelectionInfo(s, ss))
          .map(TypeInfo.Selectable(tl.name, _))
      case _: Enum[?] =>
        if (x.isEmpty) G.pure(TypeInfo.Enum(tl.name))
        else G.raise(s"Field `${f.name}` of enum type `${tl.name}` must not have a selection set.", List(c))
      case _: Scalar[?] =>
        if (x.isEmpty)
          G.pure(TypeInfo.Scalar(tl.name))
        else G.raise(s"Field `${f.name}` of scalar type `${tl.name}` must not have a selection set.", List(c))
    }

    verifyArgsF &> i.flatMap { fi =>
      G.cursorAsk.map(c => FieldInfo[F, C](f.name, f.alias, f.arguments, ims.copy(inner = fi), f.directives, caret, c))
    }
  }
}

sealed trait TypeInfo[+G[_], +C] {
  def name: String
}
object TypeInfo {
  final case class Scalar(name: String) extends TypeInfo[Nothing, Nothing]
  final case class Enum(name: String) extends TypeInfo[Nothing, Nothing]
  final case class Selectable[G[_], C](name: String, selection: List[SelectionInfo[G, C]]) extends TypeInfo[G, C]
}

final case class SelectionInfo[G[_], C](
    s: Selectable[G, ?],
    fields: NonEmptyList[FieldInfo[G, C]],
    fragmentName: Option[String]
)

final case class FieldInfo[G[_], C](
    name: String,
    alias: Option[String],
    args: Option[QA.Arguments[C, AnyValue]],
    tpe: InverseModifierStack[TypeInfo[G, C]],
    directives: Option[QA.Directives[C, AnyValue]],
    caret: C,
    path: Cursor
) {
  lazy val outputName: String = alias.getOrElse(name)
}
