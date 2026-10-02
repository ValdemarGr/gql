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
  type Collected[A] = (EitherNec[PositionalError[C], A], G[Unit])
  val G = Alg.Ops[C]

  private def capture[A](fa: G[Collected[A]]): G[Collected[A]] =
    fa.attempt.map {
      case Right(result) => result
      case Left(errors)  => (Left(errors), Alg.RaiseError(errors))
    }

  private def combine[A](results: List[Collected[List[A]]]): Collected[List[A]] =
    (results.traverse(_._1.toValidated).map(_.flatten).toEither, results.parTraverse_(_._2))

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
  ): G[Collected[List[SelectionInfo[F, C]]]] = {
    val all = ss.selections
    val fields = all.collect { case QA.Selection.FieldSelection(field, c) => (c, field) }

    val actualFields =
      sel.abstractFieldMap + ("__typename" -> AbstractField(None, Eval.now(stringScalar), None))

    val validateFieldsF = fields
      .parTraverse { case (caret, field) =>
        actualFields.get(field.name) match {
          case None => capture(G.raise[Collected[FieldInfo[F, C]]](s"Field '${field.name}' is not a member of `${sel.name}`.", List(caret)))
          case Some(f) => G.ambientField(field.name)(collectFieldInfo(f, field, caret))
        }
      }
      .map { results =>
        val shape = results.traverse(_._1.toValidated).map(_.toNel.toList.map(SelectionInfo(sel, _, None))).toEither
        (shape, results.parTraverse_(_._2))
      }

    val realInlines =
      capture(
        all
          .collect { case QA.Selection.InlineFragmentSelection(f, c) => (c, f) }
          .parFlatTraverse { case (caret, f) =>
            da.foldDirectives[Position.InlineFragmentSpread](f.directives, List(caret))(f) {
              case (f, p: Position.InlineFragmentSpread[a], d) =>
                da.parseArg(p, d.arguments, List(caret)).map(p.handler(_, f)).flatMap(G.raiseEither(_, List(caret)))
            }.map(_ tupleLeft caret)
          }
          .flatMap(_.parTraverse { case (caret, f) =>
            capture(f.typeCondition.traverse(matchType(_, sel, caret)).map(_.getOrElse(sel)).flatMap { t =>
              collectSelectionInfo(t, f.selectionSet)
            })
          }.map(combine(_)))
      )

    val realFragments = capture(
      all
        .collect { case QA.Selection.FragmentSpreadSelection(f, c) => (c, f) }
        .parFlatTraverse { case (caret, f) =>
          da.foldDirectives[Position.FragmentSpread](f.directives, List(caret))(f) { case (f, p: Position.FragmentSpread[a], d) =>
            da.parseArg(p, d.arguments, List(caret)).map(p.handler(_, f)).flatMap(G.raiseEither(_, List(caret)))
          }.map(_ tupleLeft caret)
        }
        .flatMap(_.parTraverse { case (caret, f) =>
          val fn = f.fragmentName
          capture(inFragment(fn, List(caret)) { f =>
            matchType(f.typeCnd, sel, f.caret).flatMap { t =>
              collectSelectionInfo(t, f.selectionSet)
                .map { case (shape, validation) => (shape.map(_.map(_.copy(fragmentName = Some(fn)))), validation) }
            }
          })
        }.map(combine(_)))
    )

    (validateFieldsF :: realInlines :: realFragments :: Nil).parSequence.map(combine(_))
  }

  def collectFieldInfo(
      qf: AbstractField[F, ?],
      f: QA.Field[C],
      caret: C
  ): G[Collected[FieldInfo[F, C]]] = {
    val fields = f.arguments.toList.flatMap(_.nel.toList).map(x => x.name -> x.value.map(List(_)))
    val verifyArgsF = qf.arg.parTraverse_ { case a: Arg[a] =>
      ap.decodeArg[a](a, fields, ambigiousEnum = false, context = List(caret)).void
    }

    val c = f.caret
    val x = f.selectionSet
    val ims = InverseModifierStack.fromOut(qf.output.value)
    val tl = ims.inner
    val i: G[Collected[TypeInfo[F, C]]] = capture(tl match {
      case s: Selectable[F, ?] =>
        G.raiseOpt(
          x,
          s"Field `${f.name}` of type `${tl.name}` must have a selection set.",
          List(c)
        ).flatMap(ss => collectSelectionInfo(s, ss))
          .map { case (shape, validation) => (shape.map(TypeInfo.Selectable(tl.name, _)), validation) }
      case _: Enum[?] =>
        if (x.isEmpty) G.pure((Right(TypeInfo.Enum(tl.name)), G.unit))
        else G.raise(s"Field `${f.name}` of enum type `${tl.name}` must not have a selection set.", List(c))
      case _: Scalar[?] =>
        if (x.isEmpty)
          G.pure((Right(TypeInfo.Scalar(tl.name)), G.unit))
        else G.raise(s"Field `${f.name}` of scalar type `${tl.name}` must not have a selection set.", List(c))
    })

    val validationF = G.pause(verifyArgsF)

    (validationF, i).parTupled.flatMap { case (validation, (shape, nested)) =>
      G.cursorAsk.map { path =>
        (
          shape.map(fi => FieldInfo[F, C](f.name, f.alias, f.arguments, ims.copy(inner = fi), f.directives, caret, path)),
          validation &> nested
        )
      }
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
