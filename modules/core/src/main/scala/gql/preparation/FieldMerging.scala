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

import cats.data._
import cats.implicits._
import gql.InverseModifierStack
import gql.ast._
import gql.parser.AnyValue
import gql.parser.{QueryAst => QA}
import gql.parser.{Value => V}

class FieldMerging[C] {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]

  def checkSelectionsMerge[F[_]](xs: NonEmptyList[SelectionInfo[F, C]]): G[Unit] = {
    val ys: NonEmptyList[NonEmptyList[(SelectionInfo[F, C], FieldInfo[F, C])]] =
      xs.flatMap(si => si.fields tupleLeft si)
        .groupByNem { case (_, f) => f.outputName }
        .toNel
        .map { case (_, v) => v }

    ys.parTraverse_ { zs =>
      // TODO partition into what should be fullchecked and what should be structural
      val mergeFieldsF = {
        val (siHead, fiHead) = zs.head
        zs.tail.parTraverse_ { case (si, fi) => checkFieldsMerge(fiHead, siHead, fi, si) }
      }

      mergeFieldsF >>
        zs.toList
          .map { case (_, fi) => fi.tpe.inner }
          .collect { case s: TypeInfo.Selectable[F, C] => s.selection.toList }
          .flatten
          .toNel
          .traverse_(checkSelectionsMerge)
    }
  }

  // Optimization: we don't check selections recursively since checkSelectionsMerge traverses the whole tree
  // We only need to check the immidiate children and will eventually have checked the whole tree
  def checkSimplifiedTypeShape[F[_]](
      a: InverseModifierStack[TypeInfo[F, C]],
      b: InverseModifierStack[TypeInfo[F, C]],
      caret: C
  ): G[Unit] = {
    (a.inner, b.inner) match {
      // It turns out we don't care if more fields are selected in one object than the other
      case (TypeInfo.Selectable(_, _), TypeInfo.Selectable(_, _)) => G.unit
      // case (SimplifiedType.Selectable(_, l), SimplifiedType.Selectable(_, r)) => F.unit
      // val lComb = l.flatMap(x => x.fields tupleLeft x).groupByNem { case (_, f) => f.outputName }
      // val rComb = r.flatMap(x => x.fields tupleLeft x).groupByNem { case (_, f) => f.outputName }
      // (lComb align rComb).toNel.parTraverse_ {
      //   case (_, Ior.Both(_, _)) => F.unit
      //   case (k, Ior.Left(_))    => raise(s"Field '$k' was missing when verifying shape equivalence.", Some(caret))
      //   case (k, Ior.Right(_))   => raise(s"Field '$k' was missing when verifying shape equivalence.", Some(caret))
      // }
      case (TypeInfo.Enum(l), TypeInfo.Enum(r)) =>
        if (l === r) G.unit
        else G.raise(s"Enums are not the same, got '$l' and '$r'.", List(caret))
      case (TypeInfo.Scalar(l), TypeInfo.Scalar(r)) =>
        if (l === r) G.unit
        else G.raise(s"Scalars are not the same, got '$l' and '$r'.", List(caret))
      case _ =>
        G.raise(s"Types are not the same, got `${a.invert.show(_.name)}` and `${b.invert.show(_.name)}`.", List(caret))
    }
  }

  def checkFieldsMerge[F[_]](
      a: FieldInfo[F, C],
      asi: SelectionInfo[F, C],
      b: FieldInfo[F, C],
      bsi: SelectionInfo[F, C]
  ): G[Unit] = {
    sealed trait EitherObject
    object EitherObject {
      case object FirstIsObject extends EitherObject
      case object SecondIsObject extends EitherObject
      case object NeitherIsObject extends EitherObject
      case object BothAreObjects extends EitherObject
    }
    lazy val objectPair = (asi.s, bsi.s) match {
      case (_: Type[F, ?], _: Type[F, ?]) => EitherObject.BothAreObjects
      case (_: Type[F, ?], _)             => EitherObject.FirstIsObject
      case (_, _: Type[F, ?])             => EitherObject.SecondIsObject
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
        case (None, None)         => G.unit
        case (Some(_), None)      => G.raise(s"A selection of field ${fieldName(a)} has arguments, while another doesn't.", List(b.caret))
        case (None, Some(_))      => G.raise(s"A selection of field ${fieldName(a)} has arguments, while another doesn't.", List(b.caret))
        case (Some(aa), Some(ba)) => compareArguments(fieldName(a), aa, ba, Some(b.caret))
      }

      val nameSameF =
        if (a.name === b.name) G.unit
        else {
          G.raise(
            s"Field $aIn and $bIn must have the same name (not alias) when they are merged.",
            List(a.caret)
          )
        }

      G.appendMessage(s"They were merged since $whyMerge") {
        argsF &> nameSameF
      }
    } else G.unit

    // 1. in FieldsInSetCanMerge
    val shapeCheckF = checkSimplifiedTypeShape(a.tpe, b.tpe, a.caret)

    thoroughCheckF &> shapeCheckF

  }

  // These technically don't need to be in the trait, but it's convenient because of error handling
  // If needed, they can always be moved
  def compareArguments(name: String, aa: QA.Arguments[C, AnyValue], ba: QA.Arguments[C, AnyValue], caret: Option[C]): G[Unit] = {
    def checkUniqueness(x: QA.Arguments[C, AnyValue]): G[Map[String, QA.Argument[C, AnyValue]]] =
      x.nel.toList
        .groupBy(_.name)
        .toList
        .parTraverse {
          case (k, v :: Nil) => G.pure(k -> v)
          case (k, _) =>
            G.raise[(String, QA.Argument[C, AnyValue])](s"Argument '$k' of field $name was not unique.", caret.toList)
        }
        .map(_.toMap)

    (checkUniqueness(aa), checkUniqueness(ba)).parTupled.flatMap { case (amap, bmap) =>
      (amap align bmap).toList.parTraverse_[G, Unit] {
        case (k, Ior.Left(_)) =>
          G.raise(s"Field $name is already selected with argument '$k', but no argument was given here.", caret.toList)
        case (k, Ior.Right(_)) =>
          G.raise(s"Field $name is already selected without argument '$k', but an argument was given here.", caret.toList)
        case (k, Ior.Both(l, r)) => G.ambientField(k)(compareValues(l.value, r.value, caret))
      }
    }
  }

  def compareValues(av: V[AnyValue, C], bv: V[AnyValue, C], caret: Option[C]): G[Unit] = {
    val cs = av.c :: bv.c :: caret.toList
    (av, bv) match {
      case (V.VariableValue(avv, _), V.VariableValue(bvv, _)) =>
        if (avv === bvv) G.unit
        else G.raise(s"Variable '$avv' and '$bvv' are not equal.", cs)
      case (V.IntValue(ai, _), V.IntValue(bi, _)) =>
        if (ai === bi) G.unit
        else G.raise(s"Int '$ai' and '$bi' are not equal.", cs)
      case (V.FloatValue(af, _), V.FloatValue(bf, _)) =>
        if (af === bf) G.unit
        else G.raise(s"Float '$af' and '$bf' are not equal.", cs)
      case (V.StringValue(as, _), V.StringValue(bs, _)) =>
        if (as === bs) G.unit
        else G.raise(s"String '$as' and '$bs' are not equal.", cs)
      case (V.BooleanValue(ab, _), V.BooleanValue(bb, _)) =>
        if (ab === bb) G.unit
        else G.raise(s"Boolean '$ab' and '$bb' are not equal.", cs)
      case (V.EnumValue(ae, _), V.EnumValue(be, _)) =>
        if (ae === be) G.unit
        else G.raise(s"Enum '$ae' and '$be' are not equal.", cs)
      case (V.NullValue(_), V.NullValue(_)) => G.unit
      case (V.ListValue(al, _), V.ListValue(bl, _)) =>
        if (al.length === bl.length) {
          al.zip(bl).zipWithIndex.parTraverse_ { case ((a, b), i) => G.ambientIndex(i)(compareValues(a, b, caret)) }
        } else
          G.raise(s"Lists are not af same size. Found list of length ${al.length} versus list of length ${bl.length}.", cs)
      case (V.ObjectValue(ao, _), V.ObjectValue(bo, _)) =>
        if (ao.size =!= bo.size)
          G.raise(
            s"Objects are not af same size. Found object of length ${ao.size} versus object of length ${bo.size}.",
            cs
          )
        else {
          def checkUniqueness(xs: List[(String, V[AnyValue, C])]) =
            xs.groupMap { case (k, _) => k } { case (_, v) => v }
              .toList
              .parTraverse {
                case (k, v :: Nil) => G.pure(k -> v)
                case (k, _)        => G.raise[(String, V[AnyValue, C])](s"Key '$k' is not unique in object.", cs)
              }
              .map(_.toMap)

          (checkUniqueness(ao), checkUniqueness(bo)).parTupled.flatMap { case (amap, bmap) =>
            // TODO test that verifies that order does not matter
            (amap align bmap).toList.parTraverse_[G, Unit] {
              case (k, Ior.Left(_))    => G.raise(s"Key '$k' is missing in object.", cs)
              case (k, Ior.Right(_))   => G.raise(s"Key '$k' is missing in object.", cs)
              case (k, Ior.Both(l, r)) => G.ambientField(k)(compareValues(l, r, caret))
            }
          }
        }
      case _ => G.raise(s"Values are not the same type, got ${pValueName(av)} and ${pValueName(bv)}.", cs)
    }
  }
}
