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
package gql

import cats.implicits._
import cats.data._
import cats.mtl._
import cats._
import io.circe._
import gql.parser.{QueryAst => QA, Value => V, NonVar, Type => T, Pos, AnyValue, Const}
import gql.ast._
import gql.resolver._
import cats.parse.Caret

object PreparedQuery {
  sealed trait PreparedField[F[_], A] extends Product with Serializable

  sealed trait PreparedStep[F[_], -I, +O] extends Product with Serializable
  object PreparedStep {
    final case class Lift[F[_], I, O](f: I => O) extends AnyRef with PreparedStep[F, I, O]
    final case class EmbedEffect[F[_], I](stableUniqueEdgeName: UniqueEdgeCursor) extends AnyRef with PreparedStep[F, F[I], I]
    final case class EmbedStream[F[_], I](signal: Boolean, stableUniqueEdgeName: UniqueEdgeCursor)
        extends AnyRef
        with PreparedStep[F, fs2.Stream[F, I], I]
    final case class EmbedError[F[_], I]() extends AnyRef with PreparedStep[F, Ior[String, I], I]
    final case class Compose[F[_], I, A, O](left: PreparedStep[F, I, A], right: PreparedStep[F, A, O])
        extends AnyRef
        with PreparedStep[F, I, O]
    final case class GetMeta[F[_], I](meta: PreparedMeta) extends AnyRef with PreparedStep[F, I, Meta]
    final case class First[F[_], I, O, C](step: PreparedStep[F, I, O]) extends AnyRef with PreparedStep[F, (I, C), (O, C)]
    final case class Batch[F[_], K, V](id: Step.BatchKey[K, V], globalEdgeId: UniqueBatchInstance[K, V])
        extends AnyRef
        with PreparedStep[F, Set[K], Map[K, V]]
    final case class Choose[F[_], A, B, C, D](
        fac: PreparedStep[F, A, C],
        fbc: PreparedStep[F, B, D]
    ) extends PreparedStep[F, Either[A, B], Either[C, D]]
  }

  sealed trait Prepared[F[_], I]

  final case class PreparedCont[F[_], I, A](
      edges: PreparedStep[F, I, A],
      cont: Prepared[F, A]
  )

  final case class Selection[F[_], I](fields: NonEmptyList[PreparedField[F, I]]) extends Prepared[F, I]

  final case class PreparedList[F[_], A, C, B](of: PreparedCont[F, A, B], toSeq: C => Seq[A]) extends Prepared[F, C]

  final case class PreparedOption[F[_], I, O](of: PreparedCont[F, I, O]) extends Prepared[F, Option[I]]

  final case class PreparedLeaf[F[_], I](name: String, encode: I => Json) extends Prepared[F, I]

  final case class PreparedDataField[F[_], A](
      name: String,
      alias: Option[String],
      cont: PreparedCont[F, A, ?]
  ) extends PreparedField[F, A] {
    lazy val outputName = alias.getOrElse(name)
  }

  final case class PreparedSpecification[F[_], I, A](
      typename: String,
      specify: I => Option[A],
      selection: NonEmptyList[PreparedDataField[F, A]]
  ) extends PreparedField[F, I]

  final case class PreparedMeta(
      variables: VariableMap,
      alias: Option[String],
      args: Option[QA.Arguments]
  )

  final case class UniqueBatchInstance[K, V](id: Int) extends AnyVal

  final case class UniqueEdgeCursor(path: NonEmptyChain[String]) {
    def append(name: String): UniqueEdgeCursor = UniqueEdgeCursor(path append name)

    lazy val asString: String = path.mkString_(".")
  }

  object UniqueEdgeCursor {
    def apply(name: String): UniqueEdgeCursor = UniqueEdgeCursor(NonEmptyChain.one(name))
  }

  final case class PositionalError[P](position: Cursor, ps: List[P], message: String)

  object PositionalError {
    import io.circe.syntax._
    implicit val encoder: Encoder.AsObject[PositionalError[Caret]] = Encoder.AsObject.instance[PositionalError[Caret]] { pe =>
      Map(
        "message" -> Some(pe.message.asJson),
        "locations" -> pe.ps.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
        "path" -> NonEmptyChain.fromChain(pe.position.path.map(_.asString)).map(_.asJson)
      ).collect { case (k, Some(v)) => k -> v }.asJsonObject
    }
  }

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

  type UsedArgs = Set[String]
  type Used[F[_], A] = WriterT[F, UsedArgs, A]
  object Used {
    def apply[F[_], C](implicit F: MonadError[F, NonEmptyChain[PositionalError[C]]]) =
      MonadError[Used[F, *], NonEmptyChain[PositionalError[C]]]
    def liftF[F[_]: Applicative, A](fa: F[A]) = WriterT.liftF[F, UsedArgs, A](fa)
  }
  def compileStep[F[_]: Parallel, G[_], C, I, O](step: Step[G, I, O], cursor: UniqueEdgeCursor, meta: PreparedMeta)(implicit
      L: Local[F, Prep],
      S: Stateful[F, Int],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]]
  ): Used[F, PreparedStep[G, I, O]] = {
    def pure[A](a: A): Used[F, A] =
      Used[F, C].pure(a)

    def rec[I2, O2](step: Step[G, I2, O2], edge: String): Used[F, PreparedStep[G, I2, O2]] =
      compileStep[F, G, C, I2, O2](step, cursor append edge, meta)

    step match {
      case Step.Alg.Lift(f)      => pure(PreparedStep.Lift(f))
      case Step.Alg.EmbedError() => pure(PreparedStep.EmbedError[G, O]())
      case alg: Step.Alg.Compose[?, i, a, o] =>
        val left = rec[i, a](alg.left, "compose-left")
        val right = rec[a, o](alg.right, "compose-right")
        (left, right).parMapN((l, r) => PreparedStep.Compose[G, i, a, o](l, r))
      case _: Step.Alg.EmbedEffect[?, i]   => pure(PreparedStep.EmbedEffect[G, i](cursor))
      case alg: Step.Alg.EmbedStream[?, i] => pure(PreparedStep.EmbedStream[G, i](alg.signal, cursor))
      case alg: Step.Alg.Choose[?, a, b, c, d] =>
        val left = rec[a, c](alg.fac, "choice-left")
        val right = rec[b, d](alg.fab, "choice-right")
        (left, right).parMapN((l, r) => PreparedStep.Choose[G, a, b, c, d](l, r))
      case Step.Alg.GetMeta() => pure(PreparedStep.GetMeta(meta))
      case alg: Step.Alg.Batch[?, k, v] =>
        Used.liftF {
          S.inspect[PreparedStep[G, I, O]] { i =>
            PreparedStep.Batch[G, k, v](alg.id, UniqueBatchInstance(i))
          } <* S.modify(_ + 1)
        }
      case alg: Step.Alg.First[?, i, o, c] =>
        rec[i, o](alg.step, "first").map(s => PreparedStep.First[G, i, o, c](s))
      case alg: Step.Alg.Argument[?, a] =>
        Used
          .liftF(decodeFieldArgs[F, G, C, a](alg.arg, meta.args, meta.variables))
          .map[PreparedStep[G, I, O]](o => PreparedStep.Lift[G, I, O](_ => o)) <*
          WriterT.tell(alg.arg.entries.map(_.name).toList.toSet)
    }
  }

  def collectFields[G[_]](step: Step[G, ?, ?]): Chain[Arg[Any]] =
    step match {
      case Step.Alg.Argument(a)   => Chain.one(a)
      case Step.Alg.First(s)      => collectFields(s)
      case Step.Alg.Choose(l, r)  => collectFields(l) ++ collectFields(r)
      case Step.Alg.Compose(l, r) => collectFields(l) ++ collectFields(r)
      case _                      => Chain.empty
    }

  def friendlyName[G[_], A](ot: Out[G, A]): String =
    ModifierStack
      .fromOut(ot)
      .show(_.name)

  def raise[F[_], C, A](s: String, caret: Option[C])(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]]
  ): F[A] =
    L.ask.map(state => NonEmptyChain.one(PositionalError(state.cursor, caret.toList, s))).flatMap(F.raiseError[A])

  def raiseOpt[F[_], C, A](o: Option[A], s: String, caret: Option[C])(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]]
  ): F[A] =
    o.map(_.pure[F]).getOrElse(raise[F, C, A](s, caret))

  def raiseEither[F[_], C, A](e: Either[String, A], caret: Option[C])(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]]
  ): F[A] =
    e match {
      case Left(value)  => raise[F, C, A](value, caret)
      case Right(value) => F.pure(value)
    }

  def ambientEdge[F[_], A](edge: GraphArc)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    L.local(fa)(_ addEdge edge)

  def ambientField[F[_], A](name: String)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    ambientEdge[F, A](GraphArc.Field(name))(fa)

  def ambientIndex[F[_], A](i: Int)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    ambientEdge[F, A](GraphArc.Index(i))(fa)

  def modifyError[F[_], C, A](f: PositionalError[C] => PositionalError[C])(fa: F[A])(implicit F: MonadError[F, NonEmptyChain[PositionalError[C]]]) =
    F.adaptError(fa)(_.map(f))

  def appendMessage[F[_], C, A](message: String)(fa: F[A])(implicit F: MonadError[F, NonEmptyChain[PositionalError[C]]]) =
    modifyError[F, C, A](d => d.copy(message = d.message + "\n" + message))(fa)

  def typenameField[A](typename: String) = {
    import gql.dsl._
    lift[fs2.Pure, A](_ => typename)
  }

  def inFragment[F[_], P[_], C, A](
      fragmentName: String,
      fragments: Map[String, P[QA.FragmentDefinition[P]]],
      caret: Option[C]
  )(
      faf: P[QA.FragmentDefinition[P]] => F[A]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F]
  ): F[A] =
    D.defer {
      L.ask.flatMap[A] {
        case c if c.cycleSet(fragmentName) =>
          raise(s"Fragment by '$fragmentName' is cyclic. Hint: graphql queries must be finite.", caret)
        case _ =>
          fragments.get(fragmentName) match {
            case None    => raise(s"Unknown fragment name '$fragmentName'.", caret)
            case Some(f) => L.local(faf(f))(_ addCycle fragmentName)
          }
      }
    }

  sealed trait SimplifiedType[G[_], C] { def selections: List[SelectionInfo[G, C]] }
  object SimplifiedType {
    final case class List[G[_], C](t: SimplifiedType[G, C]) extends SimplifiedType[G, C] { def selections = t.selections }
    final case class Option[G[_], C](t: SimplifiedType[G, C]) extends SimplifiedType[G, C] { def selections = t.selections }
    final case class Scalar[G[_], C](name: String) extends SimplifiedType[G, C] { def selections = Nil }
    final case class Enum[G[_], C](name: String) extends SimplifiedType[G, C] { def selections = Nil }
    final case class Selectable[G[_], C](name: String, selection: NonEmptyList[SelectionInfo[G, C]]) extends SimplifiedType[G, C] {
      def selections = selection.toList
    }
  }

  def getSimplifiedTypeString[G[_], C](st: SimplifiedType[G, C], inOption: Boolean = false): String = {
    val optPart = if (inOption) "" else "!"
    val n = st match {
      case SimplifiedType.List(t)             => s"[${getSimplifiedTypeString(t)}]"
      case SimplifiedType.Option(t)           => getSimplifiedTypeString(t, inOption = true)
      case SimplifiedType.Scalar(name)        => name
      case SimplifiedType.Enum(name)          => name
      case SimplifiedType.Selectable(name, _) => name
    }
    n + optPart
  }

  final case class FieldInfo[G[_], C](
      name: String,
      alias: Option[String],
      args: Option[QA.Arguments],
      tpe: SimplifiedType[G, C],
      caret: Option[C],
      path: Cursor
  ) {
    lazy val outputName: String = alias.getOrElse(name)
  }

  object FieldInfo {
    def apply[F[_]: Functor, G[_], C](name: String, alias: Option[String], args: Option[QA.Arguments], tpe: SimplifiedType[G, C], caret: Option[C])(
        implicit L: Local[F, Prep]
    ): F[FieldInfo[G, C]] =
      L.ask.map(_.cursor).map(FieldInfo(name, alias, args, tpe, caret, _))
  }

  def collectFieldInfo[F[_]: Parallel, G[_], P[_], C](
      af: AbstractField[G, ?],
      f: QA.Field[P],
      caret: Option[C],
      variableMap: VariableMap,
      fragments: Map[String, P[QA.FragmentDefinition[P]]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F],
      P: Positional[P, C]
  ): F[FieldInfo[G, C]] = ambientField(f.name) {
    // Verify arguments by decoding them
    val decF = af.arg.traverse_ { case a: Arg[a] => decodeFieldArgs[F, G, C, a](a, f.arguments, variableMap).void }

    // Verify subselection
    def verifySubsel(t: Out[G, ?]): F[SimplifiedType[G, C]] =
      (t, P(f.selectionSet)) match {
        case (arr: OutArr[G, ?, ?, ?], _) => verifySubsel(arr.of).map(SimplifiedType.List(_))
        case (opt: OutOpt[G, ?, ?], _)    => verifySubsel(opt.of).map(SimplifiedType.Option(_))
        case (ol: Selectable[G, ?], Some(ss)) =>
          collectSelectionInfo[F, G, P, C](ol, ss, variableMap, fragments, discoveryState)
            .map(xs => SimplifiedType.Selectable(ol.name, xs))
        case (e: Enum[?], None)   => F.pure(SimplifiedType.Enum(e.name))
        case (s: Scalar[?], None) => F.pure(SimplifiedType.Scalar(s.name))
        case (o, Some(_))         => raise(s"Type `${friendlyName(o)}` cannot have selections.", P.extract(f.selectionSet).some)
        case (o, None)            => raise(s"Object like type `${friendlyName(o)}` must have a selection.", P.extract(f.selectionSet).some)
      }

    decF &> verifySubsel(af.output.value).flatMap(FieldInfo[F, G, C](f.name, f.alias, f.arguments, _, caret))
  }

  final case class SelectionInfo[G[_], C](
      s: Selectable[G, ?],
      fields: NonEmptyList[FieldInfo[G, C]],
      fragmentName: Option[String]
  )
  def collectSelectionInfo[F[_]: Parallel, G[_], P[_], C](
      s: Selectable[G, ?],
      ss: QA.SelectionSet[P],
      variableMap: VariableMap,
      fragments: Map[String, P[QA.FragmentDefinition[P]]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F],
      P: Positional[P, C]
  ): F[NonEmptyList[SelectionInfo[G, C]]] = D.defer {
    val posed = ss.selections.map(p => (P.extract(p), P(p)))
    val fields = posed.collect{ case (c, QA.Selection.FieldSelection(f)) => c -> f }

    val actualFields: Map[String, AbstractField[G, ?]] =
      s.abstractFieldMap + ("__typename" -> AbstractField(None, Eval.now(stringScalar), None))

    val validateFieldsF: F[List[SelectionInfo[G, C]]] = fields
      .parTraverse { case (caret, field) =>
        actualFields.get(field.name) match {
          case None    => raise[F, C, FieldInfo[G, C]](s"Field '${field.name}' is not a member of `${s.name}`.", caret.some)
          case Some(f) => collectFieldInfo[F, G, P, C](f, field, caret.some, variableMap, fragments, discoveryState)
        }
      }
      .map(_.toNel.toList.map(SelectionInfo(s, _, None)))

    val realInlines =
      posed
        .collect { case (c, QA.Selection.InlineFragmentSelection(f)) => (c, f) }
        .parFlatTraverse { case (caret, f) =>
          f.typeCondition.traverse(matchType[F, G, P, C](_, s, caret.some, discoveryState)).map(_.getOrElse(s)).flatMap { t =>
            collectSelectionInfo[F, G, P, C](t, f.selectionSet, variableMap, fragments, discoveryState).map(_.toList)
          }
        }

    val realFragments =
      posed
        .collect { case (c, QA.Selection.FragmentSpreadSelection(f)) => (c, f) }
        .parFlatTraverse { case (caret, f) =>
          val fn = f.fragmentName
          inFragment(fn, fragments, caret.some) { p =>
            val f = P(p)
            matchType[F, G, P, C](f.typeCnd, s, P.extract(p).some, discoveryState).flatMap { t =>
              collectSelectionInfo[F, G, P, C](t, f.selectionSet, variableMap, fragments, discoveryState)
                .map(_.toList.map(_.copy(fragmentName = Some(fn))))
            }
          }
        }

    (validateFieldsF :: realInlines :: realFragments :: Nil).parFlatSequence
      // Unfortunate, but is always safe here since nel is input
      .map(_.toNel.get)
  }

  def fieldName[G[_], C](f: FieldInfo[G, C]): String =
    s"'${f.alias.getOrElse(f.name)}'${f.alias.map(x => s" (alias for '$x')").mkString}"

  def compareValues[F[_]: Parallel, C](av: V[AnyValue], bv: V[AnyValue], caret: Option[C])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[Unit] = {
    (av, bv) match {
      case (V.VariableValue(avv), V.VariableValue(bvv)) =>
        if (avv === bvv) F.unit
        else raise[F, C, Unit](s"Variable '$avv' and '$bvv' are not equal.", caret)
      case (V.IntValue(ai), V.IntValue(bi)) =>
        if (ai === bi) F.unit
        else raise[F, C, Unit](s"Int '$ai' and '$bi' are not equal.", caret)
      case (V.FloatValue(af), V.FloatValue(bf)) =>
        if (af === bf) F.unit
        else raise[F, C, Unit](s"Float '$af' and '$bf' are not equal.", caret)
      case (V.StringValue(as), V.StringValue(bs)) =>
        if (as === bs) F.unit
        else raise[F, C, Unit](s"String '$as' and '$bs' are not equal.", caret)
      case (V.BooleanValue(ab), V.BooleanValue(bb)) =>
        if (ab === bb) F.unit
        else raise[F, C, Unit](s"Boolean '$ab' and '$bb' are not equal.", caret)
      case (V.EnumValue(ae), V.EnumValue(be)) =>
        if (ae === be) F.unit
        else raise[F, C, Unit](s"Enum '$ae' and '$be' are not equal.", caret)
      case (V.NullValue(), V.NullValue()) => F.unit
      case (V.ListValue(al), V.ListValue(bl)) =>
        if (al.length === bl.length) {
          al.zip(bl).zipWithIndex.parTraverse_ { case ((a, b), i) => ambientIndex(i)(compareValues[F, C](a, b, caret)) }
        } else
          raise[F, C, Unit](s"Lists are not af same size. Found list of length ${al.length} versus list of length ${bl.length}.", caret)
      case (V.ObjectValue(ao), V.ObjectValue(bo)) =>
        if (ao.size =!= bo.size)
          raise[F, C, Unit](
            s"Objects are not af same size. Found object of length ${ao.size} versus object of length ${bo.size}.",
            caret
          )
        else {
          def checkUniqueness(xs: List[(String, V[AnyValue])]) =
            xs.groupMap { case (k, _) => k } { case (_, v) => v }
              .toList
              .parTraverse {
                case (k, v :: Nil) => F.pure(k -> v)
                case (k, _)        => raise[F, C, (String, V[AnyValue])](s"Key '$k' is not unique in object.", caret)
              }
              .map(_.toMap)

          (checkUniqueness(ao), checkUniqueness(bo)).parTupled.flatMap { case (amap, bmap) =>
            // TODO test that verifies that order does not matter
            (amap align bmap).toList.parTraverse_ {
              case (k, Ior.Left(_))    => raise[F, C, Unit](s"Key '$k' is missing in object.", caret)
              case (k, Ior.Right(_))   => raise[F, C, Unit](s"Key '$k' is missing in object.", caret)
              case (k, Ior.Both(l, r)) => ambientField(k)(compareValues[F, C](l, r, caret))
            }
          }
        }
      case _ => raise[F, C, Unit](s"Values are not same type, got ${pValueName(av)} and ${pValueName(bv)}.", caret)
    }
  }

  def compareArguments[F[_]: Parallel, C](name: String, aa: QA.Arguments, ba: QA.Arguments, caret: Option[C])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ) = {
    def checkUniqueness(x: QA.Arguments): F[Map[String, QA.Argument]] =
      x.nel.toList
        .groupBy(_.name)
        .toList
        .parTraverse {
          case (k, v :: Nil) => F.pure(k -> v)
          case (k, _) =>
            raise[F, C, (String, QA.Argument)](s"Argument '$k' of field $name was not unique.", caret)
        }
        .map(_.toMap)

    (checkUniqueness(aa), checkUniqueness(ba)).parTupled.flatMap { case (amap, bmap) =>
      (amap align bmap).toList.parTraverse_ {
        case (k, Ior.Left(_)) =>
          raise[F, C, Unit](s"Field $name is already selected with argument '$k', but no argument was given here.", caret)
        case (k, Ior.Right(_)) =>
          raise[F, C, Unit](s"Field $name is already selected without argument '$k', but an argument was given here.", caret)
        case (k, Ior.Both(l, r)) => ambientField(k)(compareValues[F, C](l.value, r.value, caret))
      }
    }
  }

  // https://spec.graphql.org/draft/#sec-Field-Selection-Merging.Formal-Specification
  def checkFieldsMerge[F[_]: Parallel, G[_], C](
      a: FieldInfo[G, C],
      asi: SelectionInfo[G, C],
      b: FieldInfo[G, C],
      bsi: SelectionInfo[G, C]
  )(implicit F: MonadError[F, NonEmptyChain[PositionalError[C]]], L: Local[F, Prep]) = {
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
        case (Some(_), None) => raise[F, C, Unit](s"A selection of field ${fieldName(a)} has arguments, while another doesn't.", b.caret)
        case (None, Some(_)) => raise[F, C, Unit](s"A selection of field ${fieldName(a)} has arguments, while another doesn't.", b.caret)
        case (Some(aa), Some(ba)) => compareArguments[F, C](fieldName(a), aa, ba, b.caret)
      }

      val nameSameF =
        if (a.name === b.name) F.unit
        else {
          raise[F, C, Unit](
            s"Field $aIn and $bIn must have the same name (not alias) when they are merged.",
            a.caret
          )
        }

      appendMessage(s"They were merged since $whyMerge") {
        argsF &> nameSameF
      }
    } else F.unit

    // 1. in FieldsInSetCanMerge
    val shapeCheckF = checkSimplifiedTypeShape[F, G, C](a.tpe, b.tpe, a.caret)

    thoroughCheckF &> shapeCheckF
  }

  // There's a bug here that will allow fewer valid queries than the spec allows.
  // In the spec, when two fields are not to be considered equal in regards to arguments and aliasing
  // (i.e. they are not to be merged), they should only be structurally equal.
  // However, during structural equality checking, we can start to check the arguments again if two fields are defined in the same type.
  //
  // A new version of this function has two outcomes for every field name:
  // Either at-least one field is defined on a non-object type and all types must be fully equal (structural, arguments and alias)
  // Or all fields are defined on object types and only fields that are defied on the same type must be fully equal (structural, arguments and alias)
  // Fields between different types should just be structurally equal.
  //
  // It should be very feasible to implement this efficiently (i.e O(n)).
  def checkSelectionsMerge[F[_]: Parallel, G[_], C](xs: NonEmptyList[SelectionInfo[G, C]])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[Unit] = {
    val ys: NonEmptyList[NonEmptyList[(SelectionInfo[G, C], FieldInfo[G, C])]] =
      xs.flatMap(si => si.fields tupleLeft si)
        .groupByNem { case (_, f) => f.outputName }
        .toNel
        .map { case (_, v) => v }

    ys.parTraverse_ { zs =>
      // TODO partition into what should be fullchecked and what should be structural
      val mergeFieldsF = {
        val (siHead, fiHead) = zs.head
        zs.tail.parTraverse_ { case (si, fi) => checkFieldsMerge[F, G, C](fiHead, siHead, fi, si) }
      }

      mergeFieldsF >>
        zs.toList.flatMap { case (_, fi) => fi.tpe.selections }.toNel.traverse_(checkSelectionsMerge[F, G, C])
    }
  }

  // Optimization: we don't check selections recursively since checkSelectionsMerge traverses the whole tree
  // We only need to check the immidiate children and will eventually have checked the whole tree
  def checkSimplifiedTypeShape[F[_]: Parallel, G[_], C](a: SimplifiedType[G, C], b: SimplifiedType[G, C], caret: Option[C])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[Unit] = {
    (a, b) match {
      case (SimplifiedType.List(l), SimplifiedType.List(r))     => checkSimplifiedTypeShape[F, G, C](l, r, caret)
      case (SimplifiedType.Option(l), SimplifiedType.Option(r)) => checkSimplifiedTypeShape[F, G, C](l, r, caret)
      // It turns out we don't care if more fields are selected in one object than the other
      case (SimplifiedType.Selectable(_, _), SimplifiedType.Selectable(_, _)) => F.unit
      // case (SimplifiedType.Selectable(_, l), SimplifiedType.Selectable(_, r)) => F.unit
      // val lComb = l.flatMap(x => x.fields tupleLeft x).groupByNem { case (_, f) => f.outputName }
      // val rComb = r.flatMap(x => x.fields tupleLeft x).groupByNem { case (_, f) => f.outputName }
      // (lComb align rComb).toNel.parTraverse_ {
      //   case (_, Ior.Both(_, _)) => F.unit
      //   case (k, Ior.Left(_))    => raise[F, Unit](s"Field '$k' was missing when verifying shape equivalence.", Some(caret))
      //   case (k, Ior.Right(_))   => raise[F, Unit](s"Field '$k' was missing when verifying shape equivalence.", Some(caret))
      // }
      case (SimplifiedType.Enum(l), SimplifiedType.Enum(r)) =>
        if (l === r) F.unit
        else raise[F, C, Unit](s"Enums are not the same, got '$l' and '$r'.", caret)
      case (SimplifiedType.Scalar(l), SimplifiedType.Scalar(r)) =>
        if (l === r) F.unit
        else raise[F, C, Unit](s"Scalars are not the same, got '$l' and '$r'.", caret)
      case _ =>
        raise[F, C, Unit](s"Types are not the same, got ${getSimplifiedTypeString(a)} and ${getSimplifiedTypeString(b)}.", caret)
    }
  }

  final case class FoundImplementation[G[_], A, B](
      tpe: Type[G, B],
      specify: A => Option[B]
  )
  def findImplementations[G[_], A](
      s: Selectable[G, A],
      discoveryState: SchemaShape.DiscoveryState[G]
  ): List[FoundImplementation[G, A, ?]] = s match {
    case t: Type[G, ?] => List(FoundImplementation(t, Some(_)))
    case u: Union[G, ?] =>
      u.types.toList.map { case x: gql.ast.Variant[G, A, b] =>
        FoundImplementation(x.tpe.value, x.specify)
      }
    case it @ Interface(_, _, _, _) =>
      val m: Map[String, SchemaShape.InterfaceImpl[G, A]] =
        discoveryState.implementations
          .get(it.name)
          .getOrElse(Map.empty)
          .collect { case (k, v: SchemaShape.InterfaceImpl[G, A] @unchecked) => (k, v) }

      m.values.toList
        .collect { case ti: SchemaShape.InterfaceImpl.TypeImpl[G, A, b] => FoundImplementation(ti.t, ti.specify) }
  }

  final case class MergedFieldInfo[G[_], C](
      name: String,
      alias: Option[String],
      args: Option[QA.Arguments],
      selections: List[SelectionInfo[G, C]],
      // TODO these two should probably be lists
      caret: Option[C],
      path: Cursor
  )
  final case class PairedFieldSelection[G[_], A, C](
      info: MergedFieldInfo[G, C],
      field: Field[G, A, ?]
  )
  final case class MergedImplementation[G[_], A, B, C](
      leaf: Type[G, B],
      selections: NonEmptyList[PairedFieldSelection[G, B, C]],
      specify: A => Option[B]
  )
  def mergeImplementations[F[_]: Parallel, G[_], A, C](
      base: Selectable[G, A],
      sels: NonEmptyList[SelectionInfo[G, C]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[NonEmptyList[MergedImplementation[G, A, ?, C]]] = {
    // We need to find all implementations of the base type
    val concreteBaseMap = findImplementations[G, A](base, discoveryState).map(x => x.tpe.name -> x).toMap

    val concreteBase = concreteBaseMap.toList

    val nestedSelections: List[(String, NonEmptyList[FieldInfo[G, C]])] = sels.toList.flatMap { sel =>
      /* The set of typenames that implement whatever we're selecting on
       * ```graphql
       * interface A {
       *   name: String
       * }
       *
       * {
       *   ...
       *   ... on A {
       *     name
       *   }
       * }
       * ```
       * In this case, we have a selection on `A`, so we must figure out what types implement `A`
       * and then for every type `T` that implements `A`, we must find the field `name` and select it on `T`.
       */
      val concreteIntersections = findImplementations(sel.s, discoveryState)
        .map { case FoundImplementation(t, _) => t.name }

      concreteIntersections tupleRight sel.fields
    }

    // TODO field merging can be optimized significantly by deduplicating fragment spreads
    // (if two fields are in the same fragment (maybe also the same position)?)
    /*
     * Now we must merge all fields that are selected on the same type.
     *
     * Merge fields at this level only.
     * We cannot merge fields globally, because we need to know the base type
     * And even if we looked for the base type, we might as well do resolver/step preparation and argument parsing
     * since that would require us to walk the tree again.
     */

    type Typename = String
    type FieldName = String
    // There may be more than one field with the same name
    // This is fine, but we need to merge their implementations
    val grouped: Map[Typename, NonEmptyMap[FieldName, NonEmptyList[FieldInfo[G, C]]]] = nestedSelections
      .groupMap { case (k, _) => k } { case (_, vs) => vs }
      .collect { case (k, x :: xs) => k -> NonEmptyList(x, xs).flatten.groupByNem(_.outputName) }

    val merged = grouped.fmap(_.fmap { fields =>
      val sels = fields.map(_.tpe.selections).reduce
      MergedFieldInfo(
        fields.head.name,
        fields.head.alias,
        fields.head.args,
        sels,
        fields.head.caret,
        fields.head.path
      )
    })

    val collected: F[List[MergedImplementation[G, A, ?, C]]] = concreteBase.parFlatTraverse { case (k, (fi: FoundImplementation[G, A, b])) =>
      val t = fi.tpe
      val specify = fi.specify
      merged.get(k).toList.traverse { fields =>
        fields.toNonEmptyList
          .parTraverse { f =>
            if (f.name === "__typename") F.pure(PairedFieldSelection[G, b, C](f, typenameField[b](t.name)))
            else {
              t.fieldMap.get(f.name) match {
                case None =>
                  raise[F, C, PairedFieldSelection[G, b, C]](s"Could not find field '${f.name}' on type `${t.name}`.", None)
                case Some(field) => F.pure(PairedFieldSelection[G, b, C](f, field))
              }
            }
          }
          .map(fields => MergedImplementation[G, A, b, C](t, fields, specify))
      }
    }

    collected.flatMap { xs =>
      xs.toNel match {
        case Some(x) => F.pure(x)
        case None =>
          raise[F, C, NonEmptyList[MergedImplementation[G, A, ?, C]]](
            s"Could not find any implementations of `${base.name}` in the selection set.",
            None
          )
      }
    }
  }

  def decodeFieldArgs[F[_]: Parallel, G[_], C, A](
      a: Arg[A],
      args: Option[QA.Arguments],
      variableMap: VariableMap
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]]
  ): F[A] = {
    val provided = args.toList.flatMap(_.nel.toList)

    // Treat input arguments as an object
    // Decode the args as-if an input
    val argObj =
      V.ObjectValue(provided.map(a => a.name -> a.value))

    parseInputObj[F, C, A](argObj, a, Some(variableMap), ambigiousEnum = false)
  }

  def prepareField[F[_]: Parallel, G[_], C, I, T](
      fi: MergedFieldInfo[G, C],
      field: Field[G, I, T],
      currentTypename: String,
      variableMap: VariableMap,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      L: Local[F, Prep],
      S: Stateful[F, Int],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F]
  ): F[PreparedDataField[G, I]] = D.defer {
    val tpe = field.output.value
    val selCaret = fi.caret
    val name = fi.name
    val step = field.resolve.underlying

    val rootUniqueName = UniqueEdgeCursor(s"${currentTypename}_$name")

    val meta: PreparedMeta = PreparedMeta(
      variableMap,
      fi.alias,
      fi.args
    )

    def compileCont[A](t: Out[G, A], cursor: UniqueEdgeCursor): Used[F, Prepared[G, A]] =
      (t, fi.selections.toNel) match {
        case (out: gql.ast.OutArr[g, a, c, b], _) =>
          val innerStep: Step[G, a, b] = out.resolver.underlying
          val nc = cursor append "array"
          val compiledStep = compileStep[F, G, C, a, b](innerStep, nc, meta)
          val compiledCont = compileCont[b](out.of, nc)
          (compiledStep, compiledCont)
            .parMapN((s, c) => PreparedList(PreparedCont(s, c), out.toSeq))
        case (out: gql.ast.OutOpt[g, a, b], _) =>
          val innerStep: Step[G, a, b] = out.resolver.underlying
          val nc = cursor append "option"
          val compiledStep = compileStep[F, G, C, a, b](innerStep, nc, meta)
          val compiledCont = compileCont[b](out.of, nc)
          (compiledStep, compiledCont)
            .parMapN((s, c) => PreparedOption(PreparedCont(s, c)))
        case (s: Selectable[G, a], Some(ss)) =>
          Used.liftF {
            prepareSelectable[F, G, C, a](s, ss, variableMap, discoveryState)
              .map(Selection(_))
          }
        case (e: Enum[a], None) =>
          Used[F, C].pure(PreparedLeaf(e.name, x => Json.fromString(e.revm(x))))
        case (s: Scalar[a], None) =>
          import io.circe.syntax._
          Used[F, C].pure(PreparedLeaf(s.name, x => s.encoder(x).asJson))
        case (o, Some(_)) =>
          Used.liftF(raise(s"Type `${friendlyName(o)}` cannot have selections.", selCaret))
        case (o, None) =>
          Used.liftF(raise(s"Object like type `${friendlyName(o)}` must have a selection.", selCaret))
      }

    val usedF = (
      compileStep[F, G, C, I, T](step, rootUniqueName, meta),
      compileCont(tpe, rootUniqueName)
    ).parMapN { (s, c) =>
      val pc = PreparedCont(s, c)
      PreparedDataField(name, fi.alias, pc)
    }

    usedF.run
      .flatMap { case (used, a) =>
        val provided = fi.args.toList.flatMap(_.nel.toList).map(_.name).toSet
        val tooMany = provided -- used
        if (tooMany.isEmpty) F.pure(a)
        else raise(s"Field '$name' does not accept the arguments ${tooMany.map(s => s"'$s'").toList.mkString_(", ")}", selCaret)
      }
  }

  def prepareSelectable[F[_]: Parallel, G[_], C, A](
      s: Selectable[G, A],
      sis: NonEmptyList[SelectionInfo[G, C]],
      variableMap: VariableMap,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      S: Stateful[F, Int],
      D: Defer[F]
  ): F[NonEmptyList[PreparedSpecification[G, A, ?]]] = {
    mergeImplementations[F, G, A, C](s, sis, discoveryState).flatMap { impls =>
      impls.parTraverse[F, PreparedSpecification[G, A, ?]] { case impl: MergedImplementation[G, A, b, C] =>
        val fa = impl.selections.parTraverse { sel =>
          sel.field match {
            case field: Field[G, b2, t] =>
              prepareField[F, G, C, b, t](sel.info, field, impl.leaf.name, variableMap, discoveryState)
          }
        }

        fa.map(xs => PreparedSpecification[G, A, b](s.name, impl.specify, xs))
      }
    }
  }

  def prepareSelectableRoot[F[_]: Parallel, G[_], P[_], C, A](
      s: Selectable[G, A],
      ss: QA.SelectionSet[P],
      variableMap: VariableMap,
      fragments: Map[String, P[QA.FragmentDefinition[P]]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      L: Local[F, Prep],
      S: Stateful[F, Int],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      D: Defer[F],
      P: Positional[P, C]
  ): F[NonEmptyList[PreparedSpecification[G, A, ?]]] = {
    collectSelectionInfo[F, G, P, C](s, ss, variableMap, fragments, discoveryState).flatMap { root =>
      checkSelectionsMerge[F, G, C](root) >> prepareSelectable[F, G, C, A](s, root, variableMap, discoveryState)
    }
  }

  type VariableMap = Map[String, Either[V[Const], Json]]

  def matchType[F[_], G[_], P[_], C](
      name: String,
      sel: Selectable[G, ?],
      caret: Option[C],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit F: MonadError[F, NonEmptyChain[PositionalError[C]]], L: Local[F, Prep]): F[Selectable[G, ?]] =
    if (sel.name == name) F.pure(sel)
    else {
      sel match {
        case t @ Type(n, _, _, _) =>
          // Check downcast
          t.implementsMap.get(name).map(_.value) match {
            case None    => raise(s"Tried to match with type `$name` on type object type `$n`.", caret)
            case Some(i) => F.pure(i)
          }
        // What types implement this interface?
        // We can both downcast and up-match
        case i @ Interface(n, _, _, _) =>
          i.implementsMap.get(name).map(_.value) match {
            case Some(i) => F.pure(i)
            case None =>
              raiseOpt(
                discoveryState.implementations.get(i.name),
                s"The interface `${i.name}` is not implemented by any type.",
                caret
              ).flatMap { m =>
                raiseOpt(
                  m.get(name).map {
                    case t: SchemaShape.InterfaceImpl.TypeImpl[G @unchecked, ?, ?]    => t.t
                    case i: SchemaShape.InterfaceImpl.OtherInterface[G @unchecked, ?] => i.i
                  },
                  s"`$name` does not implement interface `$n`, possible implementations are ${m.keySet.mkString(", ")}.",
                  caret
                )
              }
          }
        // Can match to any type or any of it's types' interfacees
        case u @ Union(n, _, _) =>
          u.instanceMap
            .get(name) match {
            case Some(i) => F.pure(i.tpe.value)
            case None =>
              u.types.toList
                .map(_.tpe.value)
                .collectFirstSome(_.implementsMap.get(name)) match {
                case None =>
                  raise(
                    s"`$name` is not a member of the union `$n` (or any of the union's types' implemented interfaces), possible members are ${u.instanceMap.keySet
                      .mkString(", ")}.",
                    caret
                  )
                case Some(x) => F.pure(x.value)
              }
          }
      }
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

  def inName[A](in: In[A]): String =
    ModifierStack
      .fromIn(in)
      .show(_.name)

  def parseInputObj[F[_]: Parallel, C, A](
      v: V.ObjectValue[gql.parser.AnyValue],
      fields: Arg[A],
      variableMap: Option[VariableMap],
      ambigiousEnum: Boolean
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[A] = {
    val xs = v.v

    val m = xs.toMap
    val required = fields.entries.map(x => x.name -> x).toList.toMap

    // All provided fields are defined
    val tooMuch = m.keySet -- required.keySet
    val tooMuchF =
      if (tooMuch.isEmpty) F.unit
      else raise[F, C, Unit](s"Too many fields provided, unknown fields are ${tooMuch.toList.map(x => s"'$x'").mkString_(", ")}.", None)

    tooMuchF >> parseArg[F, C, A](fields, m, variableMap, ambigiousEnum)
  }

  def parseInput[F[_]: Parallel, C, A](v: V[AnyValue], tpe: In[A], variableMap: Option[VariableMap], ambigiousEnum: Boolean)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[A] =
    (tpe, v) match {
      case (_, V.VariableValue(v)) =>
        variableMap match {
          case None => raise(s"Variables may not occur here. Variable '$$$v' was provided.", None)
          case Some(vm) =>
            vm.get(v) match {
              case None =>
                raise(
                  s"Variable '$$$v' was not declared and provided as a possible variable for this operation. Hint add the variable to the variables list of the operation '(..., $$$v: ${inName(tpe)})' and provide a value in the variables parameter.",
                  None
                )
              case Some(Left(pval)) => parseInput[F, C, A](pval, tpe, None, ambigiousEnum = false)
              case Some(Right(j)) =>
                val asPVal = V.fromJson(j)
                parseInput[F, C, A](asPVal, tpe, None, ambigiousEnum = true)
            }
        }
      case (e @ Enum(name, _, _), v) =>
        ambientField(name) {
          val fa: F[String] = v match {
            case V.EnumValue(s)                    => F.pure(s)
            case V.StringValue(s) if ambigiousEnum => F.pure(s)
            case _ =>
              raise(s"Enum value expected for `$name`, but got ${pValueName(v)}.", None)
          }
          fa.flatMap { s =>
            e.m.lookup(s) match {
              case Some(x) => F.pure(x)
              case None =>
                val names = e.m.keys.toList
                raise(
                  s"Enum value `$s` does not occur in enum type `$name`, possible enum values are ${names.map(s => s"`$s`").mkString_(", ")}.",
                  None
                )
            }
          }
        }
      case (Scalar(name, _, decoder, _), x: NonVar) =>
        ambientField(name) {
          raiseEither(decoder(x), None)
        }
      case (Input(name, fields, _), V.ObjectValue(xs)) =>
        ambientField(name) {
          parseArg[F, C, A](fields, xs.toMap, variableMap, ambigiousEnum)
        }
      case (arr: InArr[a, c], V.ListValue(xs)) =>
        xs.zipWithIndex
          .parTraverse { case (x, i) =>
            ambientIndex(i) {
              parseInput[F, C, a](x, arr.of, variableMap, ambigiousEnum)
            }
          }
          .flatMap(arr.fromSeq(_).fold(raise(_, None), F.pure(_)))
      case (_: InOpt[a], V.NullValue()) => F.pure(Option.empty[a])
      case (opt: InOpt[a], x)           => parseInput[F, C, a](x, opt.of, variableMap, ambigiousEnum).map(Some(_))
      case (i, _)                       => raise(s"Expected type `${inName(i)}`, but got value ${pValueName(v)}.", None)
    }

  def parseArg[F[_]: Parallel, C, A](arg: Arg[A], input: Map[String, V[AnyValue]], variableMap: Option[VariableMap], ambigiousEnum: Boolean)(
      implicit
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      L: Local[F, Prep]
  ): F[A] = {
    val fv = arg.impl.foldMap[F, ValidatedNec[String, A]](new (Arg.Impl ~> F) {
      def apply[A](fa: Arg.Impl[A]): F[A] = fa match {
        case fa: DecodedArgValue[a, A] =>
          ambientField(fa.av.name) {
            def compileWith(x: V[AnyValue], default: Boolean) =
              parseInput[F, C, a](x, fa.av.input.value, variableMap, ambigiousEnum)
                .flatMap(a => raiseEither[F, C, A](fa.decode(ArgParam(default, a)), None))

            input
              .get(fa.av.name)
              .map(compileWith(_, false))
              .orElse(fa.av.defaultValue.map(compileWith(_, true)))
              .getOrElse {
                fa.av.input.value match {
                  case _: gql.ast.InOpt[a] => raiseEither(fa.decode(ArgParam(true, None)), None)
                  case _ =>
                    raise(s"Missing argument for '${fa.av.name}' and no default value was found.", None)
                }
              }
          }
      }
    })

    fv.flatMap(v => raiseEither(v.toEither.leftMap(_.mkString_(", ")), None))
  }

  def getOperationDefinition[F[_], P[_], C](
      ops: List[P[QA.OperationDefinition[P]]],
      operationName: Option[String]
  )(implicit 
  F: MonadError[F, (String, List[C])],
  P: Positional[P, C]
  ): F[QA.OperationDefinition[P]] = {
    lazy val possible = ops
      .map(P(_))
      .collect { case d: QA.OperationDefinition.Detailed[P] => d.name }
      .collect { case Some(x) => s"'$x'" }
      .mkString(", ")
    (ops, operationName) match {
      case (Nil, _)      => F.raiseError((s"No operations provided.", Nil))
      case (x :: Nil, _) => F.pure(P(x))
      case (xs, _) if xs.exists {
            case Pos(_, _: QA.OperationDefinition.Simple[?])                     => true
            case Pos(_, x: QA.OperationDefinition.Detailed[?]) if x.name.isEmpty => true
            case _                                                           => false
          } =>
        F.raiseError(
          (s"Exactly one operation must be suplied if the operations include at least one unnamed operation.", xs.map(P.extract))
        )
      case (xs, None) =>
        F.raiseError(
          (s"Operation name must be supplied when supplying multiple operations, provided operations are $possible."),
          xs.map(P.extract)
        )
      case (xs, Some(name)) =>
        val o = xs.collectFirst { case Pos(_, d: QA.OperationDefinition.Detailed[P]) if d.name.contains(name) => d }
        F.fromOption(o, (s"Unable to find operation '$name', provided possible operations are $possible.", xs.map(P.extract)))
    }
  }

  def operationType[P[_]](od: QA.OperationDefinition[P]) =
    od match {
      case QA.OperationDefinition.Simple(_)             => QA.OperationType.Query
      case QA.OperationDefinition.Detailed(ot, _, _, _) => ot
    }

  sealed trait PrepResult[G[_], Q, M, S]
  object PrepResult {
    final case class Query[G[_], Q, M, S](query: NonEmptyList[PreparedField[G, Q]]) extends PrepResult[G, Q, M, S]
    final case class Mutation[G[_], Q, M, S](mutation: NonEmptyList[PreparedField[G, M]]) extends PrepResult[G, Q, M, S]
    final case class Subscription[G[_], Q, M, S](subscription: NonEmptyList[PreparedField[G, S]]) extends PrepResult[G, Q, M, S]
  }

  // TODO add another phase after finding the OperationDefinition and before this,
  // that checks all that variables have been used
  def prepareParts[F[_]: Parallel, G[_], P[_], C, Q, M, S](
      op: QA.OperationDefinition[P],
      frags: List[P[QA.FragmentDefinition[P]]],
      schema: SchemaShape[G, Q, M, S],
      variableMap: Map[String, Json]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError[C]]],
      S: Stateful[F, Int],
      D: Defer[F],
      P: Positional[P, C]
  ): F[PrepResult[G, Q, M, S]] = {
    val ot = operationType(op)

    val preCheckVariablesF: F[VariableMap] = op match {
      case QA.OperationDefinition.Simple(_) => F.pure(Map.empty)
      case QA.OperationDefinition.Detailed(_, _, vdsO, _) =>
        vdsO.toList
          .flatMap(_.nel.toList)
          .parTraverse { p =>
            val vd = P(p)
            val caret = P.extract(p)
            val defaultWithFallback: Option[V[Const]] = vd.defaultValue.orElse(vd.tpe match {
              case T.NonNull(_) => None
              case _            => Some(V.NullValue())
            })

            def printType(t: T, inOption: Boolean = false): String = {
              val suffix = if (inOption) "" else "!"
              val prefix = t match {
                case T.List(of)    => s"[${printType(of)}]"
                case T.Named(n)    => n
                case T.NonNull(of) => printType(of, inOption = true)
              }
              prefix + suffix
            }

            val rootValueF: F[Either[V[Const], Json]] = (variableMap.get(vd.name), defaultWithFallback) match {
              case (None, Some(d)) => F.pure(Left(d))
              case (Some(x), _)    => F.pure(Right(x))
              case (None, None) =>
                raise(
                  s"Variable '$$${vd.name}' is required but was not provided. Hint: Provide variable or a default value for '$$${vd.name}' of type `${printType(vd.tpe)}`.",
                  caret.some
                )
            }

            def verify(currentType: T, currentValue: Either[V[Const], Json], inOption: Boolean): F[Unit] = {
              (currentType, currentValue) match {
                case (T.Named(name), v) =>
                  v match {
                    case Left(V.ListValue(_)) =>
                      raise(
                        s"Expected a value of type `$name` when checking the default value of '$$${vd.name}', found list instead.",
                        caret.some
                      )
                    case Left(V.NullValue()) if !inOption =>
                      raise(
                        s"Expected a non-nullable value of type `$name` when checking the default value of '$$${vd.name}', found null instead.",
                        caret.some
                      )

                    case Right(j) if j.isArray =>
                      raise(
                        s"Expected a value of type `$name` when checking the variable input of '$$${vd.name}', found list instead.",
                        caret.some
                      )
                    case Right(j) if j.isNull && !inOption =>
                      raise(
                        s"Expected a non-nullable value of type `$name` when checking the variable input of '$$${vd.name}', found null instead.",
                        caret.some
                      )
                    case _ => F.unit
                  }
                case (T.List(of), v) =>
                  v match {
                    case Left(V.ListValue(vs)) =>
                      vs.zipWithIndex.parTraverse { case (v, i) =>
                        ambientIndex(i) {
                          verify(of, Left(v), inOption = true)
                        }
                      }.void
                    case Left(V.NullValue()) =>
                      if (inOption) F.unit
                      else
                        raise(
                          s"Expected a non-nullable value of type list when checking the default value of '$$${vd.name}', found null instead.",
                          caret.some
                        )
                    case Left(p) =>
                      raise(
                        s"Expected a value of type list when checking the default value of '$$${vd.name}', found a graphql value of type ${pValueName(p)}} instead.",
                        caret.some
                      )
                    case Right(j) =>
                      if (j.isNull) {
                        if (inOption) F.unit
                        else
                          raise(
                            s"Expected a non-nullable value of type list when checking the variable input of '$$${vd.name}', found null instead.",
                            caret.some
                          )
                      } else {
                        j.asArray match {
                          case None =>
                            raise(
                              s"Expected a value of type list when checking the variable input of '$$${vd.name}', found a json value of type ${j.name} instead.",
                              caret.some
                            )
                          case Some(xs) =>
                            xs.zipWithIndex.parTraverse { case (v, i) =>
                              ambientIndex(i) {
                                verify(of, Right(v), inOption = true)
                              }
                            }.void
                        }
                      }
                  }
                case (T.NonNull(of), v) => verify(of, v, inOption = false)
              }
            }

            val rootType = vd.tpe
            rootValueF
              .flatTap(e => verify(rootType, e, inOption = true))
              .map(v => vd.name -> v)
          }
          .map(_.toMap)
    }

    val selection = op match {
      case QA.OperationDefinition.Simple(sel)            => sel
      case QA.OperationDefinition.Detailed(_, _, _, sel) => sel
    }

    preCheckVariablesF.flatMap[PrepResult[G, Q, M, S]] { vm =>
      def runWith[A](o: Type[G, A]): F[NonEmptyList[PreparedSpecification[G, A, _]]] =
        prepareSelectableRoot[F, G, P, C, A](
          o,
          selection,
          vm,
          frags.map(f => P(f).name -> f).toMap,
          schema.discover
        )

      ot match {
        // We sneak the introspection query in here
        case QA.OperationType.Query =>
          val i: NonEmptyList[(String, Field[G, Unit, ?])] = schema.introspection
          val q = schema.query
          val full = q.copy(fields = i.map { case (k, v) => k -> v.contramap[G, Q](_ => ()) } concatNel q.fields)
          runWith[Q](full).map(PrepResult.Query(_))
        case QA.OperationType.Mutation =>
          raiseOpt(schema.mutation, "No `Mutation` type defined in this schema.", None)
            .flatMap(runWith[M])
            .map(PrepResult.Mutation(_))
        case QA.OperationType.Subscription =>
          raiseOpt(schema.subscription, "No `Subscription` type defined in this schema.", None)
            .flatMap(runWith[S])
            .map(PrepResult.Subscription(_))
      }
    }
  }

  // We can derive stateful and local from monad partial order for the stack :)
  // Note that EitherT must be the wrap State since EitherT provides a parallel evidence even though its effect is not parallel; wohoo
  // Note also that State (Stateful) can model what Kleisli (Local) does, but it is nicer to use Local for non-stateful operations
  // Using State for the Local algbra can easily cause headaches, such as when accidentally resetting the state or similar
  // The Prepare program needs to accumulate ids, while resetting paths.
  type H[A, C] = EitherT[Kleisli[State[Int, *], Prep, *], NonEmptyChain[PositionalError[C]], A]

  def runK[C] = new (H[*, C] ~> EitherNec[PositionalError[C], *]) {
    override def apply[A](fa: H[A, C]): EitherNec[PositionalError[C], A] =
      fa.value.run(Prep.empty).runA(0).value
  }

  trait Positional[P[_], C] {
    def apply[A](p: P[A]): A

    def extract[A](p: P[A]): C
  }

  object Positional {
    def apply[P[_], C](f: P ~> cats.data.Const[C, *], fk: P ~> Id): Positional[P, C] = new Positional[P, C] {
      def apply[A](pa: P[A]) = fk(pa)

      def extract[A](pa: P[A]) = f(pa).getConst
    }
  }

  def prepare[F[_], P[_], C, Q, M, S](
      executabels: NonEmptyList[QA.ExecutableDefinition[P]],
      schema: SchemaShape[F, Q, M, S],
      variableMap: Map[String, Json],
      operationName: Option[String]
  )(implicit P: Positional[P, C]): EitherNec[PositionalError[C], PrepResult[F, Q, M, S]] = {
    val (ops, frags) = executabels.toList.partitionEither {
      case QA.ExecutableDefinition.Operation(op)  => Left(op)
      case QA.ExecutableDefinition.Fragment(frag) => Right(frag)
    }

    getOperationDefinition[Either[(String, List[C]), *], P, C](ops, operationName) match {
      case Left((e, carets)) => Left(NonEmptyChain.one(PositionalError(Cursor.empty, carets, e)))
      case Right(op) =>
        runK {
          prepareParts[H[*, C], F, P, C, Q, M, S](op, frags, schema, variableMap)
        }
    }
  }
}
