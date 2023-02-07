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
import gql.parser.{QueryParser => P, Pos}
import P.Value._
import gql.ast._
import gql.resolver._
import cats.parse.Caret
import gql.parser.QueryParser

object PreparedQuery {
  sealed trait PreparedField[F[_], A]

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

  sealed trait PreparedStep[F[_], -I, +O]

  final case class PreparedMeta(
      variables: VariableMap,
      alias: Option[String],
      args: Option[P.Arguments]
  )

  object PreparedStep {
    final case class Lift[F[_], I, O](f: I => O) extends AnyRef with PreparedStep[F, I, O]
    final case class EmbedEffect[F[_], I](stableUniqueEdgeName: UniqueEdgeCursor) extends AnyRef with PreparedStep[F, F[I], I]
    final case class EmbedStream[F[_], I](stableUniqueEdgeName: UniqueEdgeCursor) extends AnyRef with PreparedStep[F, fs2.Stream[F, I], I]
    final case class EmbedError[F[_], I]() extends AnyRef with PreparedStep[F, Ior[String, I], I]
    final case class Compose[F[_], I, A, O](left: PreparedStep[F, I, A], right: PreparedStep[F, A, O])
        extends AnyRef
        with PreparedStep[F, I, O]
    final case class Skip[F[_], I, O](compute: PreparedStep[F, I, O]) extends AnyRef with PreparedStep[F, Either[I, O], O]
    final case class GetMeta[F[_], I](meta: PreparedMeta) extends AnyRef with PreparedStep[F, I, Meta]
    final case class First[F[_], I, O, C](step: PreparedStep[F, I, O]) extends AnyRef with PreparedStep[F, (I, C), (O, C)]
    final case class Batch[F[_], K, V](id: Step.BatchKey[K, V], globalEdgeId: UniqueBatchInstance[K, V])
        extends AnyRef
        with PreparedStep[F, Set[K], Map[K, V]]
  }

  final case class UniqueBatchInstance[K, V](id: Int) extends AnyVal

  final case class UniqueEdgeCursor(path: NonEmptyChain[String]) {
    def append(name: String): UniqueEdgeCursor = UniqueEdgeCursor(path append name)

    lazy val asString: String = path.mkString_(".")
  }

  object UniqueEdgeCursor {
    def apply(name: String): UniqueEdgeCursor = UniqueEdgeCursor(NonEmptyChain.one(name))
  }

  final case class PositionalError(position: PrepCursor, caret: List[Caret], message: String) {
    lazy val asGraphQL: JsonObject = {
      import io.circe.syntax._
      Map(
        "message" -> Some(message.asJson),
        "locations" -> caret.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
        "path" -> NonEmptyChain.fromChain(position.position.map(_.name)).map(_.asJson)
      ).collect { case (k, Some(v)) => k -> v }.asJsonObject
    }
  }

  final case class PrepCursor(position: Chain[Validation.Edge]) {
    def add(edge: Validation.Edge): PrepCursor = PrepCursor(position append edge)
  }

  object PrepCursor {
    val empty: PrepCursor = PrepCursor(Chain.empty)
  }

  final case class Prep(
      cycleSet: Set[String],
      cursor: PrepCursor
  ) {
    def addEdge(edge: Validation.Edge): Prep = copy(cursor = cursor add edge)

    def addCycle(s: String): Prep = copy(cycleSet = cycleSet + s)
  }

  object Prep {
    val empty: Prep = Prep(Set.empty, PrepCursor.empty)
  }

  type UsedArgs = Set[String]
  type Used[F[_], A] = WriterT[F, UsedArgs, A]
  object Used {
    def apply[F[_]](implicit F: MonadError[F, NonEmptyChain[PositionalError]]) =
      MonadError[Used[F, *], NonEmptyChain[PositionalError]]
    def liftF[F[_]: Applicative, A](fa: F[A]) = WriterT.liftF[F, UsedArgs, A](fa)
  }
  def compileStep[F[_]: Parallel, G[_], I, O](step: Step[G, I, O], cursor: UniqueEdgeCursor, meta: PreparedMeta)(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): Used[F, State[Int, PreparedStep[G, I, O]]] = {
    def pure[A](a: A): Used[F, State[Int, A]] =
      Used[F].pure(State.pure(a))

    def rec[I2, O2](step: Step[G, I2, O2], edge: String): Used[F, State[Int, PreparedStep[G, I2, O2]]] =
      compileStep[F, G, I2, O2](step, cursor append edge, meta)

    step match {
      case Step.Alg.Lift(f)      => pure(PreparedStep.Lift(f))
      case Step.Alg.EmbedError() => pure(PreparedStep.EmbedError[G, O]())
      case alg: Step.Alg.Compose[?, i, a, o] =>
        val left = rec[i, a](alg.left, "left")
        val right = rec[a, o](alg.right, "right")
        (left, right).parMapN((_, _).tupled).map(_.map { case (l, r) => PreparedStep.Compose[G, i, a, o](l, r) })
      case _: Step.Alg.EmbedEffect[?, i] => pure(PreparedStep.EmbedEffect[G, i](cursor))
      case _: Step.Alg.EmbedStream[?, i] => pure(PreparedStep.EmbedStream[G, i](cursor))
      case alg: Step.Alg.Skip[g, i, ?] =>
        rec[i, O](alg.compute, "skip").map(_.map(s => PreparedStep.Skip(s)))
      case Step.Alg.GetMeta() => pure(PreparedStep.GetMeta(meta))
      case alg: Step.Alg.Batch[?, k, v] =>
        Used[F].pure(State { (i: Int) =>
          (i + 1, PreparedStep.Batch[G, k, v](alg.id, UniqueBatchInstance(i)))
        })
      case alg: Step.Alg.First[?, i, o, c] =>
        rec[i, o](alg.step, "first").map(_.map(s => PreparedStep.First[G, i, o, c](s)))
      case alg: Step.Alg.Argument[g, ?, a] =>
        val fa = Used
          .liftF(decodeFieldArgs[F, G, a](alg.arg, meta.args, meta.variables))
          .map[PreparedStep[G, I, O]](o => PreparedStep.Lift[G, I, O](_ => o)) <*
          WriterT.tell(alg.arg.entries.map(_.name).toList.toSet)

        fa.map(State.pure(_))
    }
  }

  def collectFields[G[_]](step: Step[G, ?, ?]): Chain[Arg[Any]] =
    step match {
      case Step.Alg.Argument(a)   => Chain.one(a)
      case Step.Alg.First(s)      => collectFields(s)
      case Step.Alg.Skip(s)       => collectFields(s)
      case Step.Alg.Compose(l, r) => collectFields(l) ++ collectFields(r)
      case _                      => Chain.empty
    }

  def friendlyName[G[_], A](ot: Out[G, A], inOption: Boolean = false): String = {
    val suffix = if (inOption) "" else "!"
    val prefix = (ot: @unchecked) match {
      case Scalar(name, _, _, _)    => name
      case Enum(name, _, _)         => name
      case Type(name, _, _, _)      => name
      case Union(name, _, _)        => name
      case Interface(name, _, _, _) => name
      case OutOpt(of, _)            => friendlyName(of, inOption = true)
      case OutArr(of, _, _)         => s"[${friendlyName(of)}]"
    }
    prefix + suffix
  }

  def raise[F[_], A](s: String, caret: Option[Caret])(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] =
    L.ask.map(state => NonEmptyChain.one(PositionalError(state.cursor, caret.toList, s))).flatMap(F.raiseError[A])

  def raiseOpt[F[_], A](o: Option[A], s: String, caret: Option[Caret])(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] =
    o.map(_.pure[F]).getOrElse(raise[F, A](s, caret))

  def raiseEither[F[_], A](e: Either[String, A], caret: Option[Caret])(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] =
    e match {
      case Left(value)  => raise[F, A](value, caret)
      case Right(value) => F.pure(value)
    }

  def ambientEdge[F[_], A](edge: Validation.Edge)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    L.local(fa)(_ addEdge edge)

  def ambientField[F[_], A](name: String)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.Field(name))(fa)

  def ambientArg[F[_], A](name: String)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.Arg(name))(fa)

  def ambientIndex[F[_], A](i: Int)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.Index(i))(fa)

  def ambientInputType[F[_], A](name: String)(fa: F[A])(implicit L: Local[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.InputType(name))(fa)

  def modifyError[F[_], A](f: PositionalError => PositionalError)(fa: F[A])(implicit F: MonadError[F, NonEmptyChain[PositionalError]]) =
    F.adaptError(fa)(_.map(f))

  def appendMessage[F[_], A](message: String)(fa: F[A])(implicit F: MonadError[F, NonEmptyChain[PositionalError]]) =
    modifyError[F, A](d => d.copy(message = d.message + "\n" + message))(fa)

  def typenameField[A](typename: String) = {
    import gql.dsl._
    lift[fs2.Pure, A](_ => typename)
  }

  def inFragment[F[_], A](
      fragmentName: String,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      caret: Option[Caret]
  )(
      faf: Pos[P.FragmentDefinition] => F[A]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
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

  sealed trait SimplifiedType[G[_]] { def selections: List[SelectionInfo[G]] }
  object SimplifiedType {
    final case class List[G[_]](t: SimplifiedType[G]) extends SimplifiedType[G] { def selections = t.selections }
    final case class Option[G[_]](t: SimplifiedType[G]) extends SimplifiedType[G] { def selections = t.selections }
    final case class Scalar[G[_]](name: String) extends SimplifiedType[G] { def selections = Nil }
    final case class Enum[G[_]](name: String) extends SimplifiedType[G] { def selections = Nil }
    final case class Selectable[G[_]](name: String, selection: NonEmptyList[SelectionInfo[G]]) extends SimplifiedType[G] {
      def selections = selection.toList
    }
  }

  def getSimplifiedTypeString[G[_]](st: SimplifiedType[G], inOption: Boolean = false): String = {
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

  final case class FieldInfo[G[_]](
      name: String,
      alias: Option[String],
      args: Option[P.Arguments],
      tpe: SimplifiedType[G],
      caret: Caret,
      path: PrepCursor
  ) {
    lazy val outputName: String = alias.getOrElse(name)
  }

  object FieldInfo {
    def apply[F[_]: Functor, G[_]](name: String, alias: Option[String], args: Option[P.Arguments], tpe: SimplifiedType[G], caret: Caret)(
        implicit L: Local[F, Prep]
    ): F[FieldInfo[G]] =
      L.ask.map(_.cursor).map(FieldInfo(name, alias, args, tpe, caret, _))
  }

  def collectFieldInfo[F[_]: Parallel, G[_]](
      af: AbstractField[G, ?],
      f: P.Field,
      caret: Caret,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[FieldInfo[G]] = ambientField(f.name) {
    // Verify arguments by decoding them
    val decF = af.arg.traverse_ { case a: Arg[a] => decodeFieldArgs[F, G, a](a, f.arguments, variableMap).void }

    // Verify subselection
    val c = f.selectionSet.caret
    def verifySubsel(t: Out[G, ?]): F[SimplifiedType[G]] =
      (t, f.selectionSet.value) match {
        case (arr: OutArr[G, ?, ?, ?], _) => verifySubsel(arr.of).map(SimplifiedType.List(_))
        case (opt: OutOpt[G, ?, ?], _)    => verifySubsel(opt.of).map(SimplifiedType.Option(_))
        case (ol: Selectable[G, ?], Some(ss)) =>
          collectSelectionInfo[F, G](ol, ss, variableMap, fragments, discoveryState)
            .map(xs => SimplifiedType.Selectable(ol.name, xs))
        case (e: Enum[?], None)   => F.pure(SimplifiedType.Enum(e.name))
        case (s: Scalar[?], None) => F.pure(SimplifiedType.Scalar(s.name))
        case (o, Some(_))         => raise(s"Type `${friendlyName(o)}` cannot have selections.", Some(c))
        case (o, None)            => raise(s"Object like type `${friendlyName(o)}` must have a selection.", Some(c))
      }

    decF &> verifySubsel(af.output.value).flatMap(FieldInfo[F, G](f.name, f.alias, f.arguments, _, caret))
  }

  final case class SelectionInfo[G[_]](
      s: Selectable[G, ?],
      fields: NonEmptyList[FieldInfo[G]],
      fragmentName: Option[String]
  )
  def collectSelectionInfo[F[_]: Parallel, G[_]](
      s: Selectable[G, ?],
      ss: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[SelectionInfo[G]]] = D.defer {
    val fields = ss.selections.collect { case Pos(caret, P.Selection.FieldSelection(field)) => (caret, field) }

    val actualFields: Map[String, AbstractField[G, ?]] =
      s.abstractFieldMap + ("__typename" -> AbstractField(None, Eval.now(stringScalar), None))

    val validateFieldsF: F[List[SelectionInfo[G]]] = fields
      .parTraverse { case (caret, field) =>
        actualFields.get(field.name) match {
          case None    => raise[F, FieldInfo[G]](s"Field '${field.name}' is not a member of `${s.name}`.", Some(caret))
          case Some(f) => collectFieldInfo[F, G](f, field, caret, variableMap, fragments, discoveryState)
        }
      }
      .map(_.toNel.toList.map(SelectionInfo(s, _, None)))

    val realInlines =
      ss.selections
        .collect { case Pos(caret, P.Selection.InlineFragmentSelection(f)) => (caret, f) }
        .parFlatTraverse { case (caret, f) =>
          f.typeCondition.traverse(matchType[F, G](_, s, caret, discoveryState)).map(_.getOrElse(s)).flatMap { t =>
            collectSelectionInfo[F, G](t, f.selectionSet, variableMap, fragments, discoveryState).map(_.toList)
          }
        }

    val realFragments =
      ss.selections
        .collect { case Pos(caret, P.Selection.FragmentSpreadSelection(f)) => (caret, f) }
        .parFlatTraverse { case (caret, f) =>
          val fn = f.fragmentName
          inFragment(fn, fragments, caret.some) { case Pos(caret, f) =>
            matchType[F, G](f.typeCnd, s, caret, discoveryState).flatMap { t =>
              collectSelectionInfo[F, G](t, f.selectionSet, variableMap, fragments, discoveryState)
                .map(_.toList.map(_.copy(fragmentName = Some(fn))))
            }
          }
        }

    (validateFieldsF :: realInlines :: realFragments :: Nil).parFlatSequence
      // Unfortunate, but is always safe here since nel is input
      .map(_.toNel.get)
  }

  def fieldName[G[_]](f: FieldInfo[G]): String =
    s"'${f.alias.getOrElse(f.name)}'${f.alias.map(x => s" (alias for '$x')").mkString}"

  def compareValues[F[_]: Parallel](av: P.Value, bv: P.Value, caret: Option[Caret])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[Unit] = {
    (av, bv) match {
      case (P.Value.VariableValue(avv), P.Value.VariableValue(bvv)) =>
        if (avv === bvv) F.unit
        else raise[F, Unit](s"Variable '$avv' and '$bvv' are not equal.", caret)
      case (P.Value.IntValue(ai), P.Value.IntValue(bi)) =>
        if (ai === bi) F.unit
        else raise[F, Unit](s"Int '$ai' and '$bi' are not equal.", caret)
      case (P.Value.FloatValue(af), P.Value.FloatValue(bf)) =>
        if (af === bf) F.unit
        else raise[F, Unit](s"Float '$af' and '$bf' are not equal.", caret)
      case (P.Value.StringValue(as), P.Value.StringValue(bs)) =>
        if (as === bs) F.unit
        else raise[F, Unit](s"String '$as' and '$bs' are not equal.", caret)
      case (P.Value.BooleanValue(ab), P.Value.BooleanValue(bb)) =>
        if (ab === bb) F.unit
        else raise[F, Unit](s"Boolean '$ab' and '$bb' are not equal.", caret)
      case (P.Value.EnumValue(ae), P.Value.EnumValue(be)) =>
        if (ae === be) F.unit
        else raise[F, Unit](s"Enum '$ae' and '$be' are not equal.", caret)
      case (P.Value.NullValue, P.Value.NullValue) => F.unit
      case (P.Value.ListValue(al), P.Value.ListValue(bl)) =>
        if (al.length === bl.length) {
          al.zip(bl).zipWithIndex.parTraverse_ { case ((a, b), i) => ambientIndex(i)(compareValues[F](a, b, caret)) }
        } else
          raise[F, Unit](s"Lists are not af same size. Found list of length ${al.length} versus list of length ${bl.length}.", caret)
      case (P.Value.ObjectValue(ao), P.Value.ObjectValue(bo)) =>
        if (ao.length =!= bo.length)
          raise[F, Unit](
            s"Objects are not af same size. Found object of length ${ao.length} versus object of length ${bo.length}.",
            caret
          )
        else {
          def checkUniqueness(xs: List[(String, P.Value)]) =
            xs.groupMap { case (k, _) => k } { case (_, v) => v }
              .toList
              .parTraverse {
                case (k, v :: Nil) => F.pure(k -> v)
                case (k, _)        => raise[F, (String, P.Value)](s"Key '$k' is not unique in object.", caret)
              }
              .map(_.toMap)

          (checkUniqueness(ao), checkUniqueness(bo)).parTupled.flatMap { case (amap, bmap) =>
            // TODO test that verifies that order does not matter
            (amap align bmap).toList.parTraverse_ {
              case (k, Ior.Left(_))    => raise[F, Unit](s"Key '$k' is missing in object.", caret)
              case (k, Ior.Right(_))   => raise[F, Unit](s"Key '$k' is missing in object.", caret)
              case (k, Ior.Both(l, r)) => ambientArg(k)(compareValues[F](l, r, caret))
            }
          }
        }
      case _ => raise[F, Unit](s"Values are not same type, got ${pValueName(av)} and ${pValueName(bv)}.", caret)
    }
  }

  def compareArguments[F[_]: Parallel](name: String, aa: P.Arguments, ba: P.Arguments, caret: Option[Caret])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ) = {
    def checkUniqueness(x: P.Arguments): F[Map[String, QueryParser.Argument]] =
      x.nel.toList
        .groupBy(_.name)
        .toList
        .parTraverse {
          case (k, v :: Nil) => F.pure(k -> v)
          case (k, _) =>
            raise[F, (String, P.Argument)](s"Argument '$k' of field $name was not unique.", caret)
        }
        .map(_.toMap)

    (checkUniqueness(aa), checkUniqueness(ba)).parTupled.flatMap { case (amap, bmap) =>
      (amap align bmap).toList.parTraverse_ {
        case (k, Ior.Left(_)) =>
          raise[F, Unit](s"Field $name is already selected with argument '$k', but no argument was given here.", caret)
        case (k, Ior.Right(_)) =>
          raise[F, Unit](s"Field $name is already selected without argument '$k', but an argument was given here.", caret)
        case (k, Ior.Both(l, r)) => ambientArg(k)(compareValues[F](l.value, r.value, caret))
      }
    }
  }

  /*
   * Put into simpler more explicit terms:
   * "shape identical" means that for two fields f1 and f2:
   *   * if f1's type is a list then f2's type must also be a list. The inner types must be "shape identical".
   *   * if f1's type is a non-null then f2's type must also be a non-null. The inner types must be "shape identical".
   *   * if f1's type is a terminal type then f2's type must also be a terminal type and they must be the same.
   *   * if f1's type is selectable then f2's type must also be selectable.
   *     for every field f1' in f1's type and f2' in f2's type that share the same name (alias or not),
   *     f1' and f2' must be "shape identical".
   * "fully identical" means that two fields must:
   *   * Have the same parameters
   *   * The same underlying field names
   *   * Be "shape identical".
   *
   * For any two fields f1 and f2 defined on two possibly different types t1 and t2
   * with the same name (alisaed or not), the following must hold:
   *  - If t1 = t2 then f1 and f2 must be "fully identical".
   *  - If t1 is an Object and t2 is an Object but t1 != t2 (fully disjoint), f1 and f2 must be "shape identical".
   *  - If t1 or t2 is not an Object (trivially true since this is the only remaining case)
   *    then f1 and f2 "fully identical". Elaboration:
   *      It is a lazy way of ensuring that in the instance that the two types are the same,
   *      the fields are "fully identical".
   *      A through algorithm would check if the intersection of subtypes of the two abstract types was empty and
   *      thereby conclude wether the fields were to be "fully identical" or "shape identical".
   *
   * This algorithm runs through the tree two times for every branch of the tree.
   * It can quickly degrade into something akin O(2^n).
   *
   * Instead, we can implement this algorithm in a single pass while keeping it simple.
   * Since "fully identical" is a stricter requirement than "shape identical", we can simply handle the
   * case in the selection part of the algorithm.
   *
   * Furthermore, the check in the algorithm has two cases for the whole set of fields:
   * If a non-object type is encountered, then all fields must be "fully identical".
   * If an object type is encountered, then group all fields by name and do "full identical" checks.
   *
   * Optimization:
   * We can check more than two fields at once by passing in a comparison field and a non-empty list of fields.
   */

  // https://spec.graphql.org/draft/#sec-Field-Selection-Merging.Formal-Specification
  def checkFieldsMerge[F[_]: Parallel, G[_]](a: FieldInfo[G], asi: SelectionInfo[G], b: FieldInfo[G], bsi: SelectionInfo[G])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ) = {
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
  }

  def mergeSelections[F[_]: Parallel, G[_]](xs: NonEmptyList[SelectionInfo[G]])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ) = {
    val ys: NonEmptyMap[String, NonEmptyList[(SelectionInfo[G], FieldInfo[G])]] =
      xs.flatMap(si => si.fields tupleLeft si)
        .groupByNem { case (_, f) => f.outputName }

    // For every field:
  }

  def mergeTypes[F[_]: Parallel, G[_]](xs: NonEmptyList[(FieldInfo[G], SelectionInfo[G])], caret: Caret)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ) = {
    val (hd, _) = xs.head
  }

  def checkSelectionsMerge[F[_]: Parallel, G[_]](xs: NonEmptyList[SelectionInfo[G]])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[Unit] = {
    val ys: NonEmptyList[NonEmptyList[(SelectionInfo[G], FieldInfo[G])]] =
      xs.flatMap(si => si.fields tupleLeft si)
        .groupByNem { case (_, f) => f.outputName }
        .toNel
        .map { case (_, v) => v }

    ys.parTraverse_ { zs =>
      val mergeFieldsF = {
        val (siHead, fiHead) = zs.head
        zs.tail.parTraverse_ { case (si, fi) => checkFieldsMerge[F, G](fiHead, siHead, fi, si) }
      }

      mergeFieldsF >>
        zs.toList.flatMap { case (_, fi) => fi.tpe.selections }.toNel.traverse_(checkSelectionsMerge[F, G])
    }
  }

  // Optimization: we don't check selections recursively since checkSelectionsMerge traverses the whole tree
  // We only need to check the immidiate children and will eventually have checked the whole tree
  def checkSimplifiedTypeShape[F[_]: Parallel, G[_]](a: SimplifiedType[G], b: SimplifiedType[G], caret: Caret)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[Unit] = {
    (a, b) match {
      case (SimplifiedType.List(l), SimplifiedType.List(r))     => checkSimplifiedTypeShape[F, G](l, r, caret)
      case (SimplifiedType.Option(l), SimplifiedType.Option(r)) => checkSimplifiedTypeShape[F, G](l, r, caret)
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
        else raise[F, Unit](s"Enums are not the same, got '$l' and '$r'.", Some(caret))
      case (SimplifiedType.Scalar(l), SimplifiedType.Scalar(r)) =>
        if (l === r) F.unit
        else raise[F, Unit](s"Scalars are not the same, got '$l' and '$r'.", Some(caret))
      case _ =>
        raise[F, Unit](s"Types are not the same, got ${getSimplifiedTypeString(a)} and ${getSimplifiedTypeString(b)}.", Some(caret))
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

  final case class PairedFieldSelection[G[_], A](
      info: FieldInfo[G],
      field: Field[G, A, ?]
  )
  final case class MergedImplementation[G[_], A, B](
      leaf: Type[G, B],
      selections: NonEmptyList[PairedFieldSelection[G, B]],
      specify: A => Option[B]
  )
  def mergeImplementations[F[_]: Parallel, G[_], A](
      base: Selectable[G, A],
      sels: NonEmptyList[SelectionInfo[G]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[NonEmptyList[MergedImplementation[G, A, ?]]] = {
    val concreteBaseMap = findImplementations[G, A](base, discoveryState).map(x => x.tpe.name -> x).toMap
    val concreteBase = concreteBaseMap.toList

    val nestedSelections = sels.toList.flatMap { sel =>
      val concreteIntersections = findImplementations(sel.s, discoveryState)
        .map { case FoundImplementation(t, _) => t.name }

      concreteIntersections tupleRight sel.fields
    }

    val grouped = nestedSelections
      .groupMap { case (k, _) => k } { case (_, vs) => vs }
      .collect { case (k, x :: xs) =>
        // Merge bug
        k -> NonEmptyList(x, xs).flatten.map(x => x.outputName -> x).toNem.toNonEmptyList
      }

    val collected: F[List[MergedImplementation[G, A, ?]]] = concreteBase.parFlatTraverse { case (k, (fi: FoundImplementation[G, A, b])) =>
      val t = fi.tpe
      val specify = fi.specify
      grouped.get(k).toList.traverse { fields =>
        fields
          .parTraverse { f =>
            if (f.name === "__typename") F.pure(PairedFieldSelection[G, b](f, typenameField[b](t.name)))
            else {
              t.fieldMap.get(f.name) match {
                case None        => raise[F, PairedFieldSelection[G, b]](s"Could not find field '${f.name}' on type `${t.name}`.", None)
                case Some(field) => F.pure(PairedFieldSelection[G, b](f, field))
              }
            }
          }
          .map(fields => MergedImplementation[G, A, b](t, fields, specify))
      }
    }

    collected.flatMap { xs =>
      xs.toNel match {
        case Some(x) => F.pure(x)
        case None =>
          raise[F, NonEmptyList[MergedImplementation[G, A, ?]]](
            s"Could not find any implementations of `${base.name}` in the selection set.",
            None
          )
      }
    }
  }

  def decodeFieldArgs[F[_]: Parallel, G[_], A](
      a: Arg[A],
      args: Option[P.Arguments],
      variableMap: VariableMap
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] = {
    val provided = args.toList.flatMap(_.nel.toList)

    // Treat input arguments as an object
    // Decode the args as-if an input
    val argObj =
      P.Value.ObjectValue(provided.map(a => a.name -> a.value))

    // a match {
    //   case PureArg(value) if provided.isEmpty => value.fold(raise(_, None), F.pure(_))
    //   case PureArg(_) =>
    //     raise(s"Field '$name' does not accept arguments.", Some(caret))
    // case nea @ NonEmptyArg(_, _) =>
    parseInputObj[F, A](argObj, a, Some(variableMap), ambigiousEnum = false)
    // }
  }

  def prepareField[F[_]: Parallel, G[_]: Applicative, I, T](
      fi: FieldInfo[G],
      field: Field[G, I, T],
      currentTypename: String,
      variableMap: VariableMap,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[State[Int, PreparedDataField[G, I]]] = D.defer {
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

    def compileCont[A](t: Out[G, A], cursor: UniqueEdgeCursor): Used[F, State[Int, Prepared[G, A]]] =
      (t, fi.tpe.selections.toNel) match {
        case (out: gql.ast.OutArr[g, a, c, b], _) =>
          val innerStep: Step[G, a, b] = out.resolver.underlying
          val nc = cursor append "array"
          val compiledStep = compileStep[F, G, a, b](innerStep, nc, meta)
          val compiledCont = compileCont[b](out.of, nc)
          (compiledStep, compiledCont)
            .parMapN((_, _).tupled)
            .map(_.map { case (s, c) =>
              PreparedList(PreparedCont(s, c), out.toSeq)
            })
        case (out: gql.ast.OutOpt[g, a, b], _) =>
          val innerStep: Step[G, a, b] = out.resolver.underlying
          val nc = cursor append "option"
          val compiledStep = compileStep[F, G, a, b](innerStep, nc, meta)
          val compiledCont = compileCont[b](out.of, nc)
          (compiledStep, compiledCont)
            .parMapN((_, _).tupled)
            .map(_.map { case (s, c) =>
              PreparedOption(PreparedCont(s, c))
            })
        case (s: Selectable[G, a], Some(ss)) =>
          Used.liftF {
            prepareSelectable[F, G, a](s, ss, variableMap, discoveryState)
              .map(_.map(Selection(_)))
          }
        case (e: Enum[a], None) =>
          Used[F].pure(State.pure(PreparedLeaf(e.name, x => Json.fromString(e.revm(x)))))
        case (s: Scalar[a], None) =>
          Used[F].pure(State.pure(PreparedLeaf(s.name, x => s.encoder(x).asJson)))
        case (o, Some(_)) =>
          Used.liftF(raise(s"Type `${friendlyName(o)}` cannot have selections.", Some(selCaret)))
        case (o, None) =>
          Used.liftF(raise(s"Object like type `${friendlyName(o)}` must have a selection.", Some(selCaret)))
      }

    val usedF = (
      compileStep[F, G, I, T](step, rootUniqueName, meta),
      compileCont(tpe, rootUniqueName)
    ).parMapN((_, _).tupled)
      .map(_.map { case (s, c) =>
        val pc = PreparedCont(s, c)
        PreparedDataField(name, fi.alias, pc)
      })

    usedF.run
      .flatMap { case (used, a) =>
        val provided = fi.args.toList.flatMap(_.nel.toList).map(_.name).toSet
        val tooMany = provided -- used
        if (tooMany.isEmpty) F.pure(a)
        else raise(s"Field '$name' does not accept the arguments ${tooMany.map(s => s"'$s'").toList.mkString_(", ")}", Some(selCaret))
      }
  }

  def prepareSelectable[F[_]: Parallel, G[_], A](
      s: Selectable[G, A],
      sis: NonEmptyList[SelectionInfo[G]],
      variableMap: VariableMap,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[State[Int, NonEmptyList[PreparedSpecification[G, A, ?]]]] = {
    mergeImplementations[F, G, A](s, sis, discoveryState).flatMap { impls =>
      impls
        .parTraverse[F, State[Int, PreparedSpecification[G, A, ?]]] { case impl: MergedImplementation[G, A, b] =>
          val fa: F[NonEmptyList[State[Int, PreparedDataField[G, b]]]] = impl.selections.parTraverse { sel =>
            sel.field match {
              case field: Field[G, b2, t] =>
                prepareField[F, G, b, t](sel.info, field, impl.leaf.name, variableMap, discoveryState)
            }
          }

          fa.map(_.sequence.map(xs => PreparedSpecification[G, A, b](s.name, impl.specify, xs)))
        }
        .map(_.sequence)
    }
  }

  def prepareSelectableRoot[F[_]: Parallel, G[_], A](
      s: Selectable[G, A],
      ss: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[PreparedSpecification[G, A, ?]]] = {
    collectSelectionInfo[F, G](s, ss, variableMap, fragments, discoveryState).flatMap { root =>
      checkSelectionsMerge[F, G](root) >>
        prepareSelectable[F, G, A](s, root, variableMap, discoveryState)
          .map(_.runA(0).value)
    }
  }

  type VariableMap = Map[String, Either[P.Value, Json]]

  def matchType[F[_], G[_]](
      name: String,
      sel: Selectable[G, ?],
      caret: Caret,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit F: MonadError[F, NonEmptyChain[PositionalError]], L: Local[F, Prep]): F[Selectable[G, ?]] =
    if (sel.name == name) F.pure(sel)
    else {
      sel match {
        case t @ Type(n, _, _, _) =>
          // Check downcast
          t.implementsMap.get(name).map(_.value) match {
            case None    => raise(s"Tried to match with type `$name` on type object type `$n`.", Some(caret))
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
                caret.some
              ).flatMap { m =>
                raiseOpt(
                  m.get(name).map {
                    case t: SchemaShape.InterfaceImpl.TypeImpl[G @unchecked, ?, ?]    => t.t
                    case i: SchemaShape.InterfaceImpl.OtherInterface[G @unchecked, ?] => i.i
                  },
                  s"`$name` does not implement interface `$n`, possible implementations are ${m.keySet.mkString(", ")}.",
                  caret.some
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
                    caret.some
                  )
                case Some(x) => F.pure(x.value)
              }
          }
      }
    }

  def pValueName(v: P.Value): String =
    v match {
      case ObjectValue(_)       => "object"
      case StringValue(_)       => "string"
      case ListValue(_)         => "list"
      case P.Value.EnumValue(_) => "enum"
      case BooleanValue(_)      => "boolean"
      case NullValue            => "null"
      case FloatValue(_)        => "float"
      case IntValue(_)          => "int"
      case VariableValue(_)     => "variable"
    }

  def inName[A](in: In[A], inOption: Boolean = false): String = {
    val suffix = if (inOption) "" else "!"
    val rec = (in: @unchecked) match {
      case InArr(of, _)          => s"[${inName(of)}]"
      case Enum(name, _, _)      => name
      case Scalar(name, _, _, _) => name
      case InOpt(of)             => s"${inName(of, inOption = true)}"
      case Input(name, _, _)     => name
    }
    rec + suffix
  }

  def parseInputObj[F[_]: Parallel, A](
      v: P.Value.ObjectValue,
      fields: Arg[A],
      variableMap: Option[VariableMap],
      ambigiousEnum: Boolean
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[A] = {
    val xs = v.v

    val m = xs.toMap
    val required = fields.entries.map(x => x.name -> x).toList.toMap

    // All provided fields are defined
    val tooMuch = m.keySet -- required.keySet
    val tooMuchF =
      if (tooMuch.isEmpty) F.unit
      else raise[F, Unit](s"Too many fields provided, unknown fields are ${tooMuch.toList.map(x => s"'$x'").mkString_(", ")}.", None)

    tooMuchF >> parseArg[F, A](fields, m, variableMap, ambigiousEnum)
  }

  def parseInput[F[_]: Parallel, A](v: P.Value, tpe: In[A], variableMap: Option[VariableMap], ambigiousEnum: Boolean)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[A] =
    (tpe, v) match {
      case (_, P.Value.VariableValue(v)) =>
        variableMap match {
          case None => raise(s"Variables may not occur here. Variable '$$$v' was provided.", None)
          case Some(vm) =>
            vm.get(v) match {
              case None =>
                raise(
                  s"Variable '$$$v' was not declared and provided as a possible variable for this operation. Hint add the variable to the variables list of the operation '(..., $$$v: ${inName(tpe)})' and provide a value in the variables parameter.",
                  None
                )
              case Some(Left(pval)) => parseInput[F, A](pval, tpe, None, ambigiousEnum = false)
              case Some(Right(j)) =>
                val asPVal = valueToParserValue(Value.fromJson(j))
                parseInput[F, A](asPVal, tpe, None, ambigiousEnum = true)
            }
        }
      case (e @ Enum(name, _, _), v) =>
        ambientInputType(name) {
          val fa: F[String] = v match {
            case P.Value.EnumValue(s)                    => F.pure(s)
            case P.Value.StringValue(s) if ambigiousEnum => F.pure(s)
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
      case (Scalar(name, _, decoder, _), x) =>
        ambientInputType(name) {
          parserValueToValue[F](x).flatMap(x => raiseEither(decoder(x), None))
        }
      case (Input(name, fields, _), o: P.Value.ObjectValue) =>
        ambientInputType(name) {
          parseInputObj[F, A](o, fields, variableMap, ambigiousEnum)
        }
      case (arr: InArr[a, c], P.Value.ListValue(xs)) =>
        xs.zipWithIndex
          .parTraverse { case (x, i) =>
            ambientIndex(i) {
              parseInput[F, a](x, arr.of, variableMap, ambigiousEnum)
            }
          }
          .flatMap(arr.fromSeq(_).fold(raise(_, None), F.pure(_)))
      case (_: InOpt[a], P.Value.NullValue) => F.pure(Option.empty[a])
      case (opt: InOpt[a], x)               => parseInput[F, a](x, opt.of, variableMap, ambigiousEnum).map(Some(_))
      case (i, _)                           => raise(s"Expected type `${inName(i)}`, but got value ${pValueName(v)}.", None)
    }

  def parseArgValue[F[_]: Parallel, A](
      a: ArgValue[A],
      input: Map[String, P.Value],
      variableMap: Option[VariableMap],
      ambigiousEnum: Boolean
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[ArgParam[A]] = {
    val fa: F[ArgParam[P.Value]] = input.get(a.name) match {
      case Some(x) => F.pure(ArgParam(defaulted = false, x))
      case None =>
        a.defaultValue match {
          // TODO this value being parsed can probably be cached, since the default is the same for every query
          case Some(dv) => F.pure(ArgParam(defaulted = true, valueToParserValue(dv)))
          case None =>
            a.input.value match {
              case InOpt(_) => F.pure(ArgParam(defaulted = false, P.Value.NullValue))
              case _ =>
                raise[F, ArgParam[P.Value]](s"Required input for field '${a.name}' was not provided and has no default value.", None)
            }
        }
    }

    ambientArg(a.name) {
      fa.flatMap(ap => parseInput[F, A](ap.value, a.input.value, variableMap, ambigiousEnum).map(x => ap.copy(value = x)))
    }
  }

  def parseArg[F[_]: Parallel, A](arg: Arg[A], input: Map[String, P.Value], variableMap: Option[VariableMap], ambigiousEnum: Boolean)(
      implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[A] = {
    // All provided fields are of the correct type
    // All required fields are either defiend or defaulted
    val fieldsF: F[NonEmptyChain[(String, ArgParam[?])]] =
      arg.entries.parTraverse { case a: ArgValue[a] =>
        parseArgValue[F, a](
          a,
          input,
          variableMap,
          ambigiousEnum
        ).map(a.name -> _)
      }

    fieldsF
      .map(_.toList.toMap)
      .flatMap(arg.decode(_).fold(raise(_, None), F.pure(_)))
  }

  def parserValueToValue[F[_]: Parallel](v: P.Value)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      L: Local[F, Prep]
  ): F[Value] =
    v match {
      case NullValue            => F.pure(Value.NullValue)
      case FloatValue(v)        => F.pure(Value.FloatValue(v))
      case P.Value.EnumValue(v) => F.pure(Value.EnumValue(v))
      case ListValue(v) =>
        v.toVector.parTraverse(parserValueToValue[F]).map(Value.ArrayValue(_))
      case IntValue(v)      => F.pure(Value.IntValue(v))
      case VariableValue(v) => raise[F, Value](s"Variable '$$$v' may not occur here.", None)
      case ObjectValue(v) =>
        v.parTraverse { case (k, v) =>
          parserValueToValue[F](v).tupleLeft(k)
        }.map(xs => Value.ObjectValue(xs.toMap))
      case BooleanValue(v) => F.pure(Value.BooleanValue(v))
      case StringValue(v)  => F.pure(Value.StringValue(v))
    }

  def valueToParserValue(v: Value): P.Value =
    v match {
      case Value.BooleanValue(v)     => P.Value.BooleanValue(v)
      case Value.StringValue(v)      => P.Value.StringValue(v)
      case Value.IntValue(v)         => P.Value.IntValue(v)
      case Value.ObjectValue(fields) => P.Value.ObjectValue(fields.toList.map { case (k, v) => k -> valueToParserValue(v) })
      case Value.ArrayValue(v)       => P.Value.ListValue(v.toList.map(valueToParserValue))
      case Value.EnumValue(v)        => P.Value.EnumValue(v)
      case Value.NullValue           => P.Value.NullValue
      case Value.FloatValue(v)       => P.Value.FloatValue(v)
    }

  def getOperationDefinition[F[_]](
      ops: List[Pos[P.OperationDefinition]],
      operationName: Option[String]
  )(implicit F: MonadError[F, (String, List[Caret])]): F[P.OperationDefinition] = {
    lazy val possible = ops
      .map(_.value)
      .collect { case d: P.OperationDefinition.Detailed => d.name }
      .collect { case Some(x) => s"'$x'" }
      .mkString(", ")
    (ops, operationName) match {
      case (Nil, _)      => F.raiseError((s"No operations provided.", Nil))
      case (x :: Nil, _) => F.pure(x.value)
      case (xs, _) if xs.exists {
            case Pos(_, _: P.OperationDefinition.Simple)                     => true
            case Pos(_, x: P.OperationDefinition.Detailed) if x.name.isEmpty => true
            case _                                                           => false
          } =>
        F.raiseError(
          (s"Exactly one operation must be suplied if the operations include at least one unnamed operation.", xs.map(_.caret))
        )
      case (xs, None) =>
        F.raiseError(
          (s"Operation name must be supplied when supplying multiple operations, provided operations are $possible."),
          xs.map(_.caret)
        )
      case (xs, Some(name)) =>
        val o = xs.collectFirst { case Pos(_, d: P.OperationDefinition.Detailed) if d.name.contains(name) => d }
        F.fromOption(o, (s"Unable to find operation '$name', provided possible operations are $possible.", xs.map(_.caret)))
    }
  }

  def operationType(od: P.OperationDefinition) =
    od match {
      case P.OperationDefinition.Simple(_)             => P.OperationType.Query
      case P.OperationDefinition.Detailed(ot, _, _, _) => ot
    }

  sealed trait PrepResult[G[_], Q, M, S]
  object PrepResult {
    final case class Query[G[_], Q, M, S](query: NonEmptyList[PreparedField[G, Q]]) extends PrepResult[G, Q, M, S]
    final case class Mutation[G[_], Q, M, S](mutation: NonEmptyList[PreparedField[G, M]]) extends PrepResult[G, Q, M, S]
    final case class Subscription[G[_], Q, M, S](subscription: NonEmptyList[PreparedField[G, S]]) extends PrepResult[G, Q, M, S]
  }

  // TODO add another phase after finding the OperationDefinition and before this,
  // that checks all that variables have been used
  def prepareParts[F[_]: Parallel, G[_]: Applicative, Q, M, S](
      op: P.OperationDefinition,
      frags: List[Pos[P.FragmentDefinition]],
      schema: Schema[G, Q, M, S],
      variableMap: Map[String, Json]
  )(implicit
      L: Local[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[PrepResult[G, Q, M, S]] = {
    val ot = operationType(op)

    val preCheckVariablesF: F[VariableMap] = op match {
      case P.OperationDefinition.Simple(_) => F.pure(Map.empty)
      case P.OperationDefinition.Detailed(_, _, vdsO, _) =>
        vdsO.toList
          .flatMap(_.nel.toList)
          .parTraverse { case Pos(caret, vd) =>
            val defaultWithFallback = vd.defaultValue.orElse(vd.tpe match {
              case P.Type.NonNull(_) => None
              case _                 => Some(P.Value.NullValue)
            })

            def printType(t: P.Type, inOption: Boolean = false): String = {
              val suffix = if (inOption) "" else "!"
              val prefix = t match {
                case P.Type.List(of)    => s"[${printType(of)}]"
                case P.Type.Named(n)    => n
                case P.Type.NonNull(of) => printType(of, inOption = true)
              }
              prefix + suffix
            }

            val rootValueF: F[Either[P.Value, Json]] = (variableMap.get(vd.name), defaultWithFallback) match {
              case (None, Some(d)) => F.pure(Left(d))
              case (Some(x), _)    => F.pure(Right(x))
              case (None, None) =>
                raise(
                  s"Variable '$$${vd.name}' is required but was not provided. Hint: Provide variable or a default value for '$$${vd.name}' of type `${printType(vd.tpe)}`.",
                  Some(caret)
                )
            }

            def verify(currentType: P.Type, currentValue: Either[P.Value, Json], inOption: Boolean): F[Unit] = {
              (currentType, currentValue) match {
                case (P.Type.Named(name), v) =>
                  v match {
                    case Left(P.Value.ListValue(_)) =>
                      raise(
                        s"Expected a value of type `$name` when checking the default value of '$$${vd.name}', found list instead.",
                        Some(caret)
                      )
                    case Left(P.Value.NullValue) if !inOption =>
                      raise(
                        s"Expected a non-nullable value of type `$name` when checking the default value of '$$${vd.name}', found null instead.",
                        Some(caret)
                      )

                    case Right(j) if j.isArray =>
                      raise(
                        s"Expected a value of type `$name` when checking the variable input of '$$${vd.name}', found list instead.",
                        Some(caret)
                      )
                    case Right(j) if j.isNull && !inOption =>
                      raise(
                        s"Expected a non-nullable value of type `$name` when checking the variable input of '$$${vd.name}', found null instead.",
                        Some(caret)
                      )
                    case _ => F.unit
                  }
                case (P.Type.List(of), v) =>
                  v match {
                    case Left(P.Value.ListValue(vs)) =>
                      vs.zipWithIndex.parTraverse { case (v, i) =>
                        ambientIndex(i) {
                          verify(of, Left(v), inOption = true)
                        }
                      }.void
                    case Left(P.Value.NullValue) =>
                      if (inOption) F.unit
                      else
                        raise(
                          s"Expected a non-nullable value of type list when checking the default value of '$$${vd.name}', found null instead.",
                          Some(caret)
                        )
                    case Left(p) =>
                      raise(
                        s"Expected a value of type list when checking the default value of '$$${vd.name}', found a graphql value of type ${pValueName(p)}} instead.",
                        Some(caret)
                      )
                    case Right(j) =>
                      if (j.isNull) {
                        if (inOption) F.unit
                        else
                          raise(
                            s"Expected a non-nullable value of type list when checking the variable input of '$$${vd.name}', found null instead.",
                            Some(caret)
                          )
                      } else {
                        j.asArray match {
                          case None =>
                            raise(
                              s"Expected a value of type list when checking the variable input of '$$${vd.name}', found a json value of type ${j.name} instead.",
                              Some(caret)
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
                case (P.Type.NonNull(of), v) => verify(of, v, inOption = false)
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
      case P.OperationDefinition.Simple(sel)            => sel
      case P.OperationDefinition.Detailed(_, _, _, sel) => sel
    }

    preCheckVariablesF.flatMap[PrepResult[G, Q, M, S]] { vm =>
      def runWith[A](o: Type[G, A]): F[NonEmptyList[PreparedSpecification[G, A, _]]] =
        prepareSelectableRoot[F, G, A](
          o,
          selection,
          vm,
          frags.map(f => f.value.name -> f).toMap,
          schema.shape.discover
        )

      ot match {
        // We sneak the introspection query in here
        case P.OperationType.Query =>
          val i: NonEmptyList[(String, Field[G, Unit, ?])] = schema.shape.introspection
          val q = schema.shape.query
          val full = q.copy(fields = i.map { case (k, v) => k -> v.contramap[G, Q](_ => ()) } concatNel q.fields)
          runWith[Q](full).map(PrepResult.Query(_))
        case P.OperationType.Mutation =>
          raiseOpt(schema.shape.mutation, "No `Mutation` type defined in this schema.", None)
            .flatMap(runWith[M])
            .map(PrepResult.Mutation(_))
        case P.OperationType.Subscription =>
          raiseOpt(schema.shape.subscription, "No `Subscription` type defined in this schema.", None)
            .flatMap(runWith[S])
            .map(PrepResult.Subscription(_))
      }
    }
  }

  type H[A] = Kleisli[EitherT[Eval, NonEmptyChain[PositionalError], *], Prep, A]

  def prepare[F[_]: Applicative, Q, M, S](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, Q, M, S],
      variableMap: Map[String, Json],
      operationName: Option[String]
  ): EitherNec[PositionalError, PrepResult[F, Q, M, S]] = {
    val (ops, frags) = executabels.toList.partitionEither {
      case P.ExecutableDefinition.Operation(op)  => Left(op)
      case P.ExecutableDefinition.Fragment(frag) => Right(frag)
    }

    getOperationDefinition[Either[(String, List[Caret]), *]](ops, operationName) match {
      case Left((e, carets)) => Left(NonEmptyChain.one(PositionalError(PrepCursor.empty, carets, e)))
      case Right(op) =>
        prepareParts[H, F, Q, M, S](op, frags, schema, variableMap)
          .run(Prep.empty)
          .value
          .value
    }
  }
}
