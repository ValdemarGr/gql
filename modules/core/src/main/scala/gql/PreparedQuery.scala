/*
 * Copyright 2022 Valdemar Grange
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
import cats.arrow.FunctionK
import gql.parser.QueryParser

object PreparedQuery {
  sealed trait Prepared[F[_], A]

  sealed trait PreparedField[F[_], A]

  final case class PreparedDataField[F[_], I, T](
      id: Int,
      name: String,
      alias: Option[String],
      cont: PreparedCont[F]
  ) extends PreparedField[F, I]

  final case class PreparedSpecification[F[_], A](
      id: Int,
      typename: String,
      specify: Any => Option[A],
      selection: NonEmptyList[PreparedDataField[F, A, ?]]
  ) extends PreparedField[F, A]

  final case class FragmentDefinition[F[_], A](
      name: String,
      typeCondition: String,
      specify: Any => Option[A],
      fields: NonEmptyList[PreparedField[F, A]]
  )

  final case class EdgeId(id: Int) extends AnyVal

  sealed trait PreparedResolver[F[_]]
  object PreparedResolver {
    final case class Fallible[F[_]](r: FallibleResolver[F, Any, Any]) extends PreparedResolver[F]
    final case class Effect[F[_]](r: EffectResolver[F, Any, Any]) extends PreparedResolver[F]
    final case class Pure[F[_]](r: PureResolver[F, Any, Any]) extends PreparedResolver[F]
    final case class Stream[F[_]](r: StreamResolver[F, Any, Any]) extends PreparedResolver[F]
    final case class Batch[F[_]](r: BatchResolver[F, Any, Any]) extends PreparedResolver[F]
  }

  sealed trait PreparedEdge[F[_]]

  object PreparedEdge {
    final case class Edge[F[_]](
        id: EdgeId,
        resolver: PreparedResolver[F],
        statisticsName: String
    ) extends PreparedEdge[F]
    final case class Skip[F[_]](
        specify: Any => F[Either[Any, Any]],
        relativeJump: Int
    ) extends PreparedEdge[F]
  }

  final case class PreparedCont[F[_]](
      edges: NonEmptyChain[PreparedEdge[F]],
      cont: Prepared[F, Any]
  )

  final case class Selection[F[_], A](fields: NonEmptyList[PreparedField[F, A]]) extends Prepared[F, A]

  final case class PreparedList[F[_], A](of: PreparedCont[F], toSeq: Any => Seq[A]) extends Prepared[F, A]

  final case class PreparedOption[F[_], A](of: PreparedCont[F]) extends Prepared[F, A]

  final case class PreparedLeaf[F[_], A](name: String, encode: A => Json) extends Prepared[F, A]

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
    def pop: PrepCursor = PrepCursor(Chain.fromOption(position.initLast).flatMap { case (xs, _) => xs })
  }

  object PrepCursor {
    val empty: PrepCursor = PrepCursor(Chain.empty)
  }

  final case class Prep(
      cycleSet: Set[String],
      nextId: Int,
      cursor: PrepCursor
  )

  object Prep {
    val empty: Prep = Prep(Set.empty, 1, PrepCursor.empty)
  }

  object InArr {
    def unapply[A](p: In[A]): Option[(In[A], Seq[?] => Either[String, A])] =
      p.asInstanceOf[In[A]] match {
        case x: InArr[?, A] => Some((x.of.asInstanceOf[In[A]], x.fromSeq.asInstanceOf[Seq[?] => Either[String, A]]))
        case _              => None
      }
  }

  object InOpt {
    def unapply[A](p: In[A]): Option[In[A]] =
      p.asInstanceOf[In[Option[A]]] match {
        case x: InOpt[A] => Some(x.of.asInstanceOf[In[A]])
        case _           => None
      }
  }

  object OutArr {
    def unapply[G[_], A](p: Out[G, A]): Option[(Out[G, A], Any => Seq[A], Resolver[G, Any, Any])] =
      p.asInstanceOf[Out[G, A]] match {
        case x: OutArr[G, ?, A, ?] =>
          Some((x.of.asInstanceOf[Out[G, A]], x.toSeq.asInstanceOf[Any => Seq[A]], x.resolver.asInstanceOf[Resolver[G, Any, Any]]))
        case _ => None
      }
  }

  object OutOpt {
    def unapply[G[_], A](p: Out[G, A]): Option[(Out[G, A], Resolver[G, Any, Any])] =
      p.asInstanceOf[Out[G, Option[A]]] match {
        case x: OutOpt[G, A, ?] => Some((x.of.asInstanceOf[Out[G, A]], x.resolver.asInstanceOf[Resolver[G, Any, Any]]))
        case _                  => None
      }
  }

  def flattenResolvers[F[_]: Monad, G[_]](parentName: String, resolver: Resolver[G, Any, Any], index: Int = 0)(implicit
      S: Stateful[F, Prep]
  ): F[(NonEmptyChain[PreparedEdge[G]], String, Int)] = {
    def cast(r: Resolver[G, ?, ?]): Resolver[G, Any, Any] = r.asInstanceOf[Resolver[G, Any, Any]]
    import PreparedResolver._
    import PreparedEdge._
    resolver match {
      case r @ BatchResolver(id, _) =>
        nextId[F].map(nid => (NonEmptyChain.of(Edge(EdgeId(nid), Batch(r), s"batch_${id.id}")), parentName, index + 1))
      case r @ EffectResolver(_) =>
        val thisName = s"${parentName}_effect"
        nextId[F].map(nid => (NonEmptyChain.of(Edge(EdgeId(nid), Effect(r), thisName)), thisName, index + 1))
      case r @ PureResolver(_) =>
        val thisName = s"${parentName}_pure"
        nextId[F].map(nid => (NonEmptyChain.of(Edge(EdgeId(nid), Pure(r), thisName)), thisName, index + 1))
      case r @ FallibleResolver(_) =>
        val thisName = s"${parentName}_fallible"
        nextId[F].map(nid => (NonEmptyChain.of(Edge(EdgeId(nid), Fallible(r), thisName)), thisName, index + 1))
      case r @ StreamResolver(_) =>
        val thisName = s"${parentName}_stream"
        nextId[F].map(nid => (NonEmptyChain.of(Edge(EdgeId(nid), Stream(r), thisName)), thisName, index + 1))
      case CacheResolver(skip, fallback) =>
        flattenResolvers[F, G](parentName, cast(fallback), index).map { case (children, newParent, newIndex) =>
          (
            NonEmptyChain.of(
              Skip(skip.asInstanceOf[Any => G[Either[Any, Any]]], newIndex + 1 - index)
            ) ++ children,
            newParent,
            newIndex
          )
        }
      case CompositionResolver(left, right) =>
        flattenResolvers[F, G](parentName, cast(left), index)
          .flatMap { case (ys, newParentName, lidx) =>
            flattenResolvers[F, G](newParentName, cast(right), lidx).map { case (zs, outName, ridx) => (ys ++ zs, outName, ridx) }
          }
    }
  }

  def underlyingOutputTypename[G[_]](ot: Out[G, ?]): String = (ot: @unchecked) match {
    case Enum(name, _, _)         => name
    case Union(name, _, _)        => name
    case Interface(name, _, _, _) => name
    case Type(name, _, _, _)      => name
    case Scalar(name, _, _, _)    => name
    case OutOpt(of, _)            => underlyingOutputTypename(of)
    case OutArr(of, _, _)         => underlyingOutputTypename(of)
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

  def nextId[F[_]: Monad](implicit S: Stateful[F, Prep]) =
    S.inspect(_.nextId) <* S.modify(x => x.copy(nextId = x.nextId + 1))

  def raise[F[_], A](s: String, caret: Option[Caret])(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] =
    S.get.map(state => NonEmptyChain.one(PositionalError(state.cursor, caret.toList, s))).flatMap(F.raiseError[A])

  def raiseOpt[F[_], A](o: Option[A], s: String, caret: Option[Caret])(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] =
    o.map(_.pure[F]).getOrElse(raise[F, A](s, caret))

  def raiseEither[F[_], A](e: Either[String, A], caret: Option[Caret])(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[A] =
    e match {
      case Left(value)  => raise[F, A](value, caret)
      case Right(value) => F.pure(value)
    }

  def ambientAt[F[_]: Monad, A](cursor: PrepCursor)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    S.inspect(_.cursor).flatMap { c =>
      S.modify(_.copy(cursor = cursor)) *> fa <* S.modify(_.copy(cursor = c))
    }

  def ambientEdge[F[_]: Monad, A](edge: Validation.Edge)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    S.inspect(_.cursor.add(edge)).flatMap(ambientAt[F, A](_)(fa))

  def ambientField[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.Field(name))(fa)

  def ambientOutputType[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.OutputType(name))(fa)

  def ambientArg[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.Arg(name))(fa)

  def ambientIndex[F[_]: Monad, A](i: Int)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.Index(i))(fa)

  def ambientInputType[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](Validation.Edge.InputType(name))(fa)

  def modifyError[F[_], A](f: PositionalError => PositionalError)(fa: F[A])(implicit F: MonadError[F, NonEmptyChain[PositionalError]]) =
    F.adaptError(fa)(_.map(f))

  def appendMessage[F[_], A](message: String)(fa: F[A])(implicit F: MonadError[F, NonEmptyChain[PositionalError]]) =
    modifyError[F, A](d => d.copy(message = d.message + "\n" + message))(fa)

  def typenameField[G[_]](typename: String) = {
    import gql.dsl._
    pure[G, Any](_ => typename)
  }

  def inFragment[F[_], A](
      fragmentName: String,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      caret: Option[Caret]
  )(
      faf: Pos[P.FragmentDefinition] => F[A]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[A] =
    D.defer {
      S.get.flatMap[A] {
        case c if c.cycleSet(fragmentName) =>
          raise(s"Fragment by '$fragmentName' is cyclic. Hint: graphql queries must be finite.", caret)
        case _ =>
          fragments.get(fragmentName) match {
            case None => raise(s"Unknown fragment name '$fragmentName'.", caret)
            case Some(f) =>
              val beforeF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet + fragmentName))
              val afterF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet - fragmentName))

              beforeF *> faf(f) <* afterF
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
        implicit S: Stateful[F, Prep]
    ): F[FieldInfo[G]] =
      S.inspect(_.cursor).map(FieldInfo(name, alias, args, tpe, caret, _))
  }

  def collectFieldInfo[F[_]: Parallel, G[_]](
      af: AbstractField[G, ?, ?],
      f: P.Field,
      caret: Caret,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[FieldInfo[G]] = ambientField(f.name) {
    // Verify arguments by decoding them
    val decF = decodeFieldArgs[F, G, Any](af.arg.asInstanceOf[Arg[Any]], f.arguments, variableMap).void

    // Verify subselection
    val c = f.selectionSet.caret
    def verifySubsel(t: Out[G, ?]): F[SimplifiedType[G]] =
      (t, f.selectionSet.value) match {
        case (OutArr(inner, _, _), _) => verifySubsel(inner).map(SimplifiedType.List(_))
        case (OutOpt(inner, _), _)    => verifySubsel(inner).map(SimplifiedType.Option(_))
        case (ol: Selectable[G, ?], Some(ss)) =>
          collectSelectionInfo[F, G](ol, ss, variableMap, fragments, discoveryState)
            .map(xs => SimplifiedType.Selectable(ol.name, xs))
        case (e: Enum[G, ?], None)   => F.pure(SimplifiedType.Enum(e.name))
        case (s: Scalar[G, ?], None) => F.pure(SimplifiedType.Scalar(s.name))
        case (o, Some(_))            => raise(s"Type `${friendlyName(o)}` cannot have selections.", Some(c))
        case (o, None)               => raise(s"Object like type `${friendlyName(o)}` must have a selection.", Some(c))
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
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[SelectionInfo[G]]] = D.defer {
    val fields = ss.selections.collect { case Pos(caret, P.Selection.FieldSelection(field)) => (caret, field) }

    val actualFields: Map[String, AbstractField[G, ?, ?]] =
      s.abstractFieldMap + ("__typename" -> AbstractField(Applicative[Arg].unit, Eval.now(stringScalar[G]), None))

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
      S: Stateful[F, Prep]
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
      S: Stateful[F, Prep]
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

  // https://spec.graphql.org/draft/#sec-Field-Selection-Merging.Formal-Specification
  def checkFieldsMerge[F[_]: Parallel, G[_]](a: FieldInfo[G], asi: SelectionInfo[G], b: FieldInfo[G], bsi: SelectionInfo[G])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
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

  def checkSelectionsMerge[F[_]: Parallel, G[_]](xs: NonEmptyList[SelectionInfo[G]])(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
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

      mergeFieldsF >> zs.toList.flatMap { case (_, fi) => fi.tpe.selections }.toNel.traverse_(checkSelectionsMerge[F, G])
    }
  }

  // Optimization: we don't check selections recursively since checkSelectionsMerge traverses the whole tree
  // We only need to check the immidiate children and will eventually have checked the whole tree
  def checkSimplifiedTypeShape[F[_]: Parallel, G[_]](a: SimplifiedType[G], b: SimplifiedType[G], caret: Caret)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
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

  def findImplementations[G[_]](
      s: Selectable[G, ?],
      discoveryState: SchemaShape.DiscoveryState[G]
  ): List[(Type[G, ?], Option[? => Option[?]])] = s match {
    case t @ Type(_, _, _, _) => List((t, None))
    case u @ Union(_, _, _)   => u.types.toList.map(x => (x.tpe.value, Some(x.specify)))
    case it @ Interface(_, _, _, _) =>
      type Specify = ? => Option[?]

      val impls: Map[String, (ObjectLike[G, ?], Specify)] =
        discoveryState.implementations
          .get(it.name)
          .getOrElse(Map.empty)
          .collect { case (k, Right(v)) => k -> v }

      impls.toList.collect { case (_, (t @ Type(_, _, _, _), specify)) => (t, Some(specify)) }
  }

  final case class PairedFieldSelection[G[_]](
      info: FieldInfo[G],
      field: Field[G, ?, ?, ?]
  )
  final case class MergedImplementation[G[_]](
      leaf: Type[G, ?],
      selections: NonEmptyList[PairedFieldSelection[G]],
      specify: Option[? => Option[?]]
  )
  def mergeImplementations[F[_]: Parallel, G[_]](
      base: Selectable[G, ?],
      sels: NonEmptyList[SelectionInfo[G]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
  ): F[NonEmptyList[MergedImplementation[G]]] = {
    val concreteBaseMap = findImplementations(base, discoveryState).map { case x @ (t, _) => t.name -> x }.toMap
    val concreteBase = concreteBaseMap.toList

    val nestedSelections = sels.toList.flatMap { sel =>
      val concreteIntersections = findImplementations(sel.s, discoveryState)
        .map { case (t, _) => t.name }

      concreteIntersections tupleRight sel.fields
    }

    val grouped = nestedSelections
      .groupMap { case (k, _) => k } { case (_, vs) => vs }
      .collect { case (k, x :: xs) => k -> NonEmptyList(x, xs).flatten.map(x => x.outputName -> x).toNem.toNonEmptyList }

    val collected = concreteBase.parFlatTraverse { case (k, (t, specify)) =>
      grouped.get(k).toList.traverse { fields =>
        fields
          .parTraverse { f =>
            if (f.name === "__typename") F.pure(PairedFieldSelection(f, typenameField[G](t.name)))
            else {
              t.fieldMap.get(f.name) match {
                case None        => raise[F, PairedFieldSelection[G]](s"Could not find field '${f.name}' on type `${t.name}`.", None)
                case Some(field) => F.pure(PairedFieldSelection(f, field))
              }
            }
          }
          .map(fields => MergedImplementation(t, fields, specify))
      }
    }

    collected.flatMap { xs =>
      xs.toNel match {
        case Some(x) => F.pure(x)
        case None =>
          raise[F, NonEmptyList[MergedImplementation[G]]](
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
      S: Stateful[F, Prep],
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

  def closeFieldParameters[F[_]: Parallel, G[_]](
      fi: FieldInfo[G],
      field: Field[G, Any, Any, Any],
      variableMap: VariableMap
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[Resolver[G, Any, Any]] = {
    val decObj = decodeFieldArgs[F, G, Any](field.args, fi.args, variableMap)
    decObj.map(a => field.resolve.contramap[Any]((_, a)))
  }

  def prepareField[F[_]: Parallel, G[_]: Applicative](
      fi: FieldInfo[G],
      field: Field[G, Any, Any, Any],
      variableMap: VariableMap,
      currentTypename: String,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[PreparedDataField[G, Any, ?]] = {
    closeFieldParameters[F, G](fi, field, variableMap).flatMap { resolve =>
      val tpe = field.output.value
      val selCaret = fi.caret
      val name = fi.name

      nextId[F].flatMap { id =>
        flattenResolvers[F, G](s"${currentTypename}_$name", resolve).flatMap { case (edges, parentName, _) =>
          def typePrep(t: Out[G, Any], parentName: String): F[Prepared[G, Any]] =
            (t, fi.tpe.selections.toNel) match {
              case (OutArr(inner, toSeq, resolver), _) =>
                flattenResolvers[F, G](parentName, resolver).flatMap { case (edges, parentName, _) =>
                  typePrep(inner, parentName).map(cont => PreparedList(PreparedCont(edges, cont), toSeq))
                }
              case (OutOpt(inner, resolver), _) =>
                flattenResolvers[F, G](parentName, resolver).flatMap { case (edges, parentName, _) =>
                  typePrep(inner, parentName).map(cont => PreparedOption(PreparedCont(edges, cont)))
                }
              case (ol: Selectable[G, ?], Some(ss)) =>
                prepareSelectable[F, G](ol, ss, variableMap, discoveryState)
                  .map(Selection(_))
              case (e: Enum[G, Any], None) =>
                F.pure(PreparedLeaf(e.name, x => Json.fromString(e.revm(x))))
              case (s: Scalar[G, Any], None) =>
                F.pure(PreparedLeaf(s.name, x => s.encoder(x).asJson))
              case (o, Some(_)) => raise(s"Type `${friendlyName[G, Any](o)}` cannot have selections.", Some(selCaret))
              case (o, None)    => raise(s"Object like type `${friendlyName[G, Any](o)}` must have a selection.", Some(selCaret))
            }

          val prepF: F[Prepared[G, Any]] = typePrep(tpe, parentName)

          prepF.map { p =>
            val pc = PreparedCont(edges, p)
            PreparedDataField(id, name, fi.alias, pc)
          }
        }
      }
    }
  }

  def prepareSelectable[F[_]: Parallel, G[_]](
      s: Selectable[G, Any],
      sis: NonEmptyList[SelectionInfo[G]],
      variableMap: VariableMap,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[PreparedField[G, Any]]] = {
    mergeImplementations[F, G](s, sis, discoveryState).flatMap { impls =>
      impls.parFlatTraverse { impl =>
        val fa = impl.selections.parTraverse { sel =>
          val field = (sel.field: Field[G, ?, ?, ?]).asInstanceOf[Field[G, Any, Any, Any]]
          prepareField[F, G](sel.info, field, variableMap, impl.leaf.name, discoveryState)
        }

        fa.flatMap { xs =>
          impl.specify match {
            case None => F.pure(xs)
            case Some(spec) =>
              nextId[F].map { id =>
                NonEmptyList.one(PreparedSpecification(id, s.name, spec.asInstanceOf[Function1[Any, Option[Any]]], xs))
              }
          }
        }
      }
    }
  }

  def prepareSelectableRoot[F[_]: Parallel, G[_]](
      s: Selectable[G, ?],
      ss: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ) = {
    collectSelectionInfo[F, G](s, ss, variableMap, fragments, discoveryState).flatMap { root =>
      checkSelectionsMerge[F, G](root) >> prepareSelectable[F, G](s.asInstanceOf[Selectable[G, Any]], root, variableMap, discoveryState)
    }
  }

  type VariableMap = Map[String, Either[P.Value, Json]]

  def matchType[F[_], G[_]](
      name: String,
      sel: Selectable[G, ?],
      caret: Caret,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit F: MonadError[F, NonEmptyChain[PositionalError]], S: Stateful[F, Prep]): F[Selectable[G, ?]] =
    if (sel.name == name) F.pure(sel)
    else {
      sel match {
        case t @ Type(n, _, _, _) =>
          // Check downcast
          t.implementsMap.get(name) match {
            case None => raise(s"Tried to match with type `$name` on type object type `$n`.", Some(caret))
            case Some(impl) =>
              val i: Interface[G, _] = impl.value
              F.pure(i.asInstanceOf[Interface[G, Any]])
          }
        // What types implement this interface?
        // We can both downcast and up-match
        case i @ Interface(n, _, _, _) =>
          i.implementsMap.get(name) match {
            case Some(impl) =>
              val i: Interface[G, _] = impl.value
              F.pure(i.asInstanceOf[Interface[G, Any]])
            case None =>
              raiseOpt(
                discoveryState.implementations.get(i.name),
                s"The interface `${i.name}` is not implemented by any type.",
                caret.some
              ).flatMap { m =>
                raiseOpt(
                  m.get(name).map {
                    case Right((x, _)) => x
                    case Left(i)       => i
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
            case Some(i) => F.pure(i.tpe.value.asInstanceOf[Type[G, Any]])
            case None =>
              u.types.toList
                .collectFirstSome { case v =>
                  val t: Type[G, _] = v.tpe.value
                  t.implementsMap.get(name)
                } match {
                case None =>
                  raise(
                    s"`$name` is not a member of the union `$n` (or any of the union's types' implemented interfaces), possible members are ${u.instanceMap.keySet
                      .mkString(", ")}.",
                    caret.some
                  )
                case Some(x) => F.pure(x.value.asInstanceOf[Interface[G, Any]])
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
      S: Stateful[F, Prep]
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
      S: Stateful[F, Prep]
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
      case (InArr(of, dec), P.Value.ListValue(xs)) =>
        xs.zipWithIndex
          .parTraverse { case (x, i) =>
            ambientIndex(i) {
              parseInput[F, A](x, of, variableMap, ambigiousEnum)
            }
          }
          .flatMap(dec(_).fold(raise(_, None), F.pure(_)))
      case (InOpt(_), P.Value.NullValue) => F.pure(None.asInstanceOf[A])
      case (InOpt(of), x)                => parseInput[F, A](x, of, variableMap, ambigiousEnum).map(Some(_).asInstanceOf[A])
      case (i, _)                        => raise(s"Expected type `${inName(i)}`, but got value ${pValueName(v)}.", None)
    }

  def parseArgValue[F[_]: Parallel, A](
      a: ArgValue[A],
      input: Map[String, P.Value],
      variableMap: Option[VariableMap],
      ambigiousEnum: Boolean
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
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
      S: Stateful[F, Prep]
  ): F[A] = {
    // All provided fields are of the correct type
    // All required fields are either defiend or defaulted
    val fieldsF: F[Chain[(String, ArgParam[Any])]] =
      arg.entries.parTraverse { a =>
        parseArgValue[F, Any](
          a.asInstanceOf[ArgValue[Any]],
          input,
          variableMap,
          ambigiousEnum
        )
          .tupleLeft(a.name)
      }

    fieldsF
      .map(_.toList.toMap)
      .flatMap(arg.decode(_).fold(raise(_, None), F.pure(_)))
  }

  def parserValueToValue[F[_]: Parallel](v: P.Value)(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
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

  // TODO add another phase after finding the OperationDefinition and before this,
  // that checks all that variables have been used
  def prepareParts[F[_]: Parallel, G[_]: Applicative](
      op: P.OperationDefinition,
      frags: List[Pos[P.FragmentDefinition]],
      schema: Schema[G, ?, ?, ?],
      variableMap: Map[String, Json]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[(P.OperationType, NonEmptyList[PreparedField[G, Any]])] = {
    val ot = operationType(op)

    val rootSchema: F[Type[G, Any]] =
      ot match {
        // We sneak the introspection query in here
        case P.OperationType.Query =>
          val i: NonEmptyList[(String, Field[G, Unit, ?, ?])] = schema.shape.introspection
          val q = schema.shape.query.asInstanceOf[Type[G, Any]]
          F.pure(q.copy(fields = q.fields concatNel i.map { case (k, v) => k -> v.contramap[Any](_ => ()) }))
        case P.OperationType.Mutation =>
          raiseOpt(schema.shape.mutation.map(_.asInstanceOf[Type[G, Any]]), "No `Mutation` type defined in this schema.", None)
        case P.OperationType.Subscription =>
          raiseOpt(schema.shape.subscription.map(_.asInstanceOf[Type[G, Any]]), "No `Subscription` type defined in this schema.", None)
      }

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

    val fa =
      preCheckVariablesF.flatMap { vm =>
        rootSchema.flatMap { root =>
          prepareSelectableRoot[F, G](
            root.asInstanceOf[Type[G, Any]],
            selection,
            vm,
            frags.map(f => f.value.name -> f).toMap,
            schema.shape.discover
          )
        }
      }

    fa tupleLeft ot
  }

  type H[A] = StateT[EitherT[Eval, NonEmptyChain[PositionalError], *], Prep, A]

  // Invariant, the computation is pure so we can run state's as many times as we want, arbitarily
  def parallelForPureState[G[_], E: Semigroup, S](implicit G: MonadError[G, E]) = new Parallel[StateT[G, S, *]] {
    type F[A] = StateT[G, S, A]

    override def sequential: F ~> F = FunctionK.id[F]

    override def parallel: F ~> F = FunctionK.id[F]

    override def applicative: Applicative[F] = new Applicative[F] {
      override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
        StateT { s =>
          ff.run(s).attempt.flatMap {
            case Right((s2, f)) => fa.run(s2).map { case (s3, a) => (s3, f(a)) }
            // Okay, there's an error
            // Let's just run the other side and see if there are more errors to report
            case Left(e) =>
              fa.run(s).attempt.flatMap {
                case Left(e2) => G.raiseError(e |+| e2)
                case Right(_) => G.raiseError(e)
              }
          }
        }

      override def pure[A](x: A): F[A] = StateT.pure(x)
    }

    override def monad: Monad[StateT[G, S, *]] = implicitly
  }

  implicit val par: Parallel[H] =
    parallelForPureState[EitherT[Eval, NonEmptyChain[PositionalError], *], NonEmptyChain[PositionalError], Prep]

  def prepare[F[_]: Applicative](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, ?, ?, ?],
      variableMap: Map[String, Json],
      operationName: Option[String]
  ): EitherNec[PositionalError, (P.OperationType, NonEmptyList[PreparedField[F, Any]])] = {
    val (ops, frags) = executabels.toList.partitionEither {
      case P.ExecutableDefinition.Operation(op)  => Left(op)
      case P.ExecutableDefinition.Fragment(frag) => Right(frag)
    }

    getOperationDefinition[Either[(String, List[Caret]), *]](ops, operationName) match {
      case Left((e, carets)) => Left(NonEmptyChain.one(PositionalError(PrepCursor.empty, carets, e)))
      case Right(op) =>
        prepareParts[H, F](op, frags, schema, variableMap)
          .runA(Prep.empty)
          .value
          .value
    }
  }
}
