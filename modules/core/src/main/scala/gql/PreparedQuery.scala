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

object PreparedQuery {
  sealed trait Prepared[F[_], A]

  sealed trait PreparedField[F[_], A]

  final case class PreparedDataField[F[_], I, T](
      id: Int,
      name: String,
      alias: Option[String],
      cont: PreparedCont[F]
  ) extends PreparedField[F, I]

  final case class PreparedFragField[F[_], A](
      id: Int,
      typename: String,
      specify: Any => Option[A],
      selection: Selection[F, A]
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

  sealed trait PrepEdge {
    def name: String
  }
  object PrepEdge {
    final case class ASTEdge(edge: SchemaShape.ValidationEdge) extends PrepEdge {
      def name: String = edge.name
    }
    final case class FragmentEdge(name: String) extends PrepEdge
  }

  final case class PrepCursor(position: Chain[PrepEdge]) {
    def add(edge: PrepEdge): PrepCursor = PrepCursor(position append edge)
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

  def ambientEdge[F[_]: Monad, A](edge: PrepEdge)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    S.inspect(_.cursor).flatMap { c =>
      S.modify(_.copy(cursor = c.add(edge))) *> fa <* S.modify(_.copy(cursor = c))
    }

  def ambientField[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.Field(name)))(fa)

  def ambientOutputType[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.OutputType(name)))(fa)

  def ambientArg[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.Arg(name)))(fa)

  def ambientIndex[F[_]: Monad, A](i: Int)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.Index(i)))(fa)

  def ambientInputType[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.InputType(name)))(fa)

  def ambientFragment[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.FragmentEdge(name))(fa)

  def findInterfaceImplementations[F[_], G[_]](it: Interface[G, ?], discoveryState: SchemaShape.DiscoveryState[G]) = {
    val impls: Map[String, (ObjectLike[G, Any], Any => Option[Any])] =
      discoveryState.implementations.get(it.name).getOrElse(Map.empty)
    val types: List[(Type[G, Any], Any => Option[Any])] =
      impls.toList.collect { case (_, (t @ Type(_, _, _, _), specify)) => (t, specify) }
    types
  }

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
  ) =
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

              beforeF *> ambientFragment(fragmentName)(faf(f)) <* afterF
          }
      }
    }

  def prepareType[F[_]: Parallel, G[_]](
      t: Type[G, Any],
      s: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
    val schemaMap = t.concreteFieldsMap
    s.selections.parFlatTraverse[F, PreparedField[G, Any]] {
      case Pos(caret, P.Selection.FieldSelection(field)) =>
        val f = if (field.name == "__typename") Some(typenameField[G](t.name)) else schemaMap.get(field.name)
        (f: @unchecked) match {
          case None => raise(s"Unknown field name '${field.name}'.", Some(caret))
          case Some(f: Field[G, Any, Any, Any] @unchecked) =>
            ambientField(field.name) {
              prepareField[F, G](field, caret, f, variableMap, fragments, currentTypename, discoveryState)
            }.map(NonEmptyList.one)
        }
      // Types just inline every field
      case Pos(caret, P.Selection.InlineFragmentSelection(f)) =>
        matchType2[F, G](f.typeCondition, t, caret, discoveryState) >>
          prepareType[F, G](t, f.selectionSet, variableMap, fragments, currentTypename, discoveryState)
      case Pos(caret, P.Selection.FragmentSpreadSelection(f)) =>
        inFragment(f.fragmentName, fragments, caret.some) { case Pos(_, f) =>
          matchType2[F, G](f.typeCnd, t, caret, discoveryState) >>
            prepareType[F, G](t, f.selectionSet, variableMap, fragments, currentTypename, discoveryState)
        }
    }
  }

  def prepareArgument[F[_]: Parallel, G[_], A](
      f: P.Field,
      caret: Caret,
      arg: Arg[A],
      variableMap: VariableMap
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
  ): F[A] = {
    val provided = f.arguments.toList.flatMap(_.nel.toList)

    // Treat input arguments as an object
    // Decode the args as-if an input
    val argObj =
      P.Value.ObjectValue(provided.map(a => a.name -> a.value))

    arg match {
      case PureArg(value) if provided.isEmpty => value.fold(raise(_, None), F.pure(_))
      case PureArg(_)                         => raise(s"Field '${f.name}' does not accept arguments.", Some(caret))
      case nea @ NonEmptyArg(_, _)            => parseInputObj[F, A](argObj, nea, Some(variableMap), ambigiousEnum = false)
    }
  }

  /*.flatMap { nel =>

              ???
              // matched match {
              //   // If the type is a supertype we check if all types adhere to this type as a lower bound
              //   // If the supertype matches another disjoint sub-type tree, no type in that sub-tree can occur if this type cannot occur as a lower bound of a resulting type
              //   case MatchResult.SuperType(_) =>
              // }
            }*/

  final case class FieldInfo[G[_]](
      tpe: Type[G, ?],
      cursor: PrepCursor,
      selection: NonEmptyList[(Caret, P.Field)]
  )
  def collectFieldInfo[F[_]: Parallel, G[_]](
      s: Selectable2[G, Any],
      ss: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[List[FieldInfo[G]]] = D.defer {
    val fields = ss.selections.collect { case Pos(caret, P.Selection.FieldSelection(field)) => (caret, field) }

    val actualFields: Map[String, AbstractField[G, ?]] =
      s.abstractFieldMap + ("__typename" -> AbstractField(Eval.now(stringScalar[G]), None))

    // This is purely for debuggability since the implementing type will also check field existence
    val validateFieldsF: F[Unit] = fields.parTraverse_ { case (caret, field) =>
      actualFields.get(field.name) match {
        case None    => raise[F, Unit](s"Field '${field.name}' is not a member of `${s.name}`.", Some(caret))
        case Some(_) => F.unit
      }
    }

    // Contains all types that can implement the selected fields
    // For every type make an inline fragment where fields is selected
    val fieldImplProduct: F[List[FieldInfo[G]]] = S.inspect(_.cursor).map { c =>
      fields.toNel
        .flatMap { nel =>
          val ts = s match {
            case t @ Type(_, _, _, _) => List(t)
            case u @ Union(_, _, _)   => u.types.toList.map(_.tpe.value)
            case i @ Interface(_, _, _, _) =>
              findInterfaceImplementations[F, G](i, discoveryState).map { case (t, _) => t }
          }
          ts.map(t => FieldInfo(t, c, nel)).toNel
        }
        .toList
        .flatMap(_.toList)
    }

    val realInlines =
      ss.selections
        .collect { case Pos(caret, P.Selection.InlineFragmentSelection(f)) => (caret, f) }
        .flatTraverse { case (caret, f) =>
          ambientFragment(s"inline_on_${f.typeCondition}") {
            matchType2[F, G](f.typeCondition, s, caret, discoveryState).flatMap { matched =>
              val t = matched.tpe
              collectFieldInfo[F, G](t, f.selectionSet, variableMap, fragments, discoveryState).map(_.toList)
            }
          }
        }

    val realFragments =
      ss.selections
        .collect { case Pos(caret, P.Selection.FragmentSpreadSelection(f)) => (caret, f) }
        .flatTraverse { case (caret, f) =>
          inFragment(f.fragmentName, fragments, caret.some) { case Pos(caret, f) =>
            matchType2[F, G](f.typeCnd, s, caret, discoveryState).flatMap { matched =>
              val t = matched.tpe
              collectFieldInfo[F, G](t, f.selectionSet, variableMap, fragments, discoveryState).map(_.toList)
            }
          }
        }

    ((validateFieldsF >> fieldImplProduct) :: realInlines :: realFragments :: Nil).parFlatSequence
  }

  def prepareSelectable2[F[_]: Parallel, G[_]](
      i: Selectable2[G, Any],
      s: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
    ???
  }

  /*
interface A {
  lol: String
}

interface B implements A {
  lol: String
}

interface C implements A {
  lol: String
}

type D implements B & A {
  lol: String
}

type E implements C & A {
  lol: String
}

type Query {
  doStuff: B
}

...

query {
  doStuff {
    ... on A {
      ... on C {
        ... on E {
          lol
        }
        ... on A {
          ... on C {
            lol
          }
        }
      }
    }
  }
}

->

query {
  doStuff {
    ... on E {
      lol
    }
  }
}

...

A => Option[B]
A => Option[C]
A => Option[D]
A => Option[E]

B => Option[D]
C => Option[E]

do interfaces have specify functions?
yes:
  we can compose specify functions
no:
  toplevel functions on types must provide specify function for all interfaces
   */

  // def prepareSelections2[F[_]: Parallel, G[_]](
  //     ol: Selectable2[G, Any],
  //     s: P.SelectionSet,
  //     variableMap: VariableMap,
  //     fragments: Map[String, Pos[P.FragmentDefinition]],
  //     currentTypename: String,
  //     discoveryState: SchemaShape.DiscoveryState[G]
  // )(implicit
  //     G: Applicative[G],
  //     S: Stateful[F, Prep],
  //     F: MonadError[F, NonEmptyChain[PositionalError]],
  //     D: Defer[F]
  // ): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
  //   // this element either is a concrete type or an interface
  //   // If this is an interface we do a fragment spread for every implementing type

  //   val fields = s.selections
  //     .collect { case Pos(caret, P.Selection.FieldSelection(field)) => (caret, field) }

  //   val inlineSpecsF: F[List[(Selectable2[G, Any], Any => Option[Any])]] =
  //     s.selections
  //       .collect { case Pos(caret, P.Selection.InlineFragmentSelection(f)) => (caret, f) }
  //       .parTraverse[F, (Selectable2[G, Any], Any => Option[Any])] { case (caret, f) =>
  //         f.typeCondition match {
  //           case None          => raise(s"Inline fragment must have a type condition.", Some(caret))
  //           case Some(typeCnd) => matchType2[F, G](typeCnd, ol, caret, discoveryState) tupleRight f.selectionSet
  //         }
  //       }

  //   val syntheticSpecs: List[(Selectable2[G, Any], Any => Option[Any])] = ol match {
  //     case c: Concrete[G, Any]  => Nil
  //     case i: Interface[G, Any] => findInterfaceImplementations[F, G](i, discoveryState)
  //   }

  //   val allInlineFragsF = inlineSpecsF
  //     .map(_ ++ syntheticSpecs)
  //   // .map(_.map { case ((ol, specialize)) =>
  //   //   prepareSelections2[F, G](ol, f.selectionSet, variableMap, fragments, typeCnd, discoveryState)
  //   //     .map(Selection(_))
  //   //     .flatMap[PreparedField[G, Any]](s => nextId[F].map(id => PreparedFragField(id, typeCnd, specialize, s)))
  //   // })

  //   val inlineFrags =
  //     s.selections
  //       .collect { case Pos(caret, P.Selection.InlineFragmentSelection(f)) => (caret, f) }
  //       .parTraverse[F, PreparedField[G, Any]] { case (caret, f) =>
  //         f.typeCondition match {
  //           case None => raise(s"Inline fragment must have a type condition.", Some(caret))
  //           case Some(typeCnd) =>
  //             matchType2[F, G](typeCnd, ol, caret, discoveryState).flatMap { case (ol, specialize) =>
  //               prepareSelections2[F, G](ol, f.selectionSet, variableMap, fragments, typeCnd, discoveryState)
  //                 .map(Selection(_))
  //                 .flatMap[PreparedField[G, Any]](s => nextId[F].map(id => PreparedFragField(id, typeCnd, specialize, s)))
  //             }
  //         }
  //       }

  //   val fragmentSpreadsF = s.selections
  //     .collect { case Pos(caret, P.Selection.FragmentSpreadSelection(f)) => (caret, f) }
  //     .parTraverse[F, PreparedField[G, Any]] { case (caret, f) =>
  //       fragments.get(f.fragmentName) match {
  //         case None => raise(s"Unknown fragment name '${f.fragmentName}'.", Some(caret))
  //         case Some(fd) =>
  //           ambientFragment(f.fragmentName) {
  //             prepareFragment2[F, G](ol, fd, variableMap, fragments, fd.value.typeCnd, discoveryState)
  //               .flatMap[PreparedField[G, Any]] { fd =>
  //                 nextId[F].map(id => PreparedFragField(id, fd.typeCondition, fd.specify, Selection(fd.fields)))
  //               }
  //           }
  //       }
  //     }

  //   // val concretes = ol match {
  //   //   case c: Concrete[G, Any] =>
  //   //   case i: Interface[G, Any] =>
  //   // }

  //   ???
  // }

  def prepareSelections[F[_]: Parallel, G[_]](
      ol: Selectable[G, Any],
      s: P.SelectionSet,
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
    val syntheticTypename =
      Field[G, Any, String, Unit](
        Applicative[Arg].unit,
        FallibleResolver[G, (Any, Unit), String] { case (input, _) =>
          // TODO this code shares much with the subtype interfaces below in matchType
          val typename = ol match {
            case Type(name, _, _, _) => Some(name)
            case Union(_, types, _) =>
              types.collectFirstSome { variant => variant.specify(input) as variant.tpe.value.name }
            case Interface(name, _, _, _) =>
              // Only look for concrete types; that is; `Type`s
              discoveryState.implementations
                .get(name)
                .toList
                .flatMap(_.values.toList)
                .collectFirstSome {
                  case (Type(name, _, _, _), spec) => spec(input) as name
                  case _                           => None
                }
          }

          G.pure(typename.toRightIor("Typename could not be determined, this is an implementation error."))
        },
        Eval.now(gql.ast.stringScalar)
      )

    val schemaMap = ol.fieldMap + ("__typename" -> syntheticTypename)
    s.selections.parTraverse[F, PreparedField[G, Any]] {
      case Pos(caret, P.Selection.FieldSelection(field)) =>
        (schemaMap.get(field.name): @unchecked) match {
          case None => raise(s"Unknown field name '${field.name}'.", Some(caret))
          case Some(f: Field[G, Any, Any, Any] @unchecked) =>
            ambientField(field.name) {
              prepareField[F, G](field, caret, f, variableMap, fragments, currentTypename, discoveryState)
            }
        }
      case Pos(caret, P.Selection.InlineFragmentSelection(f)) =>
        Option(f.typeCondition) match {
          case None => raise(s"Inline fragment must have a type condition.", Some(caret))
          case Some(typeCnd) =>
            matchType[F, G](typeCnd, ol, caret, discoveryState).flatMap { case (ol, specialize) =>
              prepareSelections[F, G](ol, f.selectionSet, variableMap, fragments, typeCnd, discoveryState)
                .map(Selection(_))
                .flatMap[PreparedField[G, Any]](s => nextId[F].map(id => PreparedFragField(id, typeCnd, specialize, s)))
            }
        }
      case Pos(caret, P.Selection.FragmentSpreadSelection(f)) =>
        fragments.get(f.fragmentName) match {
          case None => raise(s"Unknown fragment name '${f.fragmentName}'.", Some(caret))
          case Some(fd) =>
            ambientFragment(f.fragmentName) {
              prepareFragment[F, G](ol, fd, variableMap, fragments, fd.value.typeCnd, discoveryState)
                .flatMap[PreparedField[G, Any]] { fd =>
                  nextId[F].map(id => PreparedFragField(id, fd.typeCondition, fd.specify, Selection(fd.fields)))
                }
            }
        }
    }
  }

  type VariableMap = Map[String, Either[P.Value, Json]]

  def closeFieldParameters[F[_]: Parallel, G[_]](
      gqlField: P.Field,
      caret: Caret,
      field: Field[G, Any, Any, Any],
      variableMap: VariableMap
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]]
  ): F[Resolver[G, Any, Any]] = {
    val provided = gqlField.arguments.toList.flatMap(_.nel.toList)

    val Field(args, resolve, _, _) = field

    // Treat input arguments as an object
    // Decode the args as-if an input
    val argObj =
      P.Value.ObjectValue(provided.map(a => a.name -> a.value))

    val decObj = args match {
      case PureArg(value) if provided.isEmpty => value.fold(raise(_, None), F.pure(_))
      case PureArg(_) =>
        raise(s"Field ${gqlField.name} does not accept arguments.", Some(caret))
      case nea @ NonEmptyArg(_, _) =>
        parseInputObj[F, Any](argObj, nea, Some(variableMap), ambigiousEnum = false)
    }

    decObj.map(a => resolve.contramap[Any]((_, a)))
  }

  def prepareField[F[_]: Parallel, G[_]: Applicative](
      gqlField: P.Field,
      caret: Caret,
      field: Field[G, Any, Any, Any],
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[PreparedField[G, Any]] = {
    closeFieldParameters[F, G](gqlField, caret, field, variableMap).flatMap { resolve =>
      val tpe = field.output.value
      val ss = gqlField.selectionSet.value
      val selCaret = gqlField.selectionSet.caret

      val tn = underlyingOutputTypename(field.output.value)

      nextId[F].flatMap { id =>
        flattenResolvers[F, G](s"${currentTypename}_${gqlField.name}", resolve).flatMap { case (edges, parentName, _) =>
          def typePrep(t: Out[G, Any], parentName: String): F[Prepared[G, Any]] =
            (t, ss) match {
              case (OutArr(inner, toSeq, resolver), _) =>
                flattenResolvers[F, G](parentName, resolver).flatMap { case (edges, parentName, _) =>
                  typePrep(inner, parentName).map(cont => PreparedList(PreparedCont(edges, cont), toSeq))
                }
              case (OutOpt(inner, resolver), _) =>
                flattenResolvers[F, G](parentName, resolver).flatMap { case (edges, parentName, _) =>
                  typePrep(inner, parentName).map(cont => PreparedOption(PreparedCont(edges, cont)))
                }
              case (ol: Selectable[G, Any], Some(ss)) =>
                prepareSelections[F, G](ol, ss, variableMap, fragments, tn, discoveryState)
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
            PreparedDataField(id, gqlField.name, gqlField.alias, pc)
          }
        }
      }
    }
  }

  // def matchType[F[_], G[_]](
  //     name: String,
  //     sel: Abstract[G, Any],
  //     caret: Caret,
  //     discoveryState: SchemaShape.DiscoveryState[G]
  // )(implicit F: MonadError[F, NonEmptyChain[PositionalError]], S: Stateful[F, Prep]): F[(Selectable[G, Any], Any => Option[Any])] =
  //   if (sel.name == name) F.pure((sel, Some(_)))
  //   else {
  //     sel match {
  //       case t @ Type(n, _, _, _) =>
  //         t.implementsMap.get(name) match {
  //           case None => raise(s"Tried to match with type `$name` on type object type `$n`.", Some(caret))
  //           case Some(impl) =>
  //             val i: Interface[G, _] = impl.implementation.value
  //             val spec: _ => Option[Any] = impl.specify
  //             F.pure((i.asInstanceOf[Interface[G, Any]], spec.asInstanceOf[Any => Option[Any]]))
  //         }
  //       // What types implement this interface?
  //       case i @ Interface(n, _, _, _) =>
  //         raiseOpt(
  //           discoveryState.implementations.get(i.name),
  //           s"The interface `${i.name}` is not implemented by any type.",
  //           caret.some
  //         ).flatMap { m =>
  //           raiseOpt(
  //             m.get(name),
  //             s"`$name` does not implement interface `$n`, possible implementations are ${m.keySet.mkString(", ")}.",
  //             caret.some
  //           )
  //         }
  //       case u @ Union(n, _, _) =>
  //         u.instanceMap
  //           .get(name) match {
  //           case Some(i) => F.pure((i.tpe.value.asInstanceOf[Type[G, Any]], i.specify))
  //           case None =>
  //             u.types.toList
  //               .collectFirstSomeM { v =>
  //                 val t: Type[G, _] = v.tpe.value
  //                 matchType[F, G](name, t.asInstanceOf[Type[G, Any]], caret, discoveryState).attempt.map(_.toOption)
  //               }
  //               .flatMap {
  //                 case None =>
  //                   raise[F, (Selectable[G, Any], Any => Option[Any])](
  //                     s"`$name` is not a member of the union `$n`, possible members are ${u.instanceMap.keySet.mkString(", ")}.",
  //                     caret.some
  //                   )
  //                 case Some(x) => F.pure(x)
  //               }
  //         }
  //     }
  //   }

  sealed trait MatchResult[G[_]] { def tpe: Selectable2[G, Any] }
  object MatchResult {
    final case class SubType[G[_]](tpe: Selectable2[G, Any]) extends MatchResult[G]
    final case class SuperType[G[_]](tpe: Selectable2[G, Any]) extends MatchResult[G]
    final case class SameType[G[_]](tpe: Selectable2[G, Any]) extends MatchResult[G]
  }

  def matchType2[F[_], G[_]](
      name: String,
      sel: Selectable2[G, Any],
      caret: Caret,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit F: MonadError[F, NonEmptyChain[PositionalError]], S: Stateful[F, Prep]): F[MatchResult[G]] =
    if (sel.name == name) F.pure(MatchResult.SameType(sel))
    else {
      sel match {
        case t @ Type(n, _, _, _) =>
          // Check downcast
          t.implementsMap.get(name) match {
            case None => raise(s"Tried to match with type `$name` on type object type `$n`.", Some(caret))
            case Some(impl) =>
              val i: Interface[G, _] = impl.implementation.value
              F.pure(MatchResult.SuperType(i.asInstanceOf[Interface[G, Any]]))
          }
        // What types implement this interface?
        // We can both downcast and up-match
        case i @ Interface(n, _, _, _) =>
          i.implementsMap.get(name) match {
            case Some(impl) =>
              val i: Interface[G, _] = impl.implementation.value
              F.pure(MatchResult.SuperType(i.asInstanceOf[Interface[G, Any]]))
            case None =>
              raiseOpt(
                discoveryState.implementations.get(i.name),
                s"The interface `${i.name}` is not implemented by any type.",
                caret.some
              ).flatMap { m =>
                raiseOpt(
                  m.get(name).map { case (x, _) => MatchResult.SubType(x) },
                  s"`$name` does not implement interface `$n`, possible implementations are ${m.keySet.mkString(", ")}.",
                  caret.some
                )
              }
          }
        // Can match to any type or any of it's types' interfacees
        case u @ Union(n, _, _) =>
          u.instanceMap
            .get(name) match {
            case Some(i) => F.pure(MatchResult.SubType(i.tpe.value.asInstanceOf[Type[G, Any]]))
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
                case Some(x) => F.pure(MatchResult.SubType(x.implementation.value.asInstanceOf[Interface[G, Any]]))
              }
          }
      }
    }

  // name is the type in the pattern match case
  // sel is the type we match on
  // sel match { case x if x.name == name  => ... }
  def matchType[F[_], G[_]](
      name: String,
      sel: Selectable[G, Any],
      caret: Caret,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit F: MonadError[F, NonEmptyChain[PositionalError]], S: Stateful[F, Prep]): F[(Selectable[G, Any], Any => Option[Any])] =
    if (sel.name == name) F.pure((sel, Some(_)))
    else {
      sel match {
        case t @ Type(n, _, _, _) =>
          t.implementsMap.get(name) match {
            case None => raise(s"Tried to match with type `$name` on type object type `$n`.", Some(caret))
            case Some(impl) =>
              val i: Interface[G, _] = impl.implementation.value
              val spec: _ => Option[Any] = impl.specify
              F.pure((i.asInstanceOf[Interface[G, Any]], spec.asInstanceOf[Any => Option[Any]]))
          }
        // What types implement this interface?
        case i @ Interface(n, _, _, _) =>
          raiseOpt(
            discoveryState.implementations.get(i.name),
            s"The interface `${i.name}` is not implemented by any type.",
            caret.some
          ).flatMap { m =>
            raiseOpt(
              m.get(name),
              s"`$name` does not implement interface `$n`, possible implementations are ${m.keySet.mkString(", ")}.",
              caret.some
            )
          }
        case u @ Union(n, _, _) =>
          u.instanceMap
            .get(name) match {
            case Some(i) => F.pure((i.tpe.value.asInstanceOf[Type[G, Any]], i.specify))
            case None =>
              u.types.toList
                .collectFirstSomeM { v =>
                  val t: Type[G, _] = v.tpe.value
                  matchType[F, G](name, t.asInstanceOf[Type[G, Any]], caret, discoveryState).attempt.map(_.toOption)
                }
                .flatMap {
                  case None =>
                    raise[F, (Selectable[G, Any], Any => Option[Any])](
                      s"`$name` is not a member of the union `$n`, possible members are ${u.instanceMap.keySet.mkString(", ")}.",
                      caret.some
                    )
                  case Some(x) => F.pure(x)
                }
          }
      }
    }

  def prepareFragment[F[_]: Parallel, G[_]: Applicative](
      ol: Selectable[G, Any],
      f: Pos[P.FragmentDefinition],
      variableMap: VariableMap,
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String,
      discoveryState: SchemaShape.DiscoveryState[G]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, NonEmptyChain[PositionalError]],
      D: Defer[F]
  ): F[FragmentDefinition[G, Any]] =
    D.defer {
      S.get.flatMap {
        case c if c.cycleSet(f.value.name) =>
          raise(s"Fragment by name ${f.value.name} is cyclic. Hint: graphql queries must be finite.", Some(f.caret))
        case _ =>
          val beforeF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet + f.value.name))
          val afterF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet - f.value.name))

          val programF: F[FragmentDefinition[G, Any]] =
            matchType[F, G](f.value.typeCnd, ol, f.caret, discoveryState)
              .flatMap { case (t, specify) =>
                prepareSelections[F, G](t, f.value.selectionSet, variableMap, fragments, currentTypename, discoveryState)
                  .map(FragmentDefinition(f.value.name, f.value.typeCnd, specify, _))
              }

          beforeF *> programF <* afterF
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
      case InArr(of, _)          => s"${inName(of)}"
      case Enum(name, _, _)      => name
      case Scalar(name, _, _, _) => name
      case InOpt(of)             => s"${inName(of, inOption = true)}"
      case Input(name, _, _)     => name
    }
    rec + suffix
  }

  def parseInputObj[F[_]: Parallel, A](
      v: P.Value.ObjectValue,
      fields: NonEmptyArg[A],
      variableMap: Option[VariableMap],
      ambigiousEnum: Boolean
  )(implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
  ): F[A] = {
    val xs = v.v

    val m = xs.toMap
    val required = fields.nec.map(x => x.name -> x).toList.toMap

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
  ) = {
    val fa =
      input.get(a.name) match {
        case None =>
          a.defaultValue match {
            case None =>
              a.input.value match {
                case InOpt(_) => F.pure(P.Value.NullValue)
                case _ =>
                  raise[F, P.Value](s"Required input for field '${a.name}' was not provided and has no default value.", None)
              }
            // TODO this value being parsed can probably be cached, since the default is the same for every query
            case Some(dv) => F.pure(valueToParserValue(dv))
          }
        case Some(x) => F.pure(x)
      }

    ambientArg(a.name) {
      fa.flatMap(parseInput[F, A](_, a.input.value, variableMap, ambigiousEnum))
    }
  }

  def parseArg[F[_]: Parallel, A](arg: Arg[A], input: Map[String, P.Value], variableMap: Option[VariableMap], ambigiousEnum: Boolean)(
      implicit
      F: MonadError[F, NonEmptyChain[PositionalError]],
      S: Stateful[F, Prep]
  ): F[A] = {
    // All provided fields are of the correct type
    // All required fields are either defiend or defaulted
    val fieldsF: F[Chain[(String, Any)]] =
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

    val rootTypename =
      ot match {
        case P.OperationType.Query        => "Query"
        case P.OperationType.Mutation     => "Mutation"
        case P.OperationType.Subscription => "Subscription"
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
          prepareSelections[F, G](
            root.asInstanceOf[Type[G, Any]],
            selection,
            vm,
            frags.map(f => f.value.name -> f).toMap,
            rootTypename,
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
