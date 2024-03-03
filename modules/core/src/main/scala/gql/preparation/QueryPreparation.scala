/*
 * Copyright 2024 Valdemar Grange
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

import gql._
import gql.ast._
import gql.resolver._
import gql.parser.{QueryAst => QA, AnyValue}
import cats._
import cats.data._
import cats.implicits._
import io.circe._
import gql.std.LazyT
import org.typelevel.scalaccompat.annotation._

class QueryPreparation[F[_], C](
    ap: ArgParsing[C],
    da: DirectiveAlg[F, C],
    variables: VariableMap[C],
    implementations: SchemaShape.Implementations[F]
) {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]
  type Write[A] = WriterT[G, Chain[(Arg[?], Any)], A]
  type Analyze[A] = LazyT[Write, PreparedMeta[F], A]
  implicit val L: Applicative[Analyze] = LazyT.applicativeForParallelLazyT

  def liftK[A](fa: G[A]): Analyze[A] = LazyT.liftF(WriterT.liftF(fa))

  val nextNodeId = G.nextId.map(NodeId(_))

  def findImplementations[A](
      s: Selectable[F, A]
  ): List[Specialization[F, A, ?]] = s match {
    case t: Type[F, ?] => List(Specialization.Type(t))
    case u: Union[F, ?] =>
      u.types.toList.map { case x: gql.ast.Variant[F, A, b] =>
        Specialization.Union(u, x)
      }
    case it @ Interface(_, _, _, _) =>
      val m: Map[String, SchemaShape.InterfaceImpl[F, A]] =
        implementations
          .get(it.name)
          .getOrElse(Map.empty)
          .collect { case (k, v: SchemaShape.InterfaceImpl[F, A] @unchecked) => (k, v) }

      m.values.toList
        .collect { case ti: SchemaShape.InterfaceImpl.TypeImpl[F, A, b] =>
          Specialization.Interface(ti.t, ti.impl)
        }
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def prepareStep[I, O](
      step: Step[F, I, O],
      fieldMeta: PartialFieldMeta[C],
      uec: UniqueEdgeCursor
  ): Analyze[PreparedStep[F, I, O]] = {
    val nextId = liftK(nextNodeId).map(StepEffectId(_, uec))

    def rec[I2, O2](
        step: Step[F, I2, O2],
        edge: String
    ): Analyze[PreparedStep[F, I2, O2]] =
      prepareStep[I2, O2](step, fieldMeta, uec append edge)

    step match {
      case Step.Alg.Lift(f)      => liftK(nextNodeId).map(PreparedStep.Lift(_, f))
      case Step.Alg.EmbedError() => liftK(nextNodeId).map(PreparedStep.EmbedError[F, O](_))
      case alg: Step.Alg.Compose[F, i, a, o] =>
        val left = rec[i, a](alg.left, "compose-left")
        val right = rec[a, o](alg.right, "compose-right")
        (liftK(nextNodeId), left, right).mapN(PreparedStep.Compose.apply[F, i, a, o])
      case _: Step.Alg.EmbedEffect[F, i] =>
        nextId.map(PreparedStep.EmbedEffect[F, i](_))
      case alg: Step.Alg.EmbedStream[F, i] =>
        nextId.map(PreparedStep.EmbedStream[F, i](alg.signal, _))
      case alg: Step.Alg.Choose[F, a, b, c, d] =>
        val left = rec[a, c](alg.fac, "choice-left")
        val right = rec[b, d](alg.fab, "choice-right")
        (liftK(nextNodeId), left, right).mapN(PreparedStep.Choose.apply[F, a, b, c, d])
      case _: Step.Alg.GetMeta[?, i] =>
        (liftK(nextNodeId), LazyT.id[Write, PreparedMeta[F]]).mapN(PreparedStep.GetMeta.apply[F, I])
      case alg: Step.Alg.Batch[F, k, v] =>
        liftK(G.nextId.map(i => PreparedStep.Batch[F, k, v](alg.id, UniqueBatchInstance(NodeId(i)))))
      case alg: Step.Alg.InlineBatch[F, k, v] =>
        nextId.map(PreparedStep.InlineBatch[F, k, v](alg.run, _))
      case alg: Step.Alg.First[F, i, o, c] =>
        (liftK(nextNodeId), rec[i, o](alg.step, "first")).mapN(PreparedStep.First.apply[F, i, o, c])
      case alg: Step.Alg.Argument[?, a] =>
        val expected = alg.arg.entries.toList.map(_.name).toSet
        val fields = fieldMeta.fields.filter { case (k, _) => expected.contains(k) }
        LazyT.liftF {
          WriterT {
            nextNodeId.flatMap { nid =>
              ap
                .decodeArg(alg.arg, fields.fmap(_.map(List(_))), ambigiousEnum = false, context = Nil)
                .map(a => (Chain(alg.arg -> a), PreparedStep.Lift[F, I, O](nid, _ => a)))
            }
          }
        }
    }
  }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def prepare[A](
      fi: MergedFieldInfo[F, C],
      t: Out[F, A],
      fieldMeta: PartialFieldMeta[C],
      uec: UniqueEdgeCursor
  ): Analyze[Prepared[F, A]] =
    (t, fi.selections.toNel) match {
      case (out: gql.ast.OutArr[F, a, c, b], _) =>
        val innerStep: Step[F, a, b] = out.resolver.underlying
        val compiledStep = prepareStep[a, b](innerStep, fieldMeta, uec)
        val compiledCont = prepare[b](fi, out.of, fieldMeta, uec append "in-arr")
        (compiledStep, compiledCont, liftK(nextNodeId)).mapN { (s, c, nid) =>
          PreparedList(nid, PreparedCont(s, c), out.toSeq)
        }
      case (out: gql.ast.OutOpt[F, a, b], _) =>
        val innerStep: Step[F, a, b] = out.resolver.underlying
        val compiledStep = prepareStep[a, b](innerStep, fieldMeta, uec)
        val compiledCont = prepare[b](fi, out.of, fieldMeta, uec append "in-opt")
        (compiledStep, compiledCont, liftK(nextNodeId)).mapN { (s, c, nid) =>
          PreparedOption(nid, PreparedCont(s, c))
        }
      case (s: Selectable[F, a], Some(ss)) =>
        liftK(prepareSelectable[A](s, ss).widen[Prepared[F, A]])
      case (e: Enum[a], None) =>
        liftK(nextNodeId).map(PreparedLeaf(_, e.name, x => Json.fromString(e.revm(x))))
      case (s: Scalar[a], None) =>
        import io.circe.syntax._
        liftK(nextNodeId).map(PreparedLeaf(_, s.name, x => s.encoder(x).asJson))
      case (o, Some(_)) =>
        liftK(G.raise(s"Type `${ModifierStack.fromOut(o).show(_.name)}` cannot have selections.", List(fi.caret)))
      case (o, None) =>
        liftK(G.raise(s"Object like type `${ModifierStack.fromOut(o).show(_.name)}` must have a selection.", List(fi.caret)))
    }

  @nowarn3("msg=.*cannot be checked at runtime because its type arguments can't be determined.*")
  def prepareField[I, O](
      fi: MergedFieldInfo[F, C],
      field: Field[F, I, O],
      currentTypename: String
  ): G[List[PreparedDataField[F, I, ?]]] = {
    da
      .foldDirectives[Position.Field[F, *]][List, (Field[F, I, ?], MergedFieldInfo[F, C])](fi.directives, List(fi.caret))(
        (field, fi)
      ) { case ((f: Field[F, I, ?], fi), p: Position.Field[F, a], d) =>
        da.parseArg(p, d.arguments, List(fi.caret))
          .map(p.handler(_, f, fi))
          .flatMap(G.raiseEither(_, List(fi.caret)))
      }
      .flatMap(_.parTraverse { case (field: Field[F, I, o2], fi) =>
        val rootUniqueName = UniqueEdgeCursor(s"${currentTypename}_${fi.name}")

        val meta: PartialFieldMeta[C] = PartialFieldMeta(fi.alias, fi.args)

        def findArgs(o: Out[F, ?]): Chain[Arg[?]] = o match {
          case x: OutArr[F, a, c, b] => collectArgs(x.resolver.underlying) ++ findArgs(x.of)
          case x: OutOpt[F, a, b]    => collectArgs(x.resolver.underlying) ++ findArgs(x.of)
          case _                     => Chain.empty
        }

        val providedArgNames = meta.fields.keySet

        val declaredArgs: Chain[Arg[?]] = collectArgs(field.resolve.underlying) ++ findArgs(field.output.value)
        val declaredArgNames = declaredArgs.toList.flatMap(_.entries.toList.map(_.name)).toSet

        val tooMany = providedArgNames -- declaredArgNames

        val verifyTooManyF: G[Unit] =
          if (tooMany.isEmpty) G.unit
          else
            G.raise(
              s"Too many arguments provided for field `${fi.name}`. Provided: ${providedArgNames.toList
                  .map(x => s"'$x'")
                  .mkString(", ")}. Declared: ${declaredArgNames.toList.map(x => s"'$x'").mkString(", ")}",
              List(fi.caret)
            )

        val preparedF = (
          prepareStep(field.resolve.underlying, meta, rootUniqueName),
          prepare(fi, field.output.value, meta, rootUniqueName append "in-root")
        ).tupled

        val pdfF: LazyT[G, PreparedMeta[F], PreparedDataField[F, I, ?]] =
          (liftK(nextNodeId), preparedF).tupled.mapF(_.run.map { case (w, f) =>
            f.andThen { case (nid, (x, y)) =>
              PreparedDataField(nid, fi.name, fi.alias, PreparedCont(x, y), field, w.toList.toMap)
            }
          })

        val out = pdfF.runWithValue { pdf =>
          PreparedMeta(
            variables.map { case (k, v) => k -> v.copy(value = v.value.map(_.void)) },
            meta.args.map(_.map(_ => ())),
            pdf
          )
        }

        verifyTooManyF &> out
      })
  }

  def mergeImplementations[A](
      base: Selectable[F, A],
      sels: NonEmptyList[SelectionInfo[F, C]]
  ): G[NonEmptyList[MergedSpecialization[F, A, ?, C]]] = {
    // We need to find all implementations of the base type
    val concreteBaseMap = findImplementations[A](base).map(x => x.target.name -> x).toMap

    val concreteBase: List[(String, Specialization[F, A, ?])] =
      concreteBaseMap.toList

    type Typename = String
    // Concrete type of caller match type -> fields for that type
    // If A <: I and B <: I (subtype <: supertype) and we select name on I,
    // then we have List("A" -> NonEmptyList.of(nameField), "B" -> NonEmptyList.of(nameField))
    val nestedSelections: List[(Typename, NonEmptyList[FieldInfo[F, C]])] = sels.toList.flatMap { sel =>
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

      // What typenames implement whatever the caller matched on
      val concreteIntersections = findImplementations(sel.s).map(_.target.name)

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

    type FieldName = String
    // There may be more than one field with the same name
    // This is fine, but we need to merge their implementations
    val grouped: Map[Typename, NonEmptyMap[FieldName, NonEmptyList[FieldInfo[F, C]]]] = nestedSelections
      .groupMap { case (k, _) => k } { case (_, vs) => vs }
      .collect { case (k, x :: xs) => k -> NonEmptyList(x, xs).flatten.groupByNem(_.outputName) }

    // Since we have multiple fields implementations for each fieldname
    // we pick one of them as the correct one
    // if the schema is valid, any one field on same the typename should be equivalent
    val merged: Map[Typename, NonEmptyMap[FieldName, MergedFieldInfo[F, C]]] =
      grouped.fmap(_.fmap { fields =>
        // TODO at a glance, there might be some field duplication here
        val sels = fields.toList
          .map(_.tpe.inner)
          .collect { case s: TypeInfo.Selectable[F, C] => s.selection.toList }
          .flatten
        MergedFieldInfo(
          fields.head.name,
          fields.head.alias,
          fields.head.args,
          sels,
          fields.head.directives,
          fields.head.caret,
          fields.head.path
        )
      })

    // For every concrete implementation of the ast type (possible type)
    // We find the selection for that type (and omit it if the type was not selected)
    val collected: G[List[MergedSpecialization[F, A, ?, C]]] = concreteBase.parFlatTraverse { case (k, (sp: Specialization[F, A, b])) =>
      val t = sp.target
      merged.get(k).toList.traverse { fields =>
        fields.toNonEmptyList
          .parTraverse { f =>
            if (f.name === "__typename")
              G.pure(PairedFieldSelection[F, b, C](f, gql.dsl.field.lift[b](_ => t.name)))
            else {
              t.fieldMap.get(f.name) match {
                case None =>
                  G.raise[PairedFieldSelection[F, b, C]](s"Could not find field '${f.name}' on type `${t.name}`.", Nil)
                case Some(field) => G.pure(PairedFieldSelection[F, b, C](f, field))
              }
            }
          }
          .map(fields => MergedSpecialization[F, A, b, C](sp, fields))
      }
    }

    collected.flatMap { xs =>
      xs.toNel match {
        case Some(x) => G.pure(x)
        case None =>
          G.raise[NonEmptyList[MergedSpecialization[F, A, ?, C]]](
            s"Could not find any implementations of `${base.name}` in the selection set.",
            Nil
          )
      }
    }
  }

  def prepareSelectable[A](
      s: Selectable[F, A],
      sis: NonEmptyList[SelectionInfo[F, C]]
  ): G[Selection[F, A]] =
    mergeImplementations[A](s, sis)
      .flatMap { impls =>
        impls.parTraverse[G, PreparedSpecification[F, A, ?]] { case impl: MergedSpecialization[F, A, b, C] =>
          val fa = impl.selections.toList.parFlatTraverse { sel =>
            sel.field match {
              case field: Field[F, b2, t] => prepareField[b, t](sel.info, field, impl.spec.typename)
            }
          }

          nextNodeId.flatMap(nid => fa.map(xs => PreparedSpecification[F, A, b](nid, impl.spec, xs)))
        }
      }
      .flatMap(xs => nextNodeId.map(nid => Selection(nid, xs.toList, s)))
}

final case class MergedFieldInfo[G[_], C](
    name: String,
    alias: Option[String],
    args: Option[QA.Arguments[C, AnyValue]],
    selections: List[SelectionInfo[G, C]],
    directives: Option[QA.Directives[C, AnyValue]],
    // TODO these two should probably be lists
    caret: C,
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

final case class MergedSpecialization[G[_], A, B, C](
    spec: Specialization[G, A, B],
    selections: NonEmptyList[PairedFieldSelection[G, B, C]]
)

final case class PartialFieldMeta[C](
    alias: Option[String],
    args: Option[QA.Arguments[C, AnyValue]]
) {
  lazy val fields = args.map(_.nel.toList).getOrElse(Nil).map(x => x.name -> x.value).toMap
}
