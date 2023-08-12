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

import gql._
import gql.ast._
import gql.resolver._
import gql.parser.{QueryAst => QA, AnyValue}
import cats._
import cats.mtl._
import cats.data._
import cats.implicits._
import io.circe._

trait QueryPreparation[F[_], G[_], C] {
  import QueryPreparation._

  def prepareStep[I, O](
      step: Step[G, I, O],
      fieldMeta: PartialFieldMeta[C]
  ): K[F, G, PreparedStep[G, I, O]]

  def prepare[A](
      fi: MergedFieldInfo[G, C],
      t: Out[G, A],
      fieldMeta: PartialFieldMeta[C]
  ): K[F, G, Prepared[G, A]]

  def prepareField[I, O](
      fi: MergedFieldInfo[G, C],
      field: Field[G, I, O],
      currentTypename: String
  ): F[List[PreparedDataField[G, I, ?]]]

  def mergeImplementations[A](
      base: Selectable[G, A],
      sels: NonEmptyList[SelectionInfo[G, C]]
  ): F[NonEmptyList[MergedImplementation[G, A, ?, C]]]

  def prepareSelectable[A](
      s: Selectable[G, A],
      sis: NonEmptyList[SelectionInfo[G, C]]
  ): F[NonEmptyList[PreparedSpecification[G, A, ?]]]
}

object QueryPreparation {
  type H[F[_], A] = Kleisli[WriterT[F, List[(Arg[?], Any)], *], UniqueEdgeCursor, A]
  case class K[F[_], G[_], A](value: H[F, Eval[PreparedMeta[G]] => A])
  implicit def applicativeForK[F[_], G[_]](implicit F: Parallel[F]): Applicative[K[F, G, *]] = new Applicative[K[F, G, *]] {
    val P = Parallel[H[F, *]]
    override def ap[A, B](ff: K[F, G, A => B])(fa: K[F, G, A]): K[F, G, B] = K(
      P.sequential {
        P.applicative.map2(P.parallel(ff.value), P.parallel(fa.value))((f, g) => (m: Eval[PreparedMeta[G]]) => f(m)(g(m)))
      }
    )

    override def pure[A](x: A): K[F, G, A] = {
      implicit val F2 = F.monad
      K(Kleisli.pure(_ => x))
    }
  }

  def apply[F[_]: Parallel, G[_], C](
      variables: VariableMap[C],
      implementations: SchemaShape.Implementations[G]
  )(implicit
      F: Monad[F],
      AP: ArgParsing[F, C],
      S: Stateful[F, Int],
      EA: ErrorAlg[F, C],
      DA: DirectiveAlg[F, G, C]
  ) = {
    import EA._

    final case class FoundImplementation[A, B](
        tpe: Type[G, B],
        specify: A => Option[B]
    )

    def findImplementations[A](
        s: Selectable[G, A]
    ): List[FoundImplementation[A, ?]] = s match {
      case t: Type[G, ?] => List(FoundImplementation(t, Some(_)))
      case u: Union[G, ?] =>
        u.types.toList.map { case x: gql.ast.Variant[G, A, b] =>
          FoundImplementation(x.tpe.value, x.specify)
        }
      case it @ Interface(_, _, _, _) =>
        val m: Map[String, SchemaShape.InterfaceImpl[G, A]] =
          implementations
            .get(it.name)
            .getOrElse(Map.empty)
            .collect { case (k, v: SchemaShape.InterfaceImpl[G, A] @unchecked) => (k, v) }

        m.values.toList
          .collect { case ti: SchemaShape.InterfaceImpl.TypeImpl[G, A, b] => FoundImplementation(ti.t, ti.specify) }
    }

    type L[A] = K[F, G, A]

    def lift[A](fa: F[A]): H[F, A] = Kleisli.liftF(WriterT.liftF(fa))
    def liftK[A](fa: F[A]): L[A] = K(lift(fa).map(a => _ => a))

    def pure[A](a: A): H[F, A] = lift(F.pure(a))
    def pureK[A](a: A): L[A] = K(pure(_ => a))

    val L = Local[H[F, *], UniqueEdgeCursor]

    val W = Tell[H[F, *], List[(Arg[?], Any)]]

    def inK[A](p: String)(fa: L[A]): L[A] = K(L.local(fa.value)(_ append p))

    def askK: L[UniqueEdgeCursor] = K(L.ask[UniqueEdgeCursor].map(x => (_: Eval[PreparedMeta[G]]) => x))

    def nextId = S.get <* S.modify(_ + 1)

    new QueryPreparation[F, G, C] {
      override def prepareStep[I, O](
          step: Step[G, I, O],
          fieldMeta: PartialFieldMeta[C]
      ): L[PreparedStep[G, I, O]] = {
        def rec[I2, O2](
            step: Step[G, I2, O2],
            edge: String
        ): L[PreparedStep[G, I2, O2]] = inK(edge) {
          prepareStep[I2, O2](step, fieldMeta)
        }

        step match {
          case Step.Alg.Lift(f)      => pureK(PreparedStep.Lift(f))
          case Step.Alg.EmbedError() => pureK(PreparedStep.EmbedError[G, O]())
          case alg: Step.Alg.Compose[?, i, a, o] =>
            val left = rec[i, a](alg.left, "compose-left")
            val right = rec[a, o](alg.right, "compose-right")
            (left, right).mapN(PreparedStep.Compose[G, i, a, o](_, _))
          case _: Step.Alg.EmbedEffect[?, i] => askK.map(PreparedStep.EmbedEffect[G, i](_))
          case alg: Step.Alg.EmbedStream[?, i] =>
            askK.map(PreparedStep.EmbedStream[G, i](alg.signal, _))
          case alg: Step.Alg.Choose[?, a, b, c, d] =>
            val left = rec[a, c](alg.fac, "choice-left")
            val right = rec[b, d](alg.fab, "choice-right")
            (left, right).mapN(PreparedStep.Choose[G, a, b, c, d](_, _))
          case _: Step.Alg.GetMeta[?, i] =>
            K(pure((pm: Eval[PreparedMeta[G]]) => PreparedStep.GetMeta[G, I](pm)))
          case alg: Step.Alg.Batch[?, k, v] =>
            liftK(nextId.map(i => PreparedStep.Batch[G, k, v](alg.id, UniqueBatchInstance(i))))
          case alg: Step.Alg.First[?, i, o, c] =>
            rec[i, o](alg.step, "first").map(PreparedStep.First[G, i, o, c](_))
          case alg: Step.Alg.Argument[?, a] =>
            val expected = alg.arg.entries.toList.map(_.name).toSet
            val fields = fieldMeta.fields.filter { case (k, _) => expected.contains(k) }
            K {
              lift(AP.decodeArg(alg.arg, fields.fmap(_.map(List(_))), ambigiousEnum = false, context = Nil))
                .flatTap(a => W.tell(List(alg.arg -> a)))
                .map(o => _ => PreparedStep.Lift[G, I, O](_ => o))
            }
        }
      }

      override def prepare[A](
          fi: MergedFieldInfo[G, C],
          t: Out[G, A],
          fieldMeta: PartialFieldMeta[C]
      ): L[Prepared[G, A]] =
        (t, fi.selections.toNel) match {
          case (out: gql.ast.OutArr[g, a, c, b], _) =>
            val innerStep: Step[G, a, b] = out.resolver.underlying
            val compiledStep = prepareStep[a, b](innerStep, fieldMeta)
            val compiledCont = prepare[b](fi, out.of, fieldMeta)
            (compiledStep, compiledCont).mapN((s, c) => PreparedList(PreparedCont(s, c), out.toSeq))
          case (out: gql.ast.OutOpt[g, a, b], _) =>
            val innerStep: Step[G, a, b] = out.resolver.underlying
            val compiledStep = prepareStep[a, b](innerStep, fieldMeta)
            val compiledCont = prepare[b](fi, out.of, fieldMeta)
            (compiledStep, compiledCont).mapN((s, c) => PreparedOption(PreparedCont(s, c)))
          case (s: Selectable[G, a], Some(ss)) =>
            liftK(prepareSelectable[a](s, ss).map(xs => Selection(xs.toList)))
          case (e: Enum[a], None) =>
            pureK(PreparedLeaf(e.name, x => Json.fromString(e.revm(x))))
          case (s: Scalar[a], None) =>
            import io.circe.syntax._
            pureK(PreparedLeaf(s.name, x => s.encoder(x).asJson))
          case (o, Some(_)) =>
            liftK(raise(s"Type `${ModifierStack.fromOut(o).show(_.name)}` cannot have selections.", List(fi.caret)))
          case (o, None) =>
            liftK(raise(s"Object like type `${ModifierStack.fromOut(o).show(_.name)}` must have a selection.", List(fi.caret)))
        }

      override def prepareField[I, O](
          fi: MergedFieldInfo[G, C],
          field: Field[G, I, O],
          currentTypename: String
      ): F[List[PreparedDataField[G, I, ?]]] = {
        DA
          .foldDirectives[Position.Field[G, *]][List, (Field[G, I, ?], MergedFieldInfo[G, C])](fi.directives, List(fi.caret))(
            (field, fi)
          ) { case ((f: Field[G, I, ?], fi), p: Position.Field[G, a], d) =>
            DA.parseArg(p, d.arguments, List(fi.caret))
              .map(p.handler(_, f, fi))
              .flatMap(raiseEither(_, List(fi.caret)))
          }
          .flatMap(_.parTraverse { case (field: Field[G, I, o2], fi) =>
            val rootUniqueName = UniqueEdgeCursor(s"${currentTypename}_${fi.name}")

            val meta: PartialFieldMeta[C] = PartialFieldMeta(fi.alias, fi.args)

            def findArgs(o: Out[G, ?]): Chain[Arg[?]] = o match {
              case x: OutArr[g, a, c, b] => collectArgs(x.resolver.underlying) ++ findArgs(x.of)
              case x: OutOpt[g, a, b]    => collectArgs(x.resolver.underlying) ++ findArgs(x.of)
              case _                     => Chain.empty
            }

            val providedArgNames = meta.fields.keySet

            val declaredArgs: Chain[Arg[?]] = collectArgs(field.resolve.underlying) ++ findArgs(field.output.value)
            val declaredArgNames = declaredArgs.toList.flatMap(_.entries.toList.map(_.name)).toSet

            val tooMany = providedArgNames -- declaredArgNames

            val verifyTooManyF: F[Unit] =
              if (tooMany.isEmpty) F.unit
              else
                raise(
                  s"Too many arguments provided for field `${fi.name}`. Provided: ${providedArgNames.toList
                      .map(x => s"'$x'")
                      .mkString(", ")}. Declared: ${declaredArgNames.toList.map(x => s"'$x'").mkString(", ")}",
                  List(fi.caret)
                )

            val preparedF = (
              prepareStep(field.resolve.underlying, meta),
              prepare(fi, field.output.value, meta)
            ).tupled

            val out = preparedF.value.run(rootUniqueName).run.map { case (w, f) =>
              val g = f.andThen { case (x, y) =>
                PreparedDataField(fi.name, fi.alias, PreparedCont(x, y), field, w.toMap)
              }
              lazy val pdf: PreparedDataField[G, I, ?] = g {
                Eval.later {
                  PreparedMeta(
                    variables.map { case (k, v) => k -> v.copy(value = v.value.map(_.void)) },
                    meta.args.map(_.map(_ => ())),
                    pdf
                  )
                }
              }
              pdf
            }

            verifyTooManyF &> out
          })
      }

      override def mergeImplementations[A](
          base: Selectable[G, A],
          sels: NonEmptyList[SelectionInfo[G, C]]
      ): F[NonEmptyList[MergedImplementation[G, A, _, C]]] = {
        // We need to find all implementations of the base type
        val concreteBaseMap = findImplementations[A](base).map(x => x.tpe.name -> x).toMap

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
          val concreteIntersections = findImplementations(sel.s)
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
          // TODO at a glance, there might be some field duplication here
          val sels = fields.toList
            .map(_.tpe.inner)
            .collect { case s: TypeInfo.Selectable[G, C] => s.selection.toList }
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

        val collected: F[List[MergedImplementation[G, A, ?, C]]] = concreteBase.parFlatTraverse {
          case (k, (fi: FoundImplementation[A, b])) =>
            val t = fi.tpe
            val specify = fi.specify
            merged.get(k).toList.traverse { fields =>
              fields.toNonEmptyList
                .parTraverse { f =>
                  if (f.name === "__typename")
                    F.pure(PairedFieldSelection[G, b, C](f, gql.dsl.lift[b](_ => t.name)))
                  else {
                    t.fieldMap.get(f.name) match {
                      case None =>
                        raise[PairedFieldSelection[G, b, C]](s"Could not find field '${f.name}' on type `${t.name}`.", Nil)
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
              raise[NonEmptyList[MergedImplementation[G, A, ?, C]]](
                s"Could not find any implementations of `${base.name}` in the selection set.",
                Nil
              )
          }
        }
      }

      override def prepareSelectable[A](
          s: Selectable[G, A],
          sis: NonEmptyList[SelectionInfo[G, C]]
      ): F[NonEmptyList[PreparedSpecification[G, A, _]]] =
        mergeImplementations[A](s, sis).flatMap { impls =>
          impls.parTraverse[F, PreparedSpecification[G, A, ?]] { case impl: MergedImplementation[G, A, b, C] =>
            val fa = impl.selections.toList.parFlatTraverse { sel =>
              sel.field match {
                case field: Field[G, b2, t] => prepareField[b, t](sel.info, field, impl.leaf.name)
              }
            }

            fa.map(xs => PreparedSpecification[G, A, b](s.name, impl.specify, xs))
          }
        }
    }
  }
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

final case class PartialFieldMeta[C](
    alias: Option[String],
    args: Option[QA.Arguments[C, AnyValue]]
) {
  lazy val fields = args.map(_.nel.toList).getOrElse(Nil).map(x => x.name -> x.value).toMap
}
