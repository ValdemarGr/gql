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
import gql._
import gql.ast._
import gql.parser.AnyValue
import gql.parser.NonVar
import gql.parser.{Value => V}

class ArgParsing[C](typeMap: TypeMap) {
  type G[A] = Alg[C, A]
  val G = Alg.Ops[C]

  def decodeIn[A](
      a: In[A],
      value: V[AnyValue, List[C]],
      ambigiousEnum: Boolean
  ): G[A] = {
    (a, value) match {
      case (_, V.VariableValue(vn, cs)) =>
        G.useVariable(vn) *> {
          val typeNotFound = G.raise(
            s"Variable '$$$vn' was not declared and provided as a possible variable for this operation. Hint add the variable to the variables list of the operation '(..., $$$vn: ${ModifierStack
                .fromIn(a)
                .show(_.name)})' and provide a value in the variables parameter.",
            cs
          )
          typeMap.get(vn) match {
            case None => typeNotFound
            case Some(tpe) =>
              val vt: ModifierStack[String] = ModifierStack.fromType(tpe)
              val at = ModifierStack.fromIn(a)

              def showType(xs: List[Modifier], name: String): String =
                ModifierStack(xs, name).show(identity)

              def showVarType(xs: List[Modifier]): String =
                showType(xs, vt.inner)

              def showArgType(xs: List[Modifier]): String =
                showType(xs, at.inner.name)

              lazy val prefix =
                s"Variable '$$${vn}' of type `${vt.show(identity)}` was not compatible with expected argument type `${at.map(_.name).show(identity)}`"

              def remaining(vs: List[Modifier], as: List[Modifier]): String =
                if (vs.size == vt.modifiers.size && as.size == at.modifiers.size) "."
                else
                  s". The remaining type for the variable `${showVarType(vs)}` is not compatible with the remaining type for the argument `${showArgType(as)}`"

              def showModifier(m: Option[Modifier]): String = m match {
                case None                   => "no modifiers"
                case Some(Modifier.NonNull) => "a non-null modifier"
                case Some(Modifier.List)    => "a list modifier"
              }

              /*
               * We must verify if the variable may occur here by comparing the type of the variable with the type of the arg
               * If we don't do this, variables will be structurally typed
               * Var should be more constrained than the arg
               * a ::= [a] | a! | A
               * v ::= [v] | v! | V
               * a compat v ::= ok | fail
               *
               *  a  compat  v  -> outcome
               * --------------------------
               *  A  compat  V  -> ok
               *  a! compat  v! -> a compat v
               *  a! compat [v] -> fail
               *  a! compat  V  -> fail
               * [a] compat  v! -> [a] compat v
               *  A  compat  v! -> A compat v
               * [a] compat [v] -> a compat v
               * [a] compat  V  -> fail
               *  A  compat [v] -> fail
               */
              def verifyTypeShape(argShape: List[Modifier], varShape: List[Modifier]): G[Unit] =
                (argShape, varShape) match {
                  // A compat V
                  case (Nil, Nil) => G.unit
                  // a! compat v! -> ok
                  case (Modifier.NonNull :: xs, Modifier.NonNull :: ys) => verifyTypeShape(xs, ys)
                  // a! compat ([v] | V) -> fail
                  case (Modifier.NonNull :: _, (Modifier.List :: _) | Nil) =>
                    G.raise(
                      s"${prefix}, because the argument expected a not-null (!) modifier, but was given ${showModifier(
                          varShape.headOption
                        )}${remaining(varShape, argShape)}",
                      cs
                    )
                  // ([a] | A) compat v! -> ok
                  case (xs, Modifier.NonNull :: ys) => verifyTypeShape(xs, ys)
                  // [a] compat [v] -> ok
                  case (Modifier.List :: xs, Modifier.List :: ys) => verifyTypeShape(xs, ys)
                  // [a] compat V -> fail
                  case (Modifier.List :: _, Nil) =>
                    G.raise(
                      s"${prefix}, because the argumented expected a list modifier ([A]) but no more modifiers were provided${remaining(varShape, argShape)}",
                      cs
                    )
                  // A compat [v] -> fail
                  case (Nil, Modifier.List :: _) =>
                    G.raise(
                      s"${prefix}, because the argumented expected no more modifiers but was given a list modifier ([A])${remaining(varShape, argShape)}",
                      cs
                    )
                }

              val verifiedF: G[Unit] = verifyTypeShape(at.modifiers, vt.modifiers)

              val verifiedTypenameF: G[Unit] =
                if (vt.inner === at.inner.name) G.unit
                else
                  G.raise(
                    s"${prefix}, typename of the variable '${vt.inner}' was not the same as the argument typename '${at.inner.name}'",
                    cs
                  )

              val parseActual = G.getVariables.flatMap { variables =>
                variables.get(vn) match {
                  case None =>
                    System.err.println(
                      s"Internal error: variable '$$$vn' not found during parsing despite being in the type map."
                    )
                    typeNotFound
                  case Some(v) if v.tpe != tpe =>
                    System.err.println(
                      s"Internal error: variable '$$$vn' type mismatch during parsing despite being in the type map. Expected `${tpe}`, got `${v.tpe}`."
                    )
                    typeNotFound
                  case Some(v) =>
                    v.value match {
                      case Right(pval) => decodeIn(a, pval.map(c2 => c2 :: cs), ambigiousEnum = false)
                      case Left(j)     => decodeIn(a, V.fromJson(j).as(cs), ambigiousEnum = true)
                    }
                }
              }

              verifiedF >> verifiedTypenameF >> parseActual
          }
        }
      case (e @ Enum(name, _, _), v) =>
        val fa: G[(String, List[C])] = v match {
          case V.EnumValue(s, cs)                    => G.pure((s, cs))
          case V.StringValue(s, cs) if ambigiousEnum => G.pure((s, cs))
          case _                                     => G.raise(s"Enum value expected for `$name`, but got ${pValueName(v)}.", v.c)
        }

        fa.flatMap[A] { case (s, cs) =>
          e.m.lookup(s) match {
            case Some(x) => G.pure(x)
            case None =>
              val names = e.m.keys.toList
              G.raise(
                s"Enum value `$s` does not occur in enum type `$name`, possible enum values are ${names.map(s => s"`$s`").mkString_(", ")}.",
                cs
              )
          }
        }
      case (Scalar(name, _, decoder, _), x: NonVar[List[C]]) =>
        G.ambientField(name) {
          G.raiseEither(decoder(x.map(_ => ())), x.c)
        }
      case (Input(_, fields, _), V.ObjectValue(xs, cs)) => decodeArg(fields, xs, ambigiousEnum, cs)
      case (a: InArr[a, c], V.ListValue(vs, cs)) =>
        vs.zipWithIndex
          .parTraverse { case (v, i) =>
            G.ambientIndex(i) {
              decodeIn(a.of, v, ambigiousEnum)
            }
          }
          .flatMap[c](a.fromSeq(_).fold(G.raise(_, cs), G.pure(_)))
      case (_: InOpt[a], V.NullValue(_)) => G.pure(Option.empty[a])
      case (opt: InOpt[a], v)            => decodeIn(opt.of, v, ambigiousEnum).map(Option(_))
      case (i, v) => G.raise(s"Expected type `${ModifierStack.fromIn(i).show(_.name)}`, but got value ${pValueName(value)}.", v.c)
    }
  }

  def decodeArg[A](
      arg: Arg[A],
      values: List[(String, V[AnyValue, List[C]])],
      ambigiousEnum: Boolean,
      context: List[C]
  ): G[A] = {
    val duplicates = values
      .groupBy { case (k, _) => k }
      .collect { case (k, v) if v.size > 1 => k }
      .toList
    val duplicatesF: G[Unit] =
      if (duplicates.isEmpty) G.unit
      else G.raise(s"Duplicate argument names found: ${duplicates.map(x => s"'$x'").mkString_(", ")}.", context)

    duplicatesF >> G.defer {
      val lookup = values.toMap
      val expected = arg.entries.toList.map(_.name).toSet
      val provided = lookup.keySet

      val tooMuch = provided -- expected
      val tooMuchF: G[Unit] =
        if (tooMuch.isEmpty) G.unit
        else G.raise(s"Too many fields provided, unknown fields are ${tooMuch.toList.map(x => s"'$x'").mkString_(", ")}.", context)

      val fv = arg.impl.parFoldMap[G, ValidatedNec[String, A]](new (Arg.Impl ~> G) {
        def apply[B](fa: Arg.Impl[B]): G[B] = fa match {
          case fa: ArgDecoder[a, B] =>
            G.ambientField(fa.av.name) {
              def compileWith(x: V[AnyValue, List[C]], default: Boolean) =
                decodeIn[a](fa.av.input.value, x, ambigiousEnum)
                  .flatMap(a => G.raiseEither(fa.decode(ArgParam(default, a)), x.c))

              lookup
                .get(fa.av.name)
                .map(compileWith(_, false))
                .orElse(fa.av.defaultValue.map(dv => compileWith(dv.as(Nil), true)))
                .getOrElse {
                  fa.av.input.value match {
                    case _: gql.ast.InOpt[a] => G.raiseEither(fa.decode(ArgParam(true, None)), context)
                    case _ =>
                      G.raise(s"Missing argument for '${fa.av.name}' and no default value was found.", context)
                  }
                }
            }
        }
      })

      tooMuchF &> fv.flatMap(v => G.raiseEither(v.toEither.leftMap(_.mkString_(", ")), context))
    }
  }
}
