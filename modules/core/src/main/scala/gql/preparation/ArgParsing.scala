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
import cats.mtl._
import gql._
import gql.ast._
import gql.parser.AnyValue
import gql.parser.NonVar
import gql.parser.{Value => V}

trait ArgParsing[F[_], C] {
  def decodeIn[A](
      a: In[A],
      value: V[AnyValue, List[C]],
      ambigiousEnum: Boolean
  ): F[A]

  def decodeArg[A](
      arg: Arg[A],
      values: Map[String, V[AnyValue, List[C]]],
      ambigiousEnum: Boolean,
      context: List[C]
  ): F[A]
}

object ArgParsing {
  type UsedVariables = Set[String]

  def apply[F[_]: Parallel, C](
      variables: VariableMap[C]
  )(implicit
      F: Monad[F],
      L: Local[F, Cursor],
      H: Handle[F, NonEmptyChain[PositionalError[C]]],
      T: Tell[F, UsedVariables]
  ): ArgParsing[F, C] = new ArgParsing[F, C] {
    val E = ErrorAlg.errorAlgForHandle[F, NonEmptyChain, C]
    val P = PathAlg[F]
    import E._
    import P._

    override def decodeIn[A](a: In[A], value: V[AnyValue, List[C]], ambigiousEnum: Boolean): F[A] = {
      (a, value) match {
        case (_, V.VariableValue(vn, cs)) =>
          T.tell(Set(vn)) *> {
            variables.get(vn) match {
              case None =>
                raise(
                  s"Variable '$$$vn' was not declared and provided as a possible variable for this operation. Hint add the variable to the variables list of the operation '(..., $$$vn: ${ModifierStack
                    .fromIn(a)
                    .show(_.name)})' and provide a value in the variables parameter.",
                  cs
                )
              case Some(v) =>
                val parseInnerF: F[A] = v.value match {
                  case Right(pval) => decodeIn(a, pval.map(c2 => c2 :: cs), ambigiousEnum = false)
                  case Left(j)     => decodeIn(a, V.fromJson(j).as(cs), ambigiousEnum = true)
                }

                val vt: ModifierStack[String] = ModifierStack.fromType(v.tpe)
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
                 * [a] compat [v] -> [a] compat [v]
                 * [a] compat  V  -> fail
                 *  A  compat [v] -> fail
                 */
                def verifyTypeShape(argShape: List[Modifier], varShape: List[Modifier]): F[Unit] =
                  (argShape, varShape) match {
                    // A compat V
                    case (Nil, Nil) => F.unit
                    // a! compat v! -> ok
                    case (Modifier.NonNull :: xs, Modifier.NonNull :: ys) => verifyTypeShape(xs, ys)
                    // a! compat ([v] | V) -> fail
                    case (Modifier.NonNull :: _, (Modifier.List :: _) | Nil) =>
                      raise(
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
                      raise(
                        s"${prefix}, because the argumented expected a list modifier ([A]) but no more modifiers were provided${remaining(varShape, argShape)}",
                        cs
                      )
                    // A compat [v] -> fail
                    case (Nil, Modifier.List :: _) =>
                      raise(
                        s"${prefix}, because the argumented expected no more modifiers but was given a list modifier ([A])${remaining(varShape, argShape)}",
                        cs
                      )
                  }

                val verifiedF: F[Unit] = verifyTypeShape(at.modifiers, vt.modifiers)

                val verifiedTypenameF: F[Unit] =
                  if (vt.inner === at.inner.name) F.unit
                  else
                    raise(
                      s"${prefix}, typename of the variable '${vt.inner}' was not the same as the argument typename '${at.inner.name}'",
                      cs
                    )

                verifiedF *> verifiedTypenameF *> parseInnerF
            }
          }
        case (e @ Enum(name, _, _), v) =>
          val fa: F[(String, List[C])] = v match {
            case V.EnumValue(s, cs)                    => F.pure((s, cs))
            case V.StringValue(s, cs) if ambigiousEnum => F.pure((s, cs))
            case _                                     => raise(s"Enum value expected for `$name`, but got ${pValueName(v)}.", v.c)
          }

          fa.flatMap[A] { case (s, cs) =>
            e.m.lookup(s) match {
              case Some(x) => F.pure(x)
              case None =>
                val names = e.m.keys.toList
                raise(
                  s"Enum value `$s` does not occur in enum type `$name`, possible enum values are ${names.map(s => s"`$s`").mkString_(", ")}.",
                  cs
                )
            }
          }
        case (Scalar(name, _, decoder, _), x: NonVar[List[C]]) =>
          ambientField(name) {
            raiseEither(decoder(x.map(_ => ())), x.c)
          }
        case (Input(_, fields, _), V.ObjectValue(xs, cs)) => decodeArg(fields, xs.toMap, ambigiousEnum, cs)
        case (a: InArr[a, c], V.ListValue(vs, cs)) =>
          vs.zipWithIndex
            .parTraverse { case (v, i) =>
              ambientIndex(i) {
                decodeIn(a.of, v, ambigiousEnum)
              }
            }
            .flatMap[c](a.fromSeq(_).fold(raise(_, cs), F.pure(_)))
        case (_: InOpt[a], V.NullValue(_)) => F.pure(Option.empty[a])
        case (opt: InOpt[a], v)            => decodeIn(opt.of, v, ambigiousEnum).map(Option(_))
        case (i, v) => raise(s"Expected type `${ModifierStack.fromIn(i).show(_.name)}`, but got value ${pValueName(value)}.", v.c)
      }
    }

    override def decodeArg[A](arg: Arg[A], values: Map[String, V[AnyValue, List[C]]], ambigiousEnum: Boolean, context: List[C]): F[A] = {
      val expected = arg.impl.enumerate.toList.map(_.av.name).toSet
      val provided = values.keySet

      val tooMuch = provided -- expected
      val tooMuchF: F[Unit] =
        if (tooMuch.isEmpty) F.unit
        else raise(s"Too many fields provided, unknown fields are ${tooMuch.toList.map(x => s"'$x'").mkString_(", ")}.", context)

      val fv = arg.impl.foldMap[F, ValidatedNec[String, A]](new (Arg.Impl ~> F) {
        def apply[A](fa: Arg.Impl[A]): F[A] = fa match {
          case fa: ArgDecoder[a, A] =>
            ambientField(fa.av.name) {
              def compileWith(x: V[AnyValue, List[C]], default: Boolean) =
                decodeIn[a](fa.av.input.value, x, ambigiousEnum)
                  .flatMap(a => raiseEither(fa.decode(ArgParam(default, a)), x.c))

              values
                .get(fa.av.name)
                .map(compileWith(_, false))
                .orElse(fa.av.defaultValue.map(dv => compileWith(dv.as(Nil), true)))
                .getOrElse {
                  fa.av.input.value match {
                    case _: gql.ast.InOpt[a] => raiseEither(fa.decode(ArgParam(true, None)), context)
                    case _ =>
                      raise(s"Missing argument for '${fa.av.name}' and no default value was found.", context)
                  }
                }
            }
        }
      })

      tooMuchF &> fv.flatMap(v => raiseEither(v.toEither.leftMap(_.mkString_(", ")), context))
    }
  }
}
