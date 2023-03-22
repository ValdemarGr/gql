package gql.preparation

import gql.parser.{QueryAst => QA, Value => V, AnyValue, Const}
import cats.data._
import io.circe._
import cats.mtl._
import cats._
import cats.implicits._
import gql.parser.QueryAst
import gql.parser.Pos
import gql.ast._
import gql.Arg
import gql.InverseModifierStack
import gql.ModifierStack
import gql.parser.NonVar
import gql.DecodedArgValue
import gql.ArgParam

trait ArgParsing[F[_]] {
  def decodeIn[A](
      a: In[A],
      value: V[AnyValue],
      ambigiousEnum: Boolean
  ): F[A]

  def decodeArg[A](
      arg: Arg[A],
      values: Map[String, V[AnyValue]],
      ambigiousEnum: Boolean
  ): F[A]
}

object ArgParsing {
  def apply[F[_]: Parallel](
      variables: VariableMap
  )(implicit
      F: Monad[F],
      P: PathAlg[F],
      E: ErrorAlg[F, ?],
      T: Tell[F, Set[String]]
  ): ArgParsing[F] = new ArgParsing[F] {
    import E._
    import P._

    override def decodeIn[A](a: In[A], value: V[AnyValue], ambigiousEnum: Boolean): F[A] = {
      (a, value) match {
        case (_, V.VariableValue(v)) =>
          T.tell(Set(v)) *> {
            variables.get(v) match {
              case None =>
                raise(
                  s"Variable '$$$v' was not declared and provided as a possible variable for this operation. Hint add the variable to the variables list of the operation '(..., $$$v: ${ModifierStack
                    .fromIn(a)
                    .show(_.name)})' and provide a value in the variables parameter.",
                  None
                )
              case Some(v) =>
                val parseInner: F[A] = v.value match {
                  case Right(pval) => decodeIn(a, pval, ambigiousEnum = false)
                  case Left(j)     => decodeIn(a, V.fromJson(j), ambigiousEnum = true)
                }
                // TODO verify variable here
                /*
              val gottenTypename = ModifierStack.fromType(v.tpe).inner
              val foundTypename =
                 */
                parseInner
            }
          }
        case (e @ Enum(name, _, _), v) =>
          val fa: F[String] = v match {
            case V.EnumValue(s)                    => F.pure(s)
            case V.StringValue(s) if ambigiousEnum => F.pure(s)
            case _                                 => raise(s"Enum value expected for `$name`, but got ${pValueName(v)}.", None)
          }

          fa.flatMap[A] { s =>
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
        case (Scalar(name, _, decoder, _), x: NonVar) =>
          ambientField(name) {
            raiseEither(decoder(x), None)
          }
        case (Input(_, fields, _), V.ObjectValue(xs)) => decodeArg(fields, xs.toMap, ambigiousEnum)
        case (a: InArr[a, c], V.ListValue(vs)) =>
          vs.zipWithIndex
            .parTraverse { case (v, i) =>
              ambientIndex(i) {
                decodeIn(a.of, v, ambigiousEnum)
              }
            }
            .flatMap[c](a.fromSeq(_).fold(raise(_, None), F.pure(_)))
        case (_: InOpt[a], V.NullValue()) => F.pure(Option.empty[a])
        case (opt: InOpt[a], v)           => decodeIn(opt.of, v, ambigiousEnum).map(Option(_))
        case (i, _) => raise(s"Expected type `${ModifierStack.fromIn(i).show(_.name)}`, but got value ${pValueName(value)}.", None)
      }
    }

    override def decodeArg[A](arg: Arg[A], values: Map[String, V[AnyValue]], ambigiousEnum: Boolean): F[A] = {
      val expected = arg.impl.enumerate.toList.map(_.av.name).toSet
      val provided = values.keySet

      val tooMuch = provided -- expected
      val tooMuchF: F[Unit] =
        if (tooMuch.isEmpty) F.unit
        else raise(s"Too many fields provided, unknown fields are ${tooMuch.toList.map(x => s"'$x'").mkString_(", ")}.", None)

      val fv = arg.impl.foldMap[F, ValidatedNec[String, A]](new (Arg.Impl ~> F) {
        def apply[A](fa: Arg.Impl[A]): F[A] = fa match {
          case fa: DecodedArgValue[a, A] =>
            ambientField(fa.av.name) {
              def compileWith(x: V[AnyValue], default: Boolean) =
                decodeIn[a](fa.av.input.value, x, ambigiousEnum)
                  .flatMap(a => raiseEither(fa.decode(ArgParam(default, a)), None))

              values
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

      tooMuchF &> fv.flatMap(v => raiseEither(v.toEither.leftMap(_.mkString_(", ")), None))
    }
  }
}
