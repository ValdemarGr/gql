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
package gql.client.codegen

import gql.parser.TypeSystemAst._
import gql.parser.QueryAst._
import cats.data._
import cats._
import org.typelevel.paiges.Doc
import cats.implicits._
import gql._
import cats.mtl.Local
import cats.mtl.Tell
import cats.mtl.Stateful
import cats.parse.Caret
import gql.client.codegen.{RenderHelpers => R}
import GenAst._

final class GenAst[F[_]: Parallel](implicit
    P: CurrentPath[F],
    U: UsedInputTypes[F],
    F: MonadError[F, NonEmptyChain[String]]
) {
  def in[A](field: String)(fa: F[A]): F[A] =
    P.local(fa)(_ append field)

  def raise[A](msg: String) = P.ask.flatMap { path =>
    F.raiseError[A](NonEmptyChain.one(msg + s" at ${path.toList.mkString(".")}"))
  }

  def partitionEmittableInputDef[G[_]](env: Env, typename: String)(implicit G: Monad[G], U: UsedInputTypes[G]): G[Unit] =
    env.get(typename).traverse_ {
      case x: TypeDefinition.InputObjectTypeDefinition => U.tell(Set(Right(x)))
      case x: TypeDefinition.EnumTypeDefinition        => U.tell(Set(Left(x)))
      case _                                           => G.unit
    }

  def partitionEmittableInputType(env: Env, tpe: gql.parser.Type): F[Unit] =
    partitionEmittableInputDef[F](env, ModifierStack.fromType(tpe).inner)

  def generateField(
      companionName: String,
      env: Env,
      f: Field[Caret],
      fd: FieldDefinition
  ): F[(SelField, Option[TypeIntro])] = {
    val ms = ModifierStack.fromType(fd.tpe)
    val gqlName = f.alias.getOrElse(f.name)
    val n = R.toPascal(gqlName)

    val existing = fd.argumentsDefinition.map(iv => iv.name -> iv).toMap
    val provided = f.arguments.map(_.nel.toList).getOrElse(Nil).map(_.name).toSet
    val existingProvided = existing.view.filterKeys(provided.contains).toMap

    val putUsedInputsF = existingProvided.values.toList.traverse_(x => partitionEmittableInputType(env, x.tpe))

    partitionEmittableInputDef[F](env, ms.inner) &>
      putUsedInputsF &>
      f.selectionSet
        .map(_.selections)
        .traverse(generateTypeDef(env, n, ms.inner, _))
        .map { ti =>
          val ms2 = if (ti.isEmpty) ms else ms.map(_ => s"${companionName}.${n}")
          val sf = SelField(
            f.name,
            f.alias,
            f.arguments.map(_.nel.toList).getOrElse(Nil),
            ms2,
            f.directives.map(_.nel.toList).getOrElse(Nil)
          )

          (sf, ti)
        }
  }

  def generateTypeDef(
      env: Env,
      name: String,
      typename: String,
      sels: NonEmptyList[Selection[Caret]]
  ): F[TypeIntro] = env.get(typename) match {
    case None => raise[TypeIntro](s"Type `$typename` not found")
    case Some(td) =>
      val fieldMapF: F[Map[String, FieldDefinition]] = td match {
        case TypeDefinition.ObjectTypeDefinition(_, _, _, _, fds)    => F.pure(fds.toList.map(f => f.name -> f).toMap)
        case TypeDefinition.InterfaceTypeDefinition(_, _, _, _, fds) => F.pure(fds.toList.map(f => f.name -> f).toMap)
        case TypeDefinition.UnionTypeDefinition(_, _, _, _)          => F.pure(Map.empty[String, FieldDefinition])
        case _ =>
          raise[Map[String, FieldDefinition]](
            s"Type tried to perform selection on`$typename`, but it is not an object, interface or union"
          )
      }

      val tn = FieldDefinition(None, "__typename", Nil, gql.parser.Type.NonNull(gql.parser.Type.Named("String")), None)

      fieldMapF.flatMap { fm =>
        val fm2 = fm + ("__typename" -> tn)
        sels
          .parTraverse(generateSelection(td, name, env, fm2, _))
          .map { xs =>
            val sels = xs.map { case (x, _) => x }
            // If there are at-lease two spreads that are sub-types of the current type, e.g Fragment.on <: This
            // Then generate a sealed trait hierachy
            val subtypeSpreads = sels.toList
              .collect { case x: SelFragSpread => x }
              .mapFilter { fs =>
                if (env.concreteSubtypesOf(typename).contains(fs.condition) && typename =!= fs.condition) Some(fs)
                else None
              }
            val groupedSpreads: List[VariantCase] =
              subtypeSpreads.groupByNel(_.condition).toList.map { case (k, vs) =>
                VariantCase(k, vs.toList.map(v => R.toCaml(v.fragmentName) -> v.scalaType))
              }

            val variant = groupedSpreads.toNel.map(Variant(_))
            val sel = gql.client.codegen.Selection(name, sels.toList, variant)
            val tis = xs.toList.mapFilter { case (_, x) => x }
            TypeIntro(sel, tis)
          }
      }
  }

  def generateSelection(
      td: TypeDefinition,
      companionName: String,
      env: Env,
      fieldMap: Map[String, FieldDefinition],
      sel: Selection[Caret]
  ): F[(Sel, Option[TypeIntro])] = {
    sel match {
      case fs: Selection.FieldSelection[Caret] =>
        val f = fs.field
        fieldMap.get(f.name) match {
          case None => raise[(Sel, Option[TypeIntro])](s"Field '${f.name}' not found in type '$companionName'")
          case Some(x) =>
            in(f.name) {
              generateField(companionName, env, f, x).widen
            }
        }
      case frag: Selection.FragmentSpreadSelection[Caret] =>
        val fs = frag.fragmentSpread

        val optTn = s"${env.packageName}.${fs.fragmentName}"

        val f = env.fragmentInfos.get(fs.fragmentName)

        // If the this type is a sub-type (more specific) of the fragment type
        // That is, This <: Fragment.on
        // Then the fragment result will always be present
        val b = f.filter(fi => env.subtypesOf(fi.on).contains(td.name)).isDefined

        val ds = fs.directives.toList.flatMap(_.nel.toList)
        f match {
          case None => raise[(Sel, Option[TypeIntro])](s"Fragment '${fs.fragmentName}' not found")
          case Some(fi) =>
            F.pure {
              SelFragSpread(optTn, fs.fragmentName, fi.on, inl = false, b, ds) -> None
            }
        }
      case inlineFrag: Selection.InlineFragmentSelection[Caret] =>
        val ilf = inlineFrag.inlineFragment
        val ss = ilf.selectionSet.selections
        val cnd = ilf.typeCondition.get

        val name = s"Inline${cnd}"

        val fn = s"${companionName}.${name}"

        val req = env.subtypesOf(cnd).contains(td.name)

        val ds = ilf.directives.toList.flatMap(_.nel.toList)
        in(s"inline-fragment-${cnd}") {
          // We'd like to match every concrete subtype of the inline fragment's type condition (since typename in the result becomes concrete)
          generateTypeDef(env, name, cnd, ss).map { ti =>
            SelFragSpread(fn, cnd, cnd, inl = true, req, ds) -> Some(ti)
          }
        }
    }
  }

  def gatherFragmentInfos(
      xs: NonEmptyList[ExecutableDefinition[Caret]]
  ): List[FragmentInfo] =
    xs.collect { case ExecutableDefinition.Fragment(f, _) =>
      FragmentInfo(f.name, f.typeCnd)
    }

  def generateExecutableDefs(
      env: Env,
      query: NonEmptyList[ExecutableDefinition[Caret]]
  ) = {
    val bodyF: F[NonEmptyList[Toplevel]] = query.parTraverse {
      case ExecutableDefinition.Operation(o, _) =>
        o match {
          case OperationDefinition.Simple(_) =>
            raise[Toplevel]("Simple operations are not supported, please name all your operations")
          case d: OperationDefinition.Detailed[Caret] if d.name.isEmpty =>
            raise[Toplevel]("Anonymous operations are not supported, please name all your operations")
          case d: OperationDefinition.Detailed[Caret] =>
            in(s"operation-${d.name.get}") {
              val vds = d.variableDefinitions.toList.flatMap(_.nel.toList)
              val usedF = vds.traverse_(vd => partitionEmittableInputType(env, vd.tpe))

              val v = vds
                .map { x =>
                  VariableField(x.name, ModifierStack.fromType(x.tpe), x.defaultValue)
                }
                .toNel
                .map(VariableType(_))

              usedF *>
                generateTypeDef(
                  env,
                  d.name.get,
                  d.tpe.toString(),
                  d.selectionSet.selections
                ).map(ti => Operation(d.tpe, ti, v))
            }
        }
      case ExecutableDefinition.Fragment(f, _) =>
        in(s"fragment-${f.name}") {
          generateTypeDef(
            env,
            f.name,
            f.typeCnd,
            f.selectionSet.selections
          ).map(ToplevelFragment(_, f.name, f.typeCnd))
        }

    }

    bodyF.map { body =>
      val bodyDoc = Doc.intercalate(Doc.hardLine + Doc.hardLine, body.toList.flatMap(_.docs))

      Doc.intercalate(
        Doc.hardLine,
        List(
          Doc.text(s"package ${env.packageName}"),
          Doc.empty,
          R.imp("_root_.gql.client._"),
          R.imp("_root_.gql.client.dsl._"),
          R.imp("_root_.gql.parser.{Value => V, AnyValue, Const}"),
          R.imp("cats.implicits._")
        )
      ) + Doc.hardLine + Doc.hardLine + bodyDoc
    }
  }

  def generateEnumType(td: TypeDefinition.EnumTypeDefinition): Doc = {
    val st = Doc.text(s"sealed trait ${td.name}")

    val names = td.values.toList.map(_.name)

    val companionParts = List(
      Doc.intercalate(
        Doc.hardLine,
        names.map { e =>
          Doc.text("case object ") + Doc.text(e) + Doc.text(" extends ") + Doc.text(td.name)
        }
      )
    ) ++ List(
      Doc.text(s"implicit val circeDecoder: io.circe.Decoder[${td.name}] = io.circe.Decoder.decodeString.emap") +
        R.hardIntercalateBracket('{') {
          names.map { e =>
            Doc.text("case ") + R.quoted(e) + Doc.text(s" => Right($e)")
          } ++ List(Doc.text(s"""case x => Left(s"Unknown enum value for ${td.name}: $$x")"""))
        }('}')
    ) ++ List(
      Doc.text(s"implicit val circeEncoder: io.circe.Encoder[${td.name}] = io.circe.Encoder.encodeString.contramap[${td.name}]") +
        R.hardIntercalateBracket('{') {
          names.map { e =>
            Doc.text("case ") + Doc.text(e) + Doc.text(s" => ") + R.quoted(e)
          }
        }('}')
    ) ++ List(
      Doc.text(s"implicit val typenameInstance: Typename[${td.name}] = typename[${td.name}](") +
        R.quoted(td.name) + Doc.char(')')
    )

    val companion = R.obj(td.name, companionParts)

    st + Doc.hardLine + companion
  }

  def generateInputType(td: TypeDefinition.InputObjectTypeDefinition): InputType = {
    val fixedMods: NonEmptyList[InputField] = td.inputFields.map { f =>
      val ms = ModifierStack.fromType(f.tpe)

      InputField(f.name, ms, f.defaultValue.isDefined)
    }

    InputType(td.name, fixedMods)
  }

  def generateOneInput(ui: UsedInput): Doc =
    ui match {
      case Left(x) => generateEnumType(x)
      case Right(x) =>
        val i = generateInputType(x)
        i.cc.doc + Doc.hardLine + i.companion
    }

  def generateInputs(env: Env, uis: List[UsedInput], packageName: String): Doc = {
    def walkObject[G[_]](
        x: TypeDefinition.InputObjectTypeDefinition
    )(implicit G: Monad[G], S: Stateful[G, Map[String, UsedInput]]): G[Unit] =
      x.inputFields.toList.traverse_ { i =>
        S.get.flatMap { cache =>
          val typename = ModifierStack.fromType(i.tpe).inner
          if (cache.get(typename).isDefined) G.unit
          else {
            val outcome = partitionEmittableInputDef[Writer[Set[UsedInput], *]](env, typename).written
            val insertF = outcome.toList.traverse_(x => S.modify(_ + (typename -> x)))

            insertF *> outcome.toList.traverse_ {
              case Right(io) => walkObject[G](io)
              case _         => G.unit
            }
          }
        }
      }

    val ios = uis.collect { case Right(x) => x }
    val m = ios
      .traverse_(walkObject[StateT[Eval, Map[String, UsedInput], *]](_))
      .runS(uis.map(io => io.bimap(_.name, _.name).merge -> io).toMap)
      .value

    Doc.intercalate(
      Doc.hardLine + Doc.hardLine,
      List(
        Doc.text(s"package $packageName"),
        Doc.intercalate(
          Doc.hardLine,
          List(
            R.imp("_root_.gql.client._"),
            R.imp("_root_.gql.client.dsl._")
          )
        )
      ) ++ m.values.toList.map(generateOneInput)
    )
  }
}

object GenAst {
  type UsedInput = Either[TypeDefinition.EnumTypeDefinition, TypeDefinition.InputObjectTypeDefinition]
  type UsedInputTypes[F[_]] = Tell[F, Set[UsedInput]]
  type CurrentPath[F[_]] = Local[F, Chain[String]]
  type Stack[A] = WriterT[EitherT[Kleisli[Eval, Chain[String], *], NonEmptyChain[String], *], Set[UsedInput], A]

  def make: GenAst[Stack] = new GenAst[Stack]

  final case class FragmentInfo(
      name: String,
      on: String
  )

  final case class Env(
      schema: Map[String, TypeDefinition],
      fragmentInfos: Map[String, FragmentInfo],
      packageName: String
  ) {
    def get(name: String): Option[TypeDefinition] = schema.get(name)

    // A -> [B <: A]
    val subtypeRelations: Map[String, Set[String]] = schema.values.toList
      .collect {
        case td: TypeDefinition.ObjectTypeDefinition =>
          // this, impl1, impl2 -> this
          (td.name :: td.interfaces) tupleRight td.name
        case td: TypeDefinition.UnionTypeDefinition =>
          // this -> this, impl1, impl2
          (td.name :: td.types.toList) tupleLeft td.name
        case td: TypeDefinition.InterfaceTypeDefinition =>
          // this, impl1, impl2 -> this
          (td.name :: td.interfaces) tupleRight td.name
      }
      .flatten
      .groupMap { case (abs, _) => abs } { case (_, conc) => conc }
      .fmap(_.toSet)

    def subtypesOf(abs: String): Set[String] = subtypeRelations.getOrElse(abs, Set.empty)

    def concreteSubtypesOf(abs: String): Set[String] =
      subtypesOf(abs).filter(schema(_) match {
        case _: TypeDefinition.ObjectTypeDefinition => true
        case _                                      => false
      })
  }
}
