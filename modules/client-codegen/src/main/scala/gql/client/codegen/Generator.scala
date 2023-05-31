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
package gql.client.codegen

import cats.effect._
import fs2.io.file._
import gql.parser.TypeSystemAst._
import gql.parser.QueryAst._
import gql.parser.{Value => V, AnyValue}
import cats.data._
import cats._
import org.typelevel.paiges.Doc
import cats.implicits._
import gql._
import cats.mtl.Local
import cats.mtl.Tell
import cats.mtl.Handle
import cats.mtl.Stateful
import cats.parse.Caret
import gql.parser.QueryAst
import gql.client.QueryValidation
import io.circe.Json
import gql.preparation.RootPreparation
import gql.parser.ParserUtil
import gql.util.SchemaUtil

object Generator {
  def modifyHead(f: Char => Char): String => String = { str =>
    val hd = str.headOption.map(f(_).toString()).getOrElse("")
    hd + str.drop(1)
  }

  val toCaml: String => String =
    modifyHead(_.toLower)

  val toPascal: String => String =
    modifyHead(_.toUpper)

  def scalaField(name: String, tpe: String): Doc =
    Doc.text(name) + Doc.char(':') + Doc.space + Doc.text(tpe)

  def hardIntercalate(left: Doc, right: Doc, xs: List[Doc], sep: Doc = Doc.empty) = {
    left +
      (Doc.hardLine + Doc.intercalate(sep + Doc.hardLine, xs)).nested(2).grouped +
      right
  }

  def hardIntercalateBracket(left: Char, sep: Doc = Doc.empty)(xs: List[Doc])(right: Char) =
    hardIntercalate(Doc.char(left), Doc.hardLine + Doc.char(right), xs, sep)

  def quoted(doc: Doc): Doc =
    Doc.char('"') + doc + Doc.char('"')

  def quoted(str: String): Doc = quoted(Doc.text(str))

  def params(xs: List[Doc]): Doc =
    Doc.intercalate(Doc.comma + Doc.space, xs).tightBracketBy(Doc.char('('), Doc.char(')'))

  def caseClass(name: String, xs: List[Doc], methods: List[Doc]): Doc =
    Doc.text(s"final case class $name") +
      hardIntercalateBracket('(', Doc.comma)(xs)(')') + methods.toNel
        .map { ms =>
          val d = Doc.hardLine + Doc.intercalate(Doc.hardLine + Doc.hardLine, ms.toList)

          Doc.text(" {") +
            d.grouped.nested(2) +
            Doc.hardLine + Doc.char('}')
        }
        .getOrElse(Doc.empty)

  def verticalApply(name: String, params: List[Doc]): Doc =
    Doc.text(name) + hardIntercalateBracket('(', Doc.comma)(params)(')')

  case class CaseClassField(
      name: String,
      tpe: InverseModifierStack[String],
      default: Option[V[AnyValue, Caret]]
  ) {
    val isOmittable = default.isDefined || tpe.modifiers.headOption.contains(InverseModifier.Optional)

    val doc: Doc =
      Doc.text(name) + Doc.char(':') + Doc.space + Doc.text(optUnless(!isOmittable)(tpe.showScala(identity))) +
        (if (isOmittable) Doc.text(" = None") else Doc.empty)

    def circeEncoderFieldOpt(objectName: String) = {
      val idx = s"${objectName}.${name}"
      quoted(name) + Doc.text(" -> ") + (
        if (isOmittable) Doc.text(s"${idx}.map(_.asJson)")
        else Doc.text(s"Some(${idx}.asJson)")
      )
    }

    lazy val variableDecl: Doc = {
      val func = if (isOmittable) "omittableVariable" else "variable"
      val ps = quoted(name) :: default.toList.map(generateValue(_, anyValue = false))
      Doc.text(s"${func}[${tpe.showScala(identity)}]") + params(ps)
    }
  }

  case class CaseClass(
      name: String,
      fields: List[CaseClassField]
  ) {
    val doc: Doc = {
      val helpers = fields.filter(_.isOmittable).map { f =>
        Doc.text(s"def set${toPascal(f.name)}(value: ${f.tpe.showScala(identity)}): ${name} = copy(${f.name} = Some(value))")
      }

      caseClass(name, fields.map(_.doc), helpers)
    }

    lazy val circeEncoder = {
      val decl = Doc.text(s"implicit lazy val circeEncoder: io.circe.Encoder.AsObject[${name}]")
      val inst = Doc.text(s"io.circe.Encoder.AsObject.instance[${name}]")
      val encBody = List(
        imp("io.circe.syntax._"),
        verticalApply(
          "Map",
          fields.map(_.circeEncoderFieldOpt("a"))
        ) + Doc.text(".collect{ case (k, Some(v)) => k -> v }.asJsonObject")
      )
      val func = hardIntercalate(
        Doc.text("{ a => "),
        Doc.hardLine + Doc.char('}'),
        List(Doc.intercalate(Doc.hardLine, encBody))
      )

      decl + Doc.text(" = ") + inst + func
    }

    lazy val typenameInstance =
      Doc.text(s"implicit val typenameInstance: Typename[${name}] = typename[${name}](") +
        quoted(name) + Doc.char(')')
  }

  def obj[G[_]: Foldable](name: String, body: G[Doc]): Doc =
    Doc.text("object") + Doc.space + Doc.text(name) + Doc.space +
      hardIntercalateBracket('{', Doc.hardLine)(body.toList)('}')

  def optUnless(cond: Boolean)(tpe: String): String =
    if (cond) tpe else s"Option[${tpe}]"

  final case class QVar(
      varDecl: Doc,
      name: String,
      scalaType: String,
      isOption: Boolean
  )
  sealed trait ContextInfo
  object ContextInfo {
    final case class Fragment(
        fragmentName: Option[String],
        typeCnd: String
    ) extends ContextInfo
    final case class Operation(
        op: OperationType,
        variables: List[VariableDefinition[Caret]]
    ) extends ContextInfo
  }
  final case class Part(
      name: String,
      typePart: NonEmptyList[Doc],
      typeMethods: List[Doc],
      subParts: List[Part],
      codec: NonEmptyList[Doc],
      contextInfo: Option[ContextInfo],
      companionExtra: List[Doc]
  ) {
    // Yes yes, it is a fold
    def collapse: Doc = {
      val tpe = caseClass(name, typePart.toList, typeMethods)

      val codecSelection = hardIntercalateBracket('(', Doc.comma)(codec.toList)(')') +
        Doc.char('.') +
        (if (codec.size > 1) Doc.text("mapN") else Doc.text("map")) +
        Doc.text("(apply)")

      val tpeName = contextInfo match {
        case Some(_: ContextInfo.Fragment) => s"Option[$name]"
        case _                             => name
      }

      def codecImplicit: Doc =
        Doc.text(s"implicit val selectionSet: SelectionSet[$tpeName] = ")

      val fullCodec: Doc = contextInfo match {
        case None => codecImplicit + codecSelection
        case Some(fi: ContextInfo.Fragment) =>
          val args = fi.fragmentName.toList ++ List(fi.typeCnd)

          val fragType = fi.fragmentName.as("fragment").getOrElse("inlineFrag")

          val invocation = Doc.text(fragType) + params(args.map(quoted))

          codecImplicit +
            hardIntercalate(
              Doc.empty,
              Doc.empty,
              List(invocation + Doc.space + hardIntercalateBracket('{')(List(codecSelection))('}'))
            )
        case Some(op: ContextInfo.Operation) =>
          val operationType = op.op match {
            case OperationType.Query        => "Query"
            case OperationType.Mutation     => "Mutation"
            case OperationType.Subscription => "Subscription"
          }

          val operationTypePath =
            s"_root_.gql.parser.QueryAst.OperationType.${operationType}"

          val (queryPrefix, expr, auxilaryType) = op.variables.toNel match {
            case None => ("named", codecSelection, None)
            case Some(zs) =>
              val vars = zs.map { v =>
                val ims = ModifierStack.fromType(v.tpe).invert
                CaseClassField(v.name, ims, v.defaultValue)
              }

              val decls = vars.toList.map(_.variableDecl)

              val (contraPart, contraType) = if (vars.size > 1) {
                val tupleApply = ("(" * vars.size) + vars.map(_.name).toList.mkString("), ") + ")"

                val unapplyPart = verticalApply("Variables", vars.toList.map(x => Doc.text(x.name)))

                val contra = Doc.text(s".contramap[Variables]") +
                  hardIntercalate(
                    Doc.text(s"{ case ") + unapplyPart + Doc.text(" =>"),
                    Doc.hardLine + Doc.char('}'),
                    List(Doc.text(tupleApply))
                  )

                (contra, Some(CaseClass("Variables", vars.toList).doc))
              } else {
                (Doc.empty, None)
              }

              val e = hardIntercalateBracket('(', Doc.text(" ~"))(decls)(')') +
                contraPart +
                Doc.text(".introduce ") +
                hardIntercalate(Doc.text("{ _ =>"), Doc.hardLine + Doc.char('}'), List(codecSelection))

              ("parameterized", e, contraType)
          }

          val queryExpr: Doc = Doc.text("val queryExpr = ") + expr

          val args: List[Doc] = List(
            Doc.text(operationTypePath),
            quoted(name),
            Doc.text("queryExpr")
          )

          val compiled: Doc = Doc.text("val query = ") +
            Doc.text("_root_.gql.client.Query.") + Doc.text(queryPrefix) + hardIntercalateBracket('(', Doc.comma)(args)(')')

          auxilaryType.map(_ + Doc.hardLine + Doc.hardLine).getOrElse(Doc.empty) +
            queryExpr +
            Doc.hardLine +
            Doc.hardLine +
            compiled
      }

      val companionParts = subParts.map(_.collapse) ++ companionExtra ++ List(fullCodec)

      val companion = Doc.text("object") + Doc.space + Doc.text(name) + Doc.space +
        hardIntercalateBracket('{', Doc.hardLine)(companionParts)('}')

      Doc.intercalate(Doc.hardLine + Doc.hardLine, List(tpe.grouped, companion.grouped))
    }
  }

  def generateValue[C](v: V[AnyValue, C], anyValue: Boolean): Doc = {
    import V._
    val tpe = if (anyValue) "AnyValue" else "Const"
    v match {
      case IntValue(v, _)     => Doc.text(s"V.IntValue(${v.toString()})")
      case StringValue(v, _)  => Doc.text(s"""V.StringValue("$v")""")
      case FloatValue(v, _)   => Doc.text(s"""V.FloatValue("$v")""")
      case NullValue(_)       => Doc.text(s"""V.NullValue()""")
      case BooleanValue(v, _) => Doc.text(s"""V.BooleanValue(${v.toString()})""")
      case ListValue(v, _) =>
        Doc.text(s"V.ListValue[${tpe}, Unit](") +
          Doc
            .intercalate(Doc.comma + Doc.line, v.map(generateValue(_, anyValue)))
            .tightBracketBy(Doc.text("List("), Doc.char(')')) +
          Doc.text(")")
      case ObjectValue(fields, _) =>
        Doc.text(s"V.ObjectValue[${tpe}, Unit](") +
          Doc
            .intercalate(
              Doc.comma + Doc.line,
              fields.map { case (k, v) => quoted(k) + Doc.text(" -> ") + generateValue(v, anyValue) }
            )
            .bracketBy(Doc.text("List("), Doc.char(')')) +
          Doc.text(")")
      case EnumValue(v, _)     => Doc.text(s"""V.EnumValue("$v")""")
      case VariableValue(v, _) => Doc.text(s"""V.VariableValue("$v")""")
    }
  }

  type UsedInput = Either[TypeDefinition.EnumTypeDefinition, TypeDefinition.InputObjectTypeDefinition]
  type UsedInputTypes[F[_]] = Tell[F, Set[UsedInput]]
  type CurrentPath[F[_]] = Local[F, Chain[String]]

  def in[F[_], A](field: String)(fa: F[A])(implicit P: CurrentPath[F]): F[A] =
    P.local(fa)(_ append field)

  def raise[F[_], A](msg: String)(implicit
      F: MonadError[F, NonEmptyChain[String]],
      P: CurrentPath[F]
  ) =
    P.ask.flatMap { path =>
      F.raiseError[A](NonEmptyChain.one(msg + s" at ${path.toList.mkString(".")}"))
    }

  def partitionEmittableInputDef[F[_]](env: Env, typename: String)(implicit F: Monad[F], U: UsedInputTypes[F]): F[Unit] =
    env.get(typename).traverse_ {
      case x: TypeDefinition.InputObjectTypeDefinition => U.tell(Set(Right(x)))
      case x: TypeDefinition.EnumTypeDefinition        => U.tell(Set(Left(x)))
      case _                                           => F.unit
    }

  def partitionEmittableInputType[F[_]](env: Env, tpe: gql.parser.Type)(implicit F: Monad[F], U: UsedInputTypes[F]): F[Unit] =
    partitionEmittableInputDef[F](env, ModifierStack.fromType(tpe).inner)

  final case class FieldPart(
      name: String,
      scalaType: String,
      localType: String,
      // local case class and companion object, not present if the field is a fragment or terminal
      subPart: Option[Part],
      codec: Doc,
      spreadCondition: Option[String]
  ) {
    lazy val typePart = scalaField(name, scalaType)
  }
  def generateField[F[_]: Parallel](
      companionName: String,
      env: Env,
      f: Field[Caret],
      fd: FieldDefinition
  )(implicit
      P: CurrentPath[F],
      U: UsedInputTypes[F],
      F: MonadError[F, NonEmptyChain[String]]
  ): F[FieldPart] = {
    val ms = ModifierStack.fromType(fd.tpe)
    val gqlName = f.alias.getOrElse(f.name)
    val n = toPascal(gqlName)

    val existsing = fd.argumentsDefinition.map(iv => iv.name -> iv).toMap
    val provided = f.arguments.map(_.nel.toList).getOrElse(Nil).map(_.name).toSet
    val existingProvided = existsing.view.filterKeys(provided.contains).toMap

    val putUsedInputsF = existingProvided.values.toList.traverse_(x => partitionEmittableInputType[F](env, x.tpe))

    putUsedInputsF &>
      f.selectionSet
        .map(_.selections)
        .parTraverse(generateTypeDef[F](env, n, ms.inner, _, None))
        .map { subPart =>
          val argPart = f.arguments.toList.flatMap(_.nel.toList).map { x =>
            Doc.text("arg") +
              (
                quoted(x.name) + Doc.char(',') + Doc.space + generateValue(x.value, anyValue = true)
              ).tightBracketBy(Doc.char('('), Doc.char(')'))
          }

          val scalaTypeName = ms.invert
            .copy(inner =
              if (subPart.isDefined) s"${companionName}.${n}"
              else ms.inner
            )
            .showScala(identity)

          val clientSel = Doc.text("sel") +
            Doc.text(scalaTypeName).tightBracketBy(Doc.char('['), Doc.char(']')) +
            params(quoted(fd.name) :: f.alias.toList.map(quoted) ++ argPart)

          FieldPart(gqlName, scalaTypeName, n, subPart, clientSel, None)
        }
  }

  def generateSelection[F[_]: Parallel](
      td: TypeDefinition,
      companionName: String,
      env: Env,
      fieldMap: Map[String, FieldDefinition],
      sel: Selection[Caret]
  )(implicit
      P: CurrentPath[F],
      U: UsedInputTypes[F],
      F: MonadError[F, NonEmptyChain[String]]
  ): F[FieldPart] = {
    sel match {
      case fs: Selection.FieldSelection[Caret] =>
        val f = fs.field
        fieldMap.get(f.name) match {
          case None => raise[F, FieldPart](s"Field '${f.name}' not found in type '$companionName'")
          case Some(x) =>
            in(f.name) {
              generateField[F](companionName, env, f, x)
            }
        }
      case frag: Selection.FragmentSpreadSelection[Caret] =>
        val fs = frag.fragmentSpread

        val f = env.fragmentInfos.get(fs.fragmentName)

        val optTn = s"Option[${fs.fragmentName}]"

        // If the this type is a sub-type (more specific) of the fragment type
        // That is, This <: Fragment.on
        // Then the fragment result will always be present
        val (tn, suf) = f.filter(fi => env.subtypesOf(fi.on).contains(td.name)) match {
          case Some(x) =>
            (
              fs.fragmentName,
              Doc.text(".requiredFragment") + params(quoted(x.name) :: quoted(x.on) :: Nil)
            )
          case _ => (optTn, Doc.empty)
        }

        F.pure {
          FieldPart(
            toCaml(fs.fragmentName),
            tn,
            fs.fragmentName,
            None,
            Doc.text(s"embed[${optTn}]") + suf,
            f.map(_.on)
          )
        }
      case inlineFrag: Selection.InlineFragmentSelection[Caret] =>
        val ilf = inlineFrag.inlineFragment
        val ss = ilf.selectionSet.selections
        val cnd = ilf.typeCondition.get

        val name = s"Inline${cnd}"

        val fn = s"${companionName}.${name}"

        val (tn, suf) =
          if (env.subtypesOf(cnd).contains(td.name))
            (fn, Doc.text(".requiredFragment") + params(quoted(name) :: quoted(cnd) :: Nil))
          else
            (s"Option[$fn]", Doc.empty)

        in(s"inline-fragment-${cnd}") {
          // We'd like to match every concrete subtype of the inline fragment's type condition (since typename in the result becomes concrete)
          generateTypeDef[F](env, name, cnd, ss, Some(ContextInfo.Fragment(None, cnd))).map { p =>
            FieldPart(
              toCaml(name),
              tn,
              name,
              Some(p),
              Doc.text(s"embed[Option[${name}]]") + suf,
              Some(cnd)
            )
          }
        }
    }
  }

  def generateTypeDef[F[_]: Parallel](
      env: Env,
      name: String,
      typename: String,
      sels: NonEmptyList[Selection[Caret]],
      contextInfo: Option[ContextInfo]
  )(implicit
      P: CurrentPath[F],
      U: UsedInputTypes[F],
      F: MonadError[F, NonEmptyChain[String]]
  ): F[Part] = {
    env.get(typename) match {
      case None => raise[F, Part](s"Type `$typename` not found")
      case Some(td) =>
        in(s"type-definition-${typename}") {
          val fieldMapF: F[Map[String, FieldDefinition]] = td match {
            case TypeDefinition.ObjectTypeDefinition(_, _, _, _, fds)    => F.pure(fds.toList.map(f => f.name -> f).toMap)
            case TypeDefinition.InterfaceTypeDefinition(_, _, _, _, fds) => F.pure(fds.toList.map(f => f.name -> f).toMap)
            case TypeDefinition.UnionTypeDefinition(_, _, _, _)          => F.pure(Map.empty[String, FieldDefinition])
            case _ =>
              raise[F, Map[String, FieldDefinition]](
                s"Type tried to perform selection on`$typename`, but it is not an object, interface or union"
              )
          }

          val tn = FieldDefinition(None, "__typename", Nil, gql.parser.Type.NonNull(gql.parser.Type.Named("String")), None)

          fieldMapF.flatMap { fm =>
            sels
              .parTraverse(generateSelection[F](td, name, env, fm + ("__typename" -> tn), _))
              .map { parts =>
                // If there are at-lease two spreads that are sub-types of the current type, e.g Fragment.on <: This
                // Then generate a sealed trait hierachy
                val subtypeSpreads = parts.toList.mapFilter { p =>
                  p.spreadCondition.flatMap { sc =>
                    if (env.concreteSubtypesOf(typename).contains(sc)) Some((p, sc))
                    else None
                  }
                }
                val groupedSpreads: List[(String, NonEmptyList[FieldPart])] =
                  subtypeSpreads.groupByNel { case (_, sc) => sc }.fmap(_.map { case (f, _) => f }).toList

                val (variantMethods, variantCompanion) = if (groupedSpreads.size > 1) {
                  val cases = Doc.intercalate(
                    Doc.text(" orElse") + Doc.hardLine,
                    groupedSpreads.map { case (sc, fps) =>
                      val (args, op) = fps match {
                        case NonEmptyList(x, Nil) => (Doc.text(x.name), Doc.text("map"))
                        case _                    => (params(fps.toList.map(fp => Doc.text(fp.name))), Doc.text("mapN"))
                      }
                      params(
                        List(
                          args + Doc.char('.') + op +
                            params(
                              List(
                                Doc.text(s"${name}.Variant.${sc}") +
                                  params(fps.toList.map(_ => Doc.text("_")))
                              )
                            )
                        )
                      )
                    }
                  )

                  val variants = Doc.intercalate(
                    Doc.hardLine,
                    groupedSpreads.map { case (sc, fps) =>
                      val cc = caseClass(
                        sc,
                        fps.toList.map(fp => scalaField(fp.name, fp.localType)),
                        List.empty
                      )
                      cc + Doc.text(" extends Variant")
                    }
                  )

                  val companionPart: Doc =
                    Doc.text(s"sealed trait Variant extends Product with Serializable") + Doc.hardLine +
                      obj(
                        "Variant",
                        List(variants)
                      )

                  val methods = Doc.text(s"lazy val variant: Option[${name}.Variant] =") +
                    (Doc.hardLine +
                      cases.grouped.nested(2)).nested(2)

                  (List(methods), List(companionPart))
                } else {
                  (Nil, Nil)
                }

                Part(
                  name,
                  parts.map(_.typePart),
                  variantMethods,
                  parts.toList.flatMap(_.subPart.toList),
                  parts.map(_.codec),
                  contextInfo,
                  variantCompanion
                )
              }
          }
        }
    }
  }

  def imp(t: String) = Doc.text(s"import $t")

  final case class FragmentInfo(
      name: String,
      on: String
  )

  def gatherFragmentInfos(
      xs: NonEmptyList[ExecutableDefinition[Caret]]
  ): List[FragmentInfo] =
    xs.collect { case ExecutableDefinition.Fragment(f, _) =>
      FragmentInfo(f.name, f.typeCnd)
    }

  def generateExecutableDefs[F[_]: Parallel](
      env: Env,
      query: NonEmptyList[ExecutableDefinition[Caret]]
  )(implicit
      P: CurrentPath[F],
      U: UsedInputTypes[F],
      F: MonadError[F, NonEmptyChain[String]]
  ) = {
    val bodyF: F[NonEmptyList[Part]] = query.parTraverse {
      case ExecutableDefinition.Operation(o, _) =>
        o match {
          case OperationDefinition.Simple(_) =>
            raise[F, Part]("Simple operations are not supported, please name all your operations")
          case d: OperationDefinition.Detailed[Caret] =>
            in(s"operation-${d.name.get}") {
              val vds = d.variableDefinitions.toList.flatMap(_.nel.toList)
              val usedF = vds.traverse_(vd => partitionEmittableInputType[F](env, vd.tpe))

              usedF *>
                generateTypeDef[F](
                  env,
                  d.name.get,
                  d.tpe.toString(),
                  d.selectionSet.selections,
                  Some(
                    ContextInfo.Operation(
                      d.tpe,
                      d.variableDefinitions.toList.flatMap(_.nel.toList)
                    )
                  )
                )
            }
        }
      case ExecutableDefinition.Fragment(f, _) =>
        in(s"fragment-${f.name}") {
          generateTypeDef[F](
            env,
            f.name,
            f.typeCnd,
            f.selectionSet.selections,
            Some(ContextInfo.Fragment(Some(f.name), f.typeCnd))
          )
        }

    }

    bodyF.map { body =>
      val bodyDoc = Doc.intercalate(Doc.hardLine + Doc.hardLine, body.toList.map(_.collapse))

      Doc.intercalate(
        Doc.hardLine,
        List(
          Doc.text("package gql.client.generated"),
          Doc.empty,
          imp("_root_.gql.client._"),
          imp("_root_.gql.client.dsl._"),
          imp("_root_.gql.parser.{Value => V, AnyValue, Const}"),
          imp("cats.implicits._")
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
        hardIntercalateBracket('{') {
          names.map { e =>
            Doc.text("case ") + quoted(e) + Doc.text(s" => Right($e)")
          } ++ List(Doc.text(s"""case x => Left(s"Unknown enum value for ${td.name}: $$x")"""))
        }('}')
    ) ++ List(
      Doc.text(s"implicit val circeEncoder: io.circe.Encoder[${td.name}] = io.circe.Encoder.encodeString.contramap[${td.name}]") +
        hardIntercalateBracket('{') {
          names.map { e =>
            Doc.text("case ") + Doc.text(e) + Doc.text(s" => ") + quoted(e)
          }
        }('}')
    ) ++ List(
      Doc.text(s"implicit val typenameInstance: Typename[${td.name}] = typename[${td.name}](") +
        quoted(td.name) + Doc.char(')')
    )

    val companion = obj(td.name, companionParts)

    st + Doc.hardLine + companion
  }

  def generateInputType(td: TypeDefinition.InputObjectTypeDefinition): Doc = {
    val fixedMods: List[CaseClassField] = td.inputFields.toList.map { f =>
      val ms = ModifierStack.fromType(f.tpe).invert

      CaseClassField(f.name, ms, f.defaultValue)
    }

    val cc = CaseClass(td.name, fixedMods)

    val companion = obj(td.name, List(cc.circeEncoder, cc.typenameInstance))

    cc.doc + Doc.hardLine + companion
  }

  def generateOneInput(ui: UsedInput): Doc =
    ui match {
      case Left(x)  => generateEnumType(x)
      case Right(x) => generateInputType(x)
    }

  def generateInputs(env: Env, uis: List[UsedInput]): Doc = {
    def walkObject[F[_]](
        x: TypeDefinition.InputObjectTypeDefinition
    )(implicit F: Monad[F], S: Stateful[F, Map[String, UsedInput]]): F[Unit] =
      x.inputFields.toList.traverse_ { i =>
        S.get.flatMap { cache =>
          val typename = ModifierStack.fromType(i.tpe).inner
          if (cache.get(typename).isDefined) F.unit
          else {
            val outcome = partitionEmittableInputDef[Writer[Set[UsedInput], *]](env, typename).written
            val insertF = outcome.toList.traverse_(x => S.modify(_ + (typename -> x)))

            insertF *> outcome.toList.traverse_ {
              case Right(io) => walkObject[F](io)
              case _         => F.unit
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
        Doc.text("package gql.client.generated"),
        Doc.intercalate(
          Doc.hardLine,
          List(
            imp("_root_.gql.client._"),
            imp("_root_.gql.client.dsl._")
          )
        )
      ) ++ m.values.toList.map(generateOneInput)
    )
  }

  final case class Env(
      schema: Map[String, TypeDefinition],
      fragmentInfos: Map[String, FragmentInfo]
  ) {
    def get(name: String): Option[TypeDefinition] = schema.get(name)

    // A -> [B <: A]
    val subtypeRelations: Map[String, Set[String]] = schema.values.toList
      .collect {
        case td: TypeDefinition.ObjectTypeDefinition =>
          // this, impl1, impl2 -> this
          (td.name :: td.interfaces) tupleRight td.name
        case td: TypeDefinition.UnionTypeDefinition =>
          // this -> this
          List(td.name -> td.name)
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

  type Err[F[_]] = Handle[F, NonEmptyChain[String]]

  def generateFor[F[_]](
      env: Env,
      query: NonEmptyList[ExecutableDefinition[Caret]]
  )(implicit E: Err[F], F: Applicative[F]): F[(Set[UsedInput], Doc)] = {
    type F[A] = WriterT[EitherT[Kleisli[Eval, Chain[String], *], NonEmptyChain[String], *], Set[UsedInput], A]
    generateExecutableDefs[F](env, query).run.value.run(Chain.empty).value.fold(E.raise, F.pure)
  }

  def getSchemaFrom(s: String): Either[String, Map[String, TypeDefinition]] =
    gql.parser.parseSchema(s).leftMap(_.prettyError.value).map(_.map(td => td.name -> td).toList.toMap)

  final case class Input(
      query: Path,
      output: Path
  )
  final case class Output(
      path: Path,
      doc: Doc
  )

  def readInputData[F[_]](i: Input)(implicit
      E: Err[F],
      F: Async[F]
  ): F[(String, NonEmptyList[ExecutableDefinition[Caret]])] =
    Files[F]
      .readAll(i.query)
      .through(fs2.text.utf8.decode[F])
      .compile
      .foldMonoid
      .flatMap { x =>
        val fa: F[NonEmptyList[ExecutableDefinition[Caret]]] =
          gql.parser.parseQuery(x).leftFlatMap(_.prettyError.value.leftNec).fold(E.raise, F.pure)

        fa tupleLeft x
      }

  def generateForInput[F[_]: Async: Err](
      env: Env,
      i: Input
  ): F[(String, Set[UsedInput], Output, NonEmptyList[ExecutableDefinition[Caret]])] =
    readInputData[F](i)
      .flatMap { case (q, eds) => generateFor(env, eds) tupleLeft ((q, eds)) }
      .map { case ((q, eds), (usedInputs, doc)) => (q, usedInputs, Output(i.output, doc), eds) }

  def gatherFragInfo[F[_]: Async: Err](i: Input): F[List[FragmentInfo]] =
    readInputData[F](i).map { case (_, eds) => gatherFragmentInfos(eds) }

  def writeStream[F[_]: Async](path: Path, doc: Doc) =
    fs2.Stream
      .iterable(doc.renderStream(80))
      .lift[F]
      .through(fs2.text.utf8.encode)
      .through(Files[F].writeAll(path))

  def readSchema[F[_]](schemaPath: Path)(implicit E: Err[F], F: Async[F]): F[Map[String, TypeDefinition]] =
    Files[F]
      .readAll(schemaPath)
      .through(fs2.text.utf8.decode[F])
      .compile
      .foldMonoid
      .flatMap(s => getSchemaFrom(s).fold(s => E.raise(NonEmptyChain.one(s)), F.pure))

  def readEnv[F[_]: Async: Err](schema: Path)(data: List[Input]): F[Env] =
    readSchema[F](schema).flatMap { m =>
      data
        .flatTraverse(gatherFragInfo[F])
        .map(f => Env(m, f.map(fi => fi.name -> fi).toMap))
    }

  final case class PositionalInfo(
      caret: Caret,
      input: Input,
      sourceQuery: String
  )

  def readAndGenerate[F[_]](schemaPath: Path, sharedPath: Path, validate: Boolean)(
      data: List[Input]
  )(implicit F: Async[F], E: Err[F]): F[Unit] =
    readEnv[F](schemaPath)(data).flatMap { e =>
      data
        .traverse(d => generateForInput[F](e, d) tupleLeft d)
        .flatMap { xs =>
          val translated: List[ExecutableDefinition[PositionalInfo]] = xs.flatMap { case (i, (q, _, _, eds)) =>
            eds.toList.map(_.map(c => PositionalInfo(c, i, q)))
          }
          val allFrags = translated.collect { case f: ExecutableDefinition.Fragment[PositionalInfo] => f }
          val allOps = translated.collect { case op: ExecutableDefinition.Operation[PositionalInfo] => op }
          lazy val errors = SchemaUtil.stubSchema(e.schema).flatMap { stubSchema =>
            allOps.parTraverse { op =>
              val full: NonEmptyList[ExecutableDefinition[PositionalInfo]] = NonEmptyList(op, allFrags)
              val vars: ValidatedNec[String, Map[String, Json]] = op.o match {
                case QueryAst.OperationDefinition.Simple(_) => Map.empty.validNec
                case QueryAst.OperationDefinition.Detailed(_, _, vds, _, _) =>
                  vds.toList
                    .flatMap(_.nel.toList)
                    .traverse(x => QueryValidation.generateVariableStub(x, e.schema))
                    .map(_.foldLeft(Map.empty[String, Json])(_ ++ _))
              }
              vars.toEither.flatMap { variables =>
                RootPreparation
                  .prepareRun(full, stubSchema, variables, None)
                  .leftMap(_.map { pe =>
                    val pos = pe.position.formatted
                    val caretErrs = pe.caret.distinct
                      .map { c =>
                        val (msg, _, _) = ParserUtil.showVirtualTextLine(c.sourceQuery, c.caret.offset)
                        s"in file ${c.input.query}\n" + msg
                      }
                    val msg = pe.message
                    s"$msg at $pos\n${caretErrs.mkString_("\n")}"
                  })
              }
            }
          }

          lazy val validateF = errors.leftTraverse[F, Unit](E.raise(_))

          val writeF: F[Unit] =
            xs.flatTraverse { case (_, (_, used, x, _)) =>
              writeStream[F](x.path, x.doc).compile.drain.as(used.toList)
            }.flatMap(_.distinct.toNel.traverse_(nel => writeStream[F](sharedPath, generateInputs(e, nel.toList)).compile.drain))

          F.whenA(validate)(validateF) *> writeF
        }
    }

  def mainGenerate[F[_]: Async](schemaPath: Path, sharedPath: Path, validate: Boolean)(data: List[Input]): F[List[String]] =
    readAndGenerate[EitherT[F, NonEmptyChain[String], *]](schemaPath, sharedPath, validate)(data).value.map(_.fold(_.toList, _ => Nil))
}
