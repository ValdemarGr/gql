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
import gql.parser.Pos
import cats.mtl.Tell

object Generator {
  /*
   * query MyQuery {
   *   name
   *   personalInfo {
   *     age
   *     height
   *   }
   *   friends {
   *     ...FriendFragment
   *   }
   * }
   *
   * fragment FriendFragment on Friend {
   *   name
   *   age
   *   personalInfo {
   *     age
   *   }
   *   ... on Human {
   *     name
   *     age
   *   }
   * }
   *
   * final case class MyQuery(
   *   name: String,
   *   personalInfo: MyQuery.PersonalInfo,
   *   friends: List[MyQuery.Friends]
   * )
   *
   * object MyQuery {
   *   final case class PersonalInfo(
   *     age: Int,
   *     height: Double
   *   )
   *
   *   object PersonalInfo {
   *     implicit lazy val selectionSet: SelectionSet[PersonalInfo] = (
   *       sel[Int]("age"),
   *       sel[Double]("height")
   *     ).mapN(apply)
   *   }
   *
   *   final case class Friends(
   *     friendFragment: Option[FriendFragment]
   *   )
   *
   *   object Friends {
   *     implicit lazy val selectionSet: SelectionSet[Friends] = (
   *       FriendFragment.frag
   *     ).mapN(apply)
   *   }
   *
   *   implicit lazy val selectionSet: SelectionSet[MyQuery] = (
   *     sel[String]("name"),
   *     sel[PersonalInfo]("personalInfo"),
   *     sel[List[Friends]]("friends")
   *   ).mapN(apply)
   * }
   *
   * final case class FriendFragment(
   *   name: String,
   *   age: Int,
   *   personalInfo: FriendFragment.PersonalInfo,
   *   humanFragment: Option[HumanFragment]
   * )
   *
   * object FriendFragment {
   *   final case class PersonalInfo(
   *     age: Int
   *   )
   *
   *   object PersonalInfo {
   *     implicit lazy val selectionSet: SelectionSet[PersonalInfo] = (
   *       sel[Int]("age")
   *     ).mapN(apply)
   *   }
   *
   *   final case class HumanFragment(
   *     name: String,
   *     age: Int
   *   )
   *
   *   object HumanFragment {
   *     implicit lazy val selectionSet: SelectionSet[HumanFragment] = (
   *       sel[String]("name"),
   *       sel[Int]("age")
   *     ).mapN(apply)
   *   }
   *
   *   implicit lazy val frag: SelectionSet[Option[FriendFragment]] =
   *    fragment("FriendFragment", "Friend") {
   *      (
   *        sel[String]("name"),
   *        sel[Int]("age"),
   *        sel[PersonalInfo]("personalInfo"),
   *        inlineFragment[HumanFragment]("Human")
   *      ).mapN(apply)
   *    }
   * }
   *
   */

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

  sealed trait ContextInfo
  object ContextInfo {
    final case class Fragment(
        fragmentName: Option[String],
        typeCnd: String,
        extraMatches: List[String]
    ) extends ContextInfo
    final case class Operation(
        op: OperationType,
        variables: List[VariableDefinition]
    ) extends ContextInfo
  }
  final case class Part(
      name: String,
      typePart: NonEmptyList[Doc],
      subParts: List[Part],
      codec: NonEmptyList[Doc],
      contextInfo: Option[ContextInfo]
  ) {
    // Yes yes, it is a fold
    def collapse: Doc = {
      val tpe = Doc.text(s"final case class $name") +
        hardIntercalateBracket('(', sep = Doc.comma)(typePart.toList)(')')

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

      val fullCodec = contextInfo match {
        case None => codecImplicit + codecSelection
        case Some(fi: ContextInfo.Fragment) =>
          val args = fi.fragmentName.toList ++ List(fi.typeCnd) ++ fi.extraMatches

          val fragType = fi.fragmentName.as("fragment").getOrElse("inlineFrag")

          val invocation = Doc.text(fragType) + params(args.map(quoted))

          codecImplicit +
            hardIntercalate(
              Doc.empty,
              Doc.empty,
              List(
                invocation + Doc.space + hardIntercalateBracket('{')(List(codecSelection))('}')
              )
            )
        case Some(op: ContextInfo.Operation) =>
          val operationType = op.op match {
            case OperationType.Query        => "Query"
            case OperationType.Mutation     => "Mutation"
            case OperationType.Subscription => "Subscription"
          }

          val operationTypePath =
            s"_root_.gql.parser.QueryAst.OperationType.${operationType}"

          val vars = op.variables.map { v =>
            val scalaType = ModifierStack.fromType(v.tpe).invert.showScala(identity)
            val args = quoted(v.name) :: v.defaultValue.toList.map(generateValue(_, anyValue = false))
            Doc.text("variable") + Doc.char('[') + Doc.text(scalaType) + Doc.char(']') + params(args)
          }

          val expr = vars.toNel match {
            case None => codecSelection
            case Some(vars) =>
              hardIntercalateBracket('(', Doc.text(" ~"))(vars.toList)(')') + Doc.text(".introduce ") +
                hardIntercalate(Doc.text("{ _ =>"), Doc.hardLine + Doc.char('}'), List(codecSelection))
          }

          val queryExpr = Doc.text("val queryExpr = ") + expr

          val queryPrefix =
            if (vars.size == 0) Doc.text("named")
            else Doc.text("parameterized")

          val args = List(
            Doc.text(operationTypePath),
            quoted(name),
            Doc.text("queryExpr")
          )

          val compiled = Doc.text("val query = ") +
            Doc.text("_root_.gql.client.Query.") + queryPrefix + hardIntercalateBracket('(', Doc.comma)(args)(')')

          queryExpr +
            Doc.hardLine +
            Doc.hardLine +
            compiled
      }

      val companionParts = subParts.map(_.collapse) ++ List(fullCodec)

      val companion = Doc.text("object") + Doc.space + Doc.text(name) + Doc.space +
        hardIntercalateBracket('{', Doc.hardLine)(companionParts)('}')

      Doc.intercalate(Doc.hardLine + Doc.hardLine, List(tpe.grouped, companion.grouped))
    }
  }

  def generateValue(v: V[AnyValue], anyValue: Boolean): Doc = {
    import V._
    val tpe = if (anyValue) "AnyValue" else "Const"
    v match {
      case IntValue(v)     => Doc.text(s"V.IntValue(${v.toString()})")
      case StringValue(v)  => Doc.text(s"""V.StringValue("$v")""")
      case FloatValue(v)   => Doc.text(s"""V.FloatValue("$v")""")
      case NullValue()     => Doc.text(s"""V.NullValue()""")
      case BooleanValue(v) => Doc.text(s"""V.BooleanValue(${v.toString()})""")
      case ListValue(v) =>
        Doc.text(s"V.ListValue[${tpe}](") +
          Doc
            .intercalate(Doc.comma + Doc.line, v.map(generateValue(_, anyValue)))
            .tightBracketBy(Doc.text("List("), Doc.char(')')) +
          Doc.text(")")
      case ObjectValue(fields) =>
        Doc.text(s"V.ObjectValue[${tpe}](") +
          Doc
            .intercalate(
              Doc.comma + Doc.line,
              fields.map { case (k, v) => quoted(k) + Doc.text(" -> ") + generateValue(v, anyValue) }
            )
            .bracketBy(Doc.text("List("), Doc.char(')')) +
          Doc.text(")")
      case EnumValue(v)     => Doc.text(s"""V.EnumValue("$v")""")
      case VariableValue(v) => Doc.text(s"""V.VariableValue("$v")""")
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

  final case class FieldPart(
      typePart: Doc,
      // local case class and companion object, not present if the field is a fragment or terminal
      subPart: Option[Part],
      codec: Doc
  )
  def generateField[F[_]: Parallel](
      companionName: String,
      schema: Map[String, TypeDefinition],
      f: Field[Pos],
      fd: FieldDefinition
  )(implicit
      P: CurrentPath[F],
      F: MonadError[F, NonEmptyChain[String]]
  ): F[FieldPart] = {
    val ms = ModifierStack.fromType(fd.tpe)
    val gqlName = f.alias.getOrElse(f.name)
    val n = toPascal(gqlName)

    f.selectionSet.value
      .map(_.selections.map(_.value))
      .parTraverse(generateTypeDef[F](schema, n, ms.inner, _, None))
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

        val sf = scalaField(gqlName, scalaTypeName)

        FieldPart(sf, subPart, clientSel)
      }
  }

  def generateSelection[F[_]: Parallel](
      companionName: String,
      schema: Map[String, TypeDefinition],
      fieldMap: Map[String, FieldDefinition],
      sel: Selection[Pos]
  )(implicit
      P: CurrentPath[F],
      F: MonadError[F, NonEmptyChain[String]]
  ): F[FieldPart] = {
    sel match {
      case fs: Selection.FieldSelection[Pos] =>
        val f = fs.field
        fieldMap.get(f.name) match {
          case None => raise[F, FieldPart](s"Field '${f.name}' not found in type '$companionName'")
          case Some(x) =>
            in(f.name) {
              generateField[F](companionName, schema, f, x)
            }
        }
      case frag: Selection.FragmentSpreadSelection[Pos] =>
        val fs = frag.fragmentSpread
        F.pure {
          FieldPart(
            scalaField(toCaml(fs.fragmentName), s"Option[${fs.fragmentName}]"),
            None,
            Doc.text(s"embed[Option[${fs.fragmentName}]]")
          )
        }
      case inlineFrag: Selection.InlineFragmentSelection[Pos] =>
        val ilf = inlineFrag.inlineFragment
        val ss = ilf.selectionSet.selections.map(_.value)
        val cnd = ilf.typeCondition.get
        val name = s"Inline${cnd}"
        in(s"inline-fragment-${cnd}") {
          generateTypeDef[F](schema, name, cnd, ss, Some(ContextInfo.Fragment(None, cnd, Nil)))
            .map { p =>
              FieldPart(
                scalaField(toCaml(name), s"Option[${companionName}.${name}]"),
                Some(p),
                Doc.text(s"embed[Option[${name}]]")
              )
            }
        }
    }
  }

  def generateTypeDef[F[_]: Parallel](
      schema: Map[String, TypeDefinition],
      name: String,
      typename: String,
      sels: NonEmptyList[Selection[Pos]],
      contextInfo: Option[ContextInfo]
  )(implicit
      P: CurrentPath[F],
      F: MonadError[F, NonEmptyChain[String]]
  ): F[Part] = {
    schema.get(typename) match {
      case None => raise[F, Part](s"Type `$typename` not found")
      case Some(td) =>
        in(s"type-definition-${typename}") {
          val fieldMapF: F[Map[String, FieldDefinition]] = td match {
            case TypeDefinition.ObjectTypeDefinition(_, _, _, fds)    => F.pure(fds.toList.map(f => f.name -> f).toMap)
            case TypeDefinition.InterfaceTypeDefinition(_, _, _, fds) => F.pure(fds.toList.map(f => f.name -> f).toMap)
            case TypeDefinition.UnionTypeDefinition(_, _, _)          => F.pure(Map.empty[String, FieldDefinition])
            case _ =>
              raise[F, Map[String, FieldDefinition]](
                s"Type tried to perform selection on`$typename`, but it is not an object, interface or union"
              )
          }

          val tn = FieldDefinition(None, "__typename", Nil, gql.parser.Type.NonNull(gql.parser.Type.Named("String")))

          fieldMapF.flatMap { fm =>
            sels
              .parTraverse(generateSelection[F](name, schema, fm + ("__typename" -> tn), _))
              .map { parts =>
                Part(
                  name,
                  parts.map(_.typePart),
                  parts.toList.flatMap(_.subPart.toList),
                  parts.map(_.codec),
                  contextInfo
                )
              }
          }
        }
    }
  }

  def imp(t: String) = Doc.text(s"import $t")

  def generateExecutableDefs[F[_]: Parallel](
      schema: Map[String, TypeDefinition],
      query: NonEmptyList[ExecutableDefinition[Pos]]
  )(implicit
      P: CurrentPath[F],
      U: UsedInputTypes[F],
      F: MonadError[F, NonEmptyChain[String]]
  ) = {
    val bodyF: F[NonEmptyList[Part]] = query.parTraverse {
      case ExecutableDefinition.Operation(o) =>
        o.value match {
          case OperationDefinition.Simple(_) =>
            raise[F, Part]("Simple operations are not supported, please name all your operations")
          case d: OperationDefinition.Detailed[Pos] =>
            in(s"operation-${d.name.get}") {
              val vds = d.variableDefinitions.toList.flatMap(_.nel.toList).map(_.value)
              val usedF = vds
                .map(vd => ModifierStack.fromType(vd.tpe).inner)
                .mapFilter(schema.get)
                .traverse_ {
                  case x: TypeDefinition.InputObjectTypeDefinition => U.tell(Set(Right(x)))
                  case x: TypeDefinition.EnumTypeDefinition        => U.tell(Set(Left(x)))
                  case _                                           => F.unit
                }

              usedF *>
                generateTypeDef[F](
                  schema,
                  d.name.get,
                  d.tpe.toString(),
                  d.selectionSet.selections.map(_.value),
                  Some(
                    ContextInfo.Operation(
                      d.tpe,
                      d.variableDefinitions.toList.flatMap(_.nel.toList.map(_.value))
                    )
                  )
                )
            }
        }
      case ExecutableDefinition.Fragment(f) => {
        in(s"fragment-${f.value.name}") {
          val f2 = f.value
          generateTypeDef[F](
            schema,
            f2.name,
            f2.typeCnd,
            f2.selectionSet.selections.map(_.value),
            Some(ContextInfo.Fragment(Some(f2.name), f2.typeCnd, Nil))
          )
        }
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
          imp("_root_.gql.parser.{Value => V, AnyValue, Const}"),
          imp("_root_.gql.client.dsl._"),
          imp("cats.implicits._")
        )
      ) + Doc.hardLine + Doc.hardLine + bodyDoc
    }
  }

  def generateInput(uis: List[UsedInput]): List[Doc] = {
    uis.map {
      case Left(x) =>
        val st = Doc.text(s"sealed trait ${x.name}")

        val names = x.values.toList.map(_.name)

        val companionParts = names.map { e =>
          Doc.text("case object ") + Doc.text(e) + Doc.text(" extends ") + Doc.text(x.name)
        } ++ List(Doc.hardLine) ++ List(
          Doc.text("implicit val circeDecoder = io.circe.Decoder.decodeString.emap") +
            hardIntercalateBracket('{') {
              names.map { e =>
                Doc.text("case ") + quoted(x.name) + Doc.text(s" => Right($e)")
              } ++ List(Doc.text("case x => Left(s\"Unknown enum value for ${x.name}: $x\")"))
            }('}')
        ) ++ List(Doc.hardLine) ++ List(
          Doc.text(s"implicit val circeEncoder = io.circe.Encoder.encodeString.contramap[${x.name}]") +
            hardIntercalateBracket('{') {
              names.map { e =>
                Doc.text("case ") + Doc.text(e) + Doc.text(s" => ") + quoted(x.name)
              }
            }('}')
        )

        val companion = Doc.text("object") + Doc.space + Doc.text(x.name) + Doc.space +
          hardIntercalateBracket('{')(companionParts)('}')

        st + Doc.hardLine + companion
      case Right(x) =>
        val fixedMods: List[(String, InverseModifierStack[String])] = x.inputFields.toList.map { f =>
          val ms = ModifierStack.fromType(f.tpe).invert

          // Add an option if the outermost modifier is not option and there is a default value
          val ms2 = if (!ms.modifiers.contains(InverseModifier.Optional) && f.defaultValue.isDefined) {
            ms.push(InverseModifier.Optional)
          } else ms

          f.name -> ms2
        }

        val cc = Doc.text("final case class ") + Doc.text(x.name) + hardIntercalateBracket('(', Doc.comma)(
          fixedMods.map { case (name, ms) =>
            val isOpt = ms.modifiers.headOption.contains(InverseModifier.Optional)

            Doc.text(name) + Doc.char(':') + Doc.space +
              Doc.text(ms.showScala(identity)) + (if (isOpt) Doc.text(" = None") else Doc.empty)
          }
        )(')')

        val companionParts = List(
          Doc.text(s"implicit val circeEncoder = io.circe.Encoder.AsObject.instance[${x.name}]{ a => ") +
            (
              Doc.hardLine +
                Doc
                  .intercalate(
                    Doc.hardLine,
                    List(
                      Doc.text("import io.circe.syntax._"),
                      Doc.text("Map") +
                        hardIntercalateBracket('(', sep = Doc.comma) {
                          fixedMods.map { case (name, _) =>
                            quoted(name) + Doc.text(" -> ") + Doc.text(s"a.$name.asJson")
                          }
                        }(')') + Doc.text(".asJsonObject")
                    )
                  )
            )
              .nested(2)
              .grouped + Doc.hardLine + Doc.char('}') + Doc.hardLine,
          Doc.text(s"implicit val typenameInstance = typename[${x.name}](") +
            quoted(x.name) + Doc.char(')')
        )

        val companion = Doc.text("object") + Doc.space + Doc.text(x.name) + Doc.space +
          hardIntercalateBracket('{')(companionParts)('}')

        cc + Doc.hardLine + companion
    }
  }

  def generateFor(
      schema: Map[String, TypeDefinition],
      query: NonEmptyList[ExecutableDefinition[Pos]]
  ): EitherNec[String, (Set[UsedInput], Doc)] = {
    type F[A] = WriterT[EitherT[Kleisli[Eval, Chain[String], *], NonEmptyChain[String], *], Set[UsedInput], A]
    generateExecutableDefs[F](schema, query).run.value.run(Chain.empty).value
  }

  def generateWith(
      schema: Map[String, TypeDefinition],
      query: String
  ): EitherNec[String, (Set[UsedInput], Doc)] =
    gql.parser.parseQuery(query).leftFlatMap(_.prettyError.value.leftNec).flatMap(generateFor(schema, _))

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

  def generateForInput[F[_]: Async](
      schema: Map[String, TypeDefinition],
      i: Input
  ): fs2.Stream[F, EitherNec[String, (Set[UsedInput], Output)]] =
    Files[F]
      .readAll(i.query)
      .through(fs2.text.utf8.decode[F])
      .foldMonoid
      .map(generateWith(schema, _))
      .map(_.map { case (usedInputs, doc) => (usedInputs, Output(i.output, doc)) })

  def writeStream[F[_]: Async](path: Path, doc: Doc) =
    fs2.Stream
      .iterable(doc.renderStream(80))
      .lift[F]
      .through(fs2.text.utf8.encode)
      .through(Files[F].writeAll(path))

  def readAndGenerate[F[_]](schemaPath: Path, sharedPath: Path)(
      data: fs2.Stream[F, Input]
  )(implicit F: Async[F]): F[List[String]] =
    Files[F]
      .readAll(schemaPath)
      .through(fs2.text.utf8.decode[F])
      .compile
      .foldMonoid
      .map(getSchemaFrom)
      .flatMap {
        case Left(e) => F.pure(List(s"failed to parse schema at $schemaPath with error $e"))
        case Right(s) =>
          data
            .flatMap(generateForInput[F](s, _))
            .evalMap[F, Validated[List[String], Set[UsedInput]]] {
              case Left(e)          => F.pure(e.toList.invalid)
              case Right((used, x)) => writeStream[F](x.path, x.doc).compile.drain.as(used.valid)
            }
            .compile
            .foldMonoid
            .flatMap(_.toEither match {
              case Left(e) => F.pure(e.toList)
              case Right(inputs) =>
                inputs.toList.toNel.traverse_ { nel =>
                  val d = Doc.intercalate(
                    Doc.hardLine + Doc.hardLine,
                    List(
                      Doc.text("package gql.client.generated"),
                      Doc.intercalate(
                        Doc.hardLine,
                        List(
                          imp("_root_.gql.client.dsl._")
                        )
                      )
                    ) ++ generateInput(nel.toList)
                  )

                  writeStream[F](sharedPath, d).compile.drain
                } as Nil
            })
      }
}
