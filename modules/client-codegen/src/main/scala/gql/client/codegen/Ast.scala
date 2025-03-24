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

import gql.parser.{QueryAst => QA}
import QA._
import gql.parser.{Value => V, AnyValue}
import cats.data._
import org.typelevel.paiges.Doc
import cats.implicits._
import gql._
import cats.parse.Caret
import gql.client.codegen.{RenderHelpers => R}

final case class CaseClassField(
    name: String,
    tpe: InverseModifierStack[String],
    default: Option[String]
) {
  val escapedName = R.escapeFieldName(name)

  val doc: Doc = Doc.text(escapedName) + Doc.char(':') + Doc.space +
    Doc.text(tpe.showScala(identity)) +
    default.map(x => Doc.text(s" = ${x}")).getOrElse(Doc.empty)
}

final case class CaseClass(
    name: String,
    fields: List[CaseClassField],
    methods: List[Doc]
) {
  val doc: Doc = R.caseClass(name, fields.map(_.doc), methods)
}

final case class Obj(
    name: String,
    body: List[Doc]
) {
  lazy val doc: Doc =
    Doc.text("object") + Doc.space + Doc.text(name) + Doc.space +
      R.hardIntercalateBracket('{', Doc.hardLine)(body)('}')
}

sealed trait Sel {
  def sel: Doc
  def cc: CaseClassField
}
final case class SelField(
    name: String,
    alias: Option[String],
    args: List[Argument[Caret, AnyValue]],
    typename: ModifierStack[String],
    directives: List[QA.Directive[Caret, AnyValue]]
) extends Sel {
  val tn = Doc.text(typename.invert.showScala(identity))

  // List(arg("argName1", value1), arg("argName2", value2))
  val argList: List[Doc] = args.map(R.generateArgument)

  val directiveList = directives.map(R.generateDirective)

  // sel[Type]("fieldName", _.args(arg("argName1", value1), arg("argName2", value2)))
  lazy val sel: Doc = {
    Doc.text("sel.build") +
      R.typeParams(List(tn)) +
      R.params {
        val al = argList.toNel.map(xs => R.method("args", xs.toList)).getOrElse(Doc.empty)
        val ds = directiveList.toNel.map(xs => R.method("directives", xs.toList)).getOrElse(Doc.empty)
        val a = alias.map(x => R.method("alias", List(R.quoted(x)))).getOrElse(Doc.empty)
        R.quoted(name) :: List(Doc.text("x => x") + al + a + ds)
      }
  }

  lazy val cc = CaseClassField(R.toCaml(alias.getOrElse(name)), typename.invert, None)
}

final case class SelFragSpread(
    scalaType: String,
    fragmentName: String,
    condition: String,
    inl: Boolean,
    required: Boolean,
    directives: List[QA.Directive[Caret, AnyValue]]
) extends Sel {
  lazy val req =
    if (required) R.method("requiredFragment", List(fragmentName, condition).map(R.quoted))
    else Doc.empty

  lazy val sel: Doc = {
    val (fn, p) =
      if (inl) ("inlineFrag.build", List(R.quoted(condition)))
      else ("fragment.spread.build", Nil)

    val d = directives.map(R.generateDirective).toNel.map(xs => R.method("directives", xs.toList)).getOrElse(Doc.empty)
    val ps = p ++ List(Doc.text(s"x => x") + d)
    Doc.text(fn) + R.typeParams(List(Doc.text(scalaType))) + R.params(ps) + req
  }

  lazy val cc: CaseClassField = {
    val m = List(InverseModifier.Optional).filter(_ => !required)
    CaseClassField(
      R.toCaml(fragmentName),
      InverseModifierStack(m, scalaType),
      None
    )
  }
}

final case class VariantCase(
    sharedType: String,
    fields: List[(String, String)]
) {
  val asScalaType = sharedType
  val asFieldName = R.toCaml(sharedType)
}

final case class Variant(
    cases: NonEmptyList[VariantCase]
) {
  val name = "Variant"

  def method(objPath: String): Doc = {
    Doc.text(s"lazy val variant: Option[${objPath}.${name}] = ") +
      Doc.hardLine +
      Doc.intercalate(
        Doc.text(" orElse") + Doc.hardLine,
        cases.toList.map { vc =>
          val n = s"${objPath}.${name}.On${vc.asScalaType}"
          R.params(vc.fields.map { case (v, _) => Doc.text(v) }) +
            R.mapN(vc.fields.length)(Doc.text(s"${n}.apply"))
        }
      )
  }

  val adt: Doc = {
    val st = s"sealed trait $name extends Product with Serializable"
    val comp = R.obj(
      name,
      cases.toList.map { vc =>
        val n = "On" + vc.asScalaType
        val f = vc.fields.map { case (n, t) => R.scalaField(n, t) }
        R.caseClass(n, f, Nil) + Doc.text(" extends ") + Doc.text(name)
      }
    )
    Doc.text(st) + Doc.hardLine + comp
  }
}

final case class Selection(
    scalaType: String,
    fields: List[Sel],
    variant: Option[Variant]
) {
  lazy val cc = CaseClass(scalaType, fields.map(_.cc), variant.toList.map(_.method(scalaType)))

  val tcName = "selectionSet"

  lazy val sel: Doc = {
    val body = fields.map(_.sel)
    val t = s"SelectionSet[${scalaType}]"
    val pref = s"implicit val selectionSet"

    Doc.text(s"$pref: $t = ") +
      R.verticalApply("", body) +
      R.mapN(body.length)(Doc.text("apply"))
  }
}

final case class VariableField(
    name: String,
    tpe: ModifierStack[String],
    default: Option[V[AnyValue, Caret]]
) {
  val asInputField = InputField(name, tpe, default.isDefined)

  lazy val variableDecl: Doc = {
    val ft = tpe.invert.showScala(identity)
    val func = if (asInputField.isOmittable) "omittableVariable" else "variable"
    val ps = R.quoted(name) :: default.toList.map(R.generateValue(_, anyValue = false))
    Doc.text(s"${func}[$ft]") + R.params(ps)
  }

  lazy val varName: String = s"var_${asInputField.cc.name}"
}

final case class InputField(
    name: String,
    tpe: ModifierStack[String],
    hasDefault: Boolean
) {
  val isOmittable: Boolean =
    hasDefault || tpe.invert.modifiers.headOption.contains(InverseModifier.Optional)

  val fullType: InverseModifierStack[String] =
    if (isOmittable) tpe.invert.push(InverseModifier.Optional)
    else tpe.invert

  val fieldName = R.toCaml(name)

  val cc = CaseClassField(fieldName, fullType, if (isOmittable) Some("None") else None)

  def encodeField(ccName: String): Doc = {
    val idx = s"${ccName}.${cc.escapedName}"
    val enc =
      if (isOmittable)
        Doc.text(idx) + R.method("map", List(Doc.text("_.asJson")))
      else
        Doc.text(s"Some($idx.asJson)")

    R.quoted(fieldName) + Doc.text(s" -> ") + enc
  }

  def setter(ccType: String): Doc = {
    val suf = R.toPascal(name)
    val t = tpe.invert.showScala(identity)
    val fn = cc.escapedName
    val lift = if (isOmittable) "Some(value)" else "value"
    Doc.text(s"def set$suf(value: $t): ${ccType} = copy($fn = $lift)")
  }
}

final case class InputType(
    name: String,
    fields: NonEmptyList[InputField]
) {
  lazy val cc = CaseClass(name, fields.toList.map(_.cc), fields.toList.map(_.setter(name)))

  lazy val enc: Doc = {
    val fp = "a"

    val body = R.blockScope(Doc.text(s" $fp =>"))(
      R.imp("io.circe.syntax._"),
      R.verticalApply(
        "Map",
        fields.toList.map(_.encodeField(fp))
      ) + Doc.text(".collect{ case (k, Some(v)) => k -> v }.asJsonObject")
    )

    val pref = Doc.text("implicit lazy val circeEncoder")
    val t = Doc.text(s"io.circe.Encoder.AsObject[${name}]")
    val decl = pref + Doc.text(": ") + t + Doc.text(" = ")

    decl + body
  }

  lazy val typenameInstance: Doc =
    Doc.text(s"implicit val typenameInstance: Typename[${name}] = typename[${name}]") +
      R.params(List(R.quoted(name)))

  lazy val companion: Doc = R.obj(name, List(enc, typenameInstance))
}

final case class VariableType(fields: NonEmptyList[VariableField]) {
  val tn = "Variables"
  val asInputType = InputType(tn, fields.map(_.asInputField))

  def intro(body: Doc): Doc = {
    val varNames = fields.map(_.varName)
    val sep = "), "
    lazy val applyNestedTuples = ("(" * fields.size) + varNames.toList.mkString(sep) + ")"
    lazy val unapplyNestedTuples = varNames.toList.mkString(", ")
    lazy val contra = Doc.text(s".contramap[$tn]") +
      R.blockScope(Doc.text(s" case $tn($unapplyNestedTuples) =>"))(Doc.text(applyNestedTuples))

    val makeVariables =
      if (fields.size > 1) contra
      else Doc.empty

    R.hardIntercalateBracket('(', Doc.text(" ~"))(fields.toList.map(_.variableDecl))(')') +
      makeVariables +
      Doc.text(".introduce") + R.blockScope(Doc.text(" _ =>"))(body)
  }
}

final case class TypeIntro(
    selection: Selection,
    companionContent: List[TypeIntro]
) {
  lazy val tpeDoc: Doc = selection.cc.doc

  def companion(extraBodyContent: List[Doc]): Doc = {
    val tpes = companionContent.flatMap(x => List(x.tpeDoc, x.companion(Nil)))
    val tc = selection.sel
    R.obj(selection.scalaType, selection.variant.toList.map(_.adt) ++ tpes ++ (tc :: extraBodyContent))
  }
}

sealed trait Toplevel {
  def docs: List[Doc]
}
final case class Operation(
    operationType: OperationType,
    ti: TypeIntro,
    vars: Option[VariableType]
) extends Toplevel {
  val ot = operationType match {
    case OperationType.Query        => "Query"
    case OperationType.Mutation     => "Mutation"
    case OperationType.Subscription => "Subscription"
  }
  val operationTypePath =
    s"_root_.gql.parser.QueryAst.OperationType.${operationType}"

  val varIntro: String = vars.as("parameterized").getOrElse("named")

  val sel = ti.selection.sel

  val tcName = Doc.text(ti.selection.tcName)
  val qe = "queryExpr"
  val queryExpr = Doc.text(s"val $qe = ") + vars.map(_.intro(tcName)).getOrElse(tcName)
  val compiled: Doc = Doc.text(s"val query = _root_.gql.client.Query.${varIntro}") +
    R.params(List(Doc.text(operationTypePath), R.quoted(ti.selection.scalaType), Doc.text(qe)))

  val companion = ti.companion(extraBodyContent = vars.toList.map(_.asInputType.cc.doc) ++ List(queryExpr, compiled))

  val docs = List(ti.selection.cc.doc, companion)
}

final case class ToplevelFragment(
    ti: TypeIntro,
    fragName: String,
    on: String
) extends Toplevel {
  val fragDef: Doc =
    Doc.text(s"implicit val fragdef: Fragment[${ti.selection.scalaType}] = ") +
      Doc.text(s"fragment[${ti.selection.scalaType}]") +
      R.params(List(R.quoted(fragName), R.quoted(on)))

  val docs = List(ti.tpeDoc, ti.companion(List(fragDef)))
}
