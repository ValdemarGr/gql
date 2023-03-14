package gql.client.codegen

import gql.parser.TypeSystemAst._
import gql.parser.QueryAst._
import gql.parser.{Value => V, AnyValue}
import cats.data._
import cats._
import org.typelevel.paiges.Doc
import cats.implicits._
import gql.ModifierStack

object Main {
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

  def toCaml(str: String): String = {
    val hd = str.take(1).toLowerCase()
    val tl = str.drop(1)
    hd + tl
  }

  def scalaField(name: String, tpe: String): Doc =
    Doc.text(name) + Doc.char(':') + Doc.space + Doc.text(tpe)

  def generateValue(v: V[AnyValue]): Doc = ???

  final case class Part(
      name: String,
      typePart: NonEmptyList[Doc],
      subParts: List[Part],
      codec: NonEmptyList[Doc]
  )

  final case class FieldPart(
      typePart: Doc,
      subPart: Option[Part],
      codec: Doc
  )
  def generateField(
      schema: Map[String, TypeDefinition],
      f: Field,
      fd: FieldDefinition
  ): FieldPart = {
    val ms = ModifierStack.fromType(fd.tpe)
    val tpeText = ms.show(identity)
    val tpe = Doc.text(tpeText)

    val subPart: Option[Part] = f.selectionSet.value
      .map(_.selections.map(_.value))
      .map(generateTypeDef(schema, f.alias.getOrElse(f.name), ms.inner, _))

    val argPart = f.arguments.toList.flatMap(_.nel.toList).map { x =>
      Doc.text("arg") +
        (
          Doc.text(x.name) + Doc.char(',') + Doc.space +
            generateValue(x.value)
        ).bracketBy(Doc.char('('), Doc.char(')'))
    }

    val clientSel = Doc.text("sel") +
      tpe.bracketBy(Doc.char('['), Doc.char(']')) +
      Doc
        .intercalate(Doc.comma + Doc.line, Doc.text(fd.name) :: argPart)
        .bracketBy(Doc.char('('), Doc.char(')'))

    FieldPart(scalaField(fd.name, tpeText), subPart, clientSel)
  }

  def generateFragmentSpread(
      schema: Map[String, TypeDefinition]
  ) = {}

  def generateSelection(
      schema: Map[String, TypeDefinition],
      fieldMap: Map[String, FieldDefinition],
      sel: Selection
  ): FieldPart = {
    sel match {
      case fs: Selection.FieldSelection =>
        val f = fs.field
        fieldMap.get(f.name) match {
          case None    => ???
          case Some(x) => generateField(schema, f, x)
        }
      case frag: Selection.FragmentSpreadSelection =>
        val fs = frag.fragmentSpread
        FieldPart(
          scalaField(toCaml(fs.fragmentName), fs.fragmentName),
          None,
          Doc.text(s"${fs.fragmentName}.frag")
        )
      case inlineFrag: Selection.InlineFragmentSelection =>
        val ilf = inlineFrag.inlineFragment
        val ss = ilf.selectionSet.selections.map(_.value)
        val cnd = ilf.typeCondition.get
        val name = s"Inline${cnd}"
        val p = generateTypeDef(schema, name, cnd, ss)
        FieldPart(
          scalaField(toCaml(name), name),
          Some(p),
          Doc.text(s"inlineFragment[${name}](${cnd})")
        )
    }
  }

  def generateTypeDef(
      schema: Map[String, TypeDefinition],
      name: String,
      typename: String,
      sels: NonEmptyList[Selection]
  ): Part = {
    schema.get(typename) match {
      case None => ???
      case Some(td) =>
        val fieldMap = td match {
          case TypeDefinition.ObjectTypeDefinition(_, _, _, fds)    => fds.toList.map(f => f.name -> f).toMap
          case TypeDefinition.InterfaceTypeDefinition(_, _, _, fds) => fds.toList.map(f => f.name -> f).toMap
          case TypeDefinition.UnionTypeDefinition(_, _, _)          => Map.empty[String, FieldDefinition]
          case _                                                    => ???
        }

        val parts = sels.map(generateSelection(schema, fieldMap, _))

        Part(
          name,
          parts.map(_.typePart),
          parts.toList.flatMap(_.subPart.toList),
          parts.map(_.codec)
        )
    }
  }

  def generateForTypeDefinition(
      schema: Map[String, TypeDefinition],
      eds: NonEmptyList[ExecutableDefinition]
  ) = {
    val frags = eds
      .collect { case ExecutableDefinition.Fragment(f) => f.value }
      .map { f =>
        Doc.text(s"final case class ${f.name}") +
          Doc.intercalate(Doc.comma + Doc.line, Nil).bracketBy(Doc.char('('), Doc.char(')'))

        f.selectionSet.selections.map(_.value)
        f
      }
  }
}

