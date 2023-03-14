package gql.client.codegen

import gql.parser.TypeSystemAst._
import gql.parser.QueryAst._
import cats.data._
import cats._
import org.typelevel.paiges.Doc
import cats.implicits._

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
     *     sel[List[FriendFragment]]("friends")
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
     *   implicit lazy val frag: Selection[Option[FriendFragment]] = fragment("FriendFragment", "Friend"){
     *     (
     *       sel[String]("name"),
     *       sel[Int]("age"),
     *       sel[PersonalInfo]("personalInfo"),
     *       inlineFragment[HumanFragment]("Human")
     *     ).mapN(apply)
     *   }
     * }
     * 
     */

    // Emits a case class and a companion object with an implicit decoder
    def generateSelectionSet(
        td: TypeDefinition,
        eds: NonEmptyList[Selection]
    ): Option[(Doc, Doc)] = {
        val o = td match {
            case TypeDefinition.ObjectTypeDefinition(_, name, _, fds) => 
                (name, fds.toList.map(f => f.name -> f).toMap).some
            case TypeDefinition.InterfaceTypeDefinition(_, name, _, fds) =>
                (name, fds.toList.map(f => f.name -> f).toMap).some
            case TypeDefinition.UnionTypeDefinition(_, name, _) =>
                (name, Map.empty[String, FieldDefinition]).some
            case _ => None
        }

        o.map{ case (name, fds) => 
            eds.map{ 
                case Selection.FieldSelection(f) =>
                    val n = f.alias.getOrElse(f.name)
                    fds.get(n) match {
                        case None => ???
                        case Some(fd) => 
                            fd
                    }
                    ???
                case _ => ()
            }
        }
        ???
    }

    def generateForTypeDefinition(
        schema: Map[String, TypeDefinition], 
        eds: NonEmptyList[ExecutableDefinition]
    ) = {
        val frags = eds
            .collect{ case ExecutableDefinition.Fragment(f) => f.value }
            .map{ f => 
                Doc.text(s"final case class ${f.name}") +
                    Doc.intercalate(Doc.comma + Doc.line, Nil).bracketBy(Doc.char('('), Doc.char(')'))

                f.selectionSet.selections.map(_.value)
                f
            }
    }
}