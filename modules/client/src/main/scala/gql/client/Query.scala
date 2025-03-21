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
package gql.client

import gql.parser.{QueryAst => P}
import io.circe._
import org.typelevel.paiges._
import cats.implicits._
import cats._
import gql.client.Selection.FragmentSpread
import gql.client.Selection.Field
import gql.client.Selection.InlineFragment
import gql.parser.GraphqlRender
import io.circe.syntax._
import gql.parser.TypeSystemAst
import gql.parser.AnyValue

trait QueryLike {
  def queryString: String

  def validate(ast: Map[String, TypeSystemAst.TypeDefinition]): List[String] =
    QueryValidation.validateQuery(queryString, ast)
}

final case class SimpleQuery[A](
    operationType: P.OperationType,
    selectionSet: SelectionSet[A]
) extends QueryLike {
  lazy val queryString: String = Query.renderQuery(this)

  def compile: Query.Compiled[A] = Query.Compiled(
    Query.queryDecoder(selectionSet),
    queryString
  )

  def compileFull: Query.Compiled[QueryResult[A]] = Query.Compiled(
    Query.queryResultDecoder(selectionSet),
    queryString
  )
}

final case class NamedQuery[A](name: String, query: SimpleQuery[A]) extends QueryLike {
  lazy val queryString: String = Query.renderQuery(query, name.some)

  def compile: Query.Compiled[A] = Query.Compiled(
    Query.queryDecoder(query.selectionSet),
    queryString
  )

  def compileFull: Query.Compiled[QueryResult[A]] = Query.Compiled(
    Query.queryResultDecoder(query.selectionSet),
    queryString
  )
}

final case class ParameterizedQuery[V, A](
    name: String,
    query: SimpleQuery[A],
    variables: Var.Impl[V]
) extends QueryLike {
  lazy val queryString: String = Query.renderQuery(query, name.some, variables.written.map(_.toList))

  def compile(v: V): Query.Compiled[A] = Query.Compiled(
    Query.queryDecoder(query.selectionSet),
    queryString,
    variables.value.encodeObject(v).some
  )

  def compileFull(v: V): Query.Compiled[QueryResult[A]] = Query.Compiled(
    Query.queryResultDecoder(query.selectionSet),
    queryString,
    variables.value.encodeObject(v).some
  )
}

object Query {
  final case class Compiled[A](
      decoder: Decoder[A],
      query: String,
      variables: Option[JsonObject] = None
  )

  def matchAlias(typename: String): String =
    s"__matchedOn$typename"

  object Compiled {
    implicit def enc[A]: io.circe.Encoder.AsObject[Query.Compiled[A]] =
      io.circe.Encoder.AsObject.instance[Query.Compiled[A]] { x =>
        JsonObject.fromMap(
          Map(
            "query" -> Some(Json.fromString(x.query)),
            "variables" -> x.variables.map(_.asJson)
          ).collect { case (k, Some(v)) => k -> v }
        )
      }
  }

  def simple[A](operationType: P.OperationType, selectionSet: SelectionSet[A]): SimpleQuery[A] =
    SimpleQuery(operationType, selectionSet)

  def named[A](operationType: P.OperationType, name: String, selectionSet: SelectionSet[A]): NamedQuery[A] =
    NamedQuery(name, simple(operationType, selectionSet))

  def parameterized[A, V](
      operationType: P.OperationType,
      name: String,
      vc: VariableClosure[V, A]
  ): ParameterizedQuery[V, A] =
    ParameterizedQuery(name, simple(operationType, vc.query), vc.variables)

  def renderQuery(sq: SimpleQuery[?], name: Option[String] = None, variables: List[Var.One[?]] = Nil): String = {
    val main =
      Render.renderOperationType(sq.operationType) +
        name.fold(Doc.empty)(n => Doc.space + Doc.text(n)) +
        variables.toNel.fold(Doc.empty) { xs =>
          Doc.intercalate(Doc.comma + Doc.line, xs.map(Render.renderVar(_)).toList).bracketBy(Doc.char('('), Doc.char(')'))
        } +
        Doc.space + Render.renderSelectionSet(sq.selectionSet)

    val frags = Doc.intercalate(
      Doc.hardLine + Doc.hardLine,
      findFragments(sq.selectionSet).map(Render.renderFragment)
    )

    (main + frags).render(100)
  }

  def queryDecoder[A](ss: SelectionSet[A]): Decoder[A] =
    Decoder.instance(_.get[A]("data")(Dec.decoderForSelectionSet(ss)))

  def queryResultDecoder[A](ss: SelectionSet[A]): Decoder[QueryResult[A]] =
    QueryResult.decoder(Dec.decoderForSelectionSet(ss))

  def findSelectionFragments[A](selection: Selection[A]): List[Fragment[?]] = selection match {
    case f: FragmentSpread[a] => f.fr :: findFragments(f.fr.subSelection)
    case f: Field[a] =>
      def unpackSubQuery(q: SubQuery[?]): List[Fragment[?]] =
        q match {
          case Terminal(_)              => Nil
          case ListModifier(subQuery)   => unpackSubQuery(subQuery)
          case OptionModifier(subQuery) => unpackSubQuery(subQuery)
          case ss: SelectionSet[?]      => findFragments(ss)
        }

      unpackSubQuery(f.subQuery)
    case f: InlineFragment[a] => findFragments(f.subSelection)
  }

  def findFragments(ss: SelectionSet[?]): List[Fragment[?]] =
    ss.impl.enumerate.toList.flatMap(findSelectionFragments)

  object Dec {
    def decoderForSubQuery[A](sq: SubQuery[A]): Decoder[A] = sq match {
      case Terminal(decoder)     => decoder // .adaptError(e => e.withMessage(s"${e.message} at ${sp.toString}"))
      case lm: ListModifier[a]   => Decoder.decodeList(decoderForSubQuery(lm.subQuery))
      case om: OptionModifier[a] => Decoder.decodeOption(decoderForSubQuery(om.subQuery))
      case ss: SelectionSet[A]   => decoderForSelectionSet(ss)
    }

    def decoderForFragment[A](on: String, ss: SelectionSet[A]): Decoder[Option[A]] = {
      Decoder.instance { cursor =>
        cursor
          .get[Option[String]](matchAlias(on))
          .flatMap(_.traverse(_ => cursor.as(decoderForSelectionSet(ss))))
      }
    }

    def decoderForSelectionSet[A](ss: SelectionSet[A]): Decoder[A] = {
      val compiler = new (Selection ~> Decoder) {
        override def apply[B](fa: Selection[B]): Decoder[B] = {
          fa match {
            case f: Selection.Field[a] =>
              val name = f.alias0.getOrElse(f.fieldName)
              Decoder.instance(_.get(name)(decoderForSubQuery(f.subQuery)))
            case f: FragmentSpread[a] => decoderForFragment(f.fr.on, f.fr.subSelection)
            case f: InlineFragment[a] => decoderForFragment(f.on, f.subSelection)
          }
        }
      }

      ss.impl.foldMap(compiler).emap(_.toEither.leftMap(_ /*.map(s => s"${s.value} at ${s.position.toString}")*/ .intercalate(", ")))
    }
  }

  object Render {
    def renderOperationType(op: P.OperationType): Doc =
      op match {
        case P.OperationType.Query        => Doc.text("query")
        case P.OperationType.Mutation     => Doc.text("mutation")
        case P.OperationType.Subscription => Doc.text("subscription")
      }

    def renderVar(v: Var.One[?]): Doc = {
      val default = v.default match {
        case None          => Doc.empty
        case Some(default) => Doc.space + Doc.char('=') + Doc.space + GraphqlRender.renderValue(default)
      }
      Doc.text(s"$$${v.name.name}") + Doc.space + Doc.char(':') + Doc.space + Doc.text(v.tpe) + default
    }

    def renderArg(a: P.Argument[Unit, AnyValue]): Doc =
      Doc.text(a.name) + Doc.char(':') + Doc.space + GraphqlRender.renderValue(a.value)

    def renderArgs(args: List[P.Argument[Unit, AnyValue]]): Doc =
      args match {
        case Nil => Doc.empty
        case xs  => Doc.intercalate(Doc.comma + Doc.space, xs.map(renderArg)).bracketBy(Doc.char('('), Doc.char(')'))
      }

    def matchTypename(on: String): SelectionSet[String] =
      dsl.sel.build[String]("__typename", _.alias(matchAlias(on)))

    def renderDirective(d: P.Directive[Unit, AnyValue]): Doc =
      Doc.text("@") + Doc.text(d.name) + renderArgs(d.arguments.toList.flatMap(_.nel.toList))

    def renderDirectives(ds: List[P.Directive[Unit, AnyValue]]): Doc =
      ds match {
        case Nil => Doc.empty
        case xs  => Doc.intercalate(Doc.space, xs.map(renderDirective))
      }

    def renderSelection[A](selection: Selection[A]): Doc = selection match {
      // Fragments are floated to the top level and handled separately
      case f: FragmentSpread[a] =>
        Doc.text(s"...${f.fr.name}") + Doc.space + renderDirectives(f.directives)
      case InlineFragment(on, subSelection, directives) =>
        Doc.text(s"... on $on") + Doc.space + renderDirectives(directives) +
          Doc.space + renderSelectionSet(subSelection <* matchTypename(on))
      case Selection.Field(name, alias, args, sq, directives) =>
        val aliased = alias match {
          case None    => Doc.empty
          case Some(a) => Doc.text(a) + Doc.char(':') + Doc.space
        }

        val lhs = aliased + Doc.text(name)

        val argsDoc = renderArgs(args)

        def renderSubQuery(sq: SubQuery[?]): Doc = sq match {
          case Terminal(_)              => Doc.empty
          case ss: SelectionSet[?]      => Doc.space + renderSelectionSet(ss)
          case ListModifier(subQuery)   => renderSubQuery(subQuery)
          case OptionModifier(subQuery) => renderSubQuery(subQuery)
        }
        val rhs = renderSubQuery(sq)

        lhs + argsDoc + renderDirectives(directives) + rhs
    }

    def renderSelectionSet(ss: SelectionSet[?]): Doc = {
      val docs = ss.impl.enumerate.toList.map(renderSelection)
      Doc.intercalate(Doc.comma + Doc.line, docs).bracketBy(Doc.char('{'), Doc.char('}'))
    }

    def renderFragment(frag: Fragment[?]): Doc = frag match {
      case frag: Fragment[a] =>
        Doc.text("fragment") + Doc.space +
          Doc.text(frag.name) + Doc.space +
          Doc.text("on") + Doc.space + Doc.text(frag.on) + Doc.space +
          renderSelectionSet(frag.subSelection <* matchTypename(frag.on))
    }
  }
}
