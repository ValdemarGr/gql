package gql.client

import cats.data._
import gql.parser.{QueryParser => P}
import io.circe._
import org.typelevel.paiges._
import cats.implicits._
import cats._
import gql.client.Selection.Fragment
import gql.client.Selection.InlineFragment
import gql.std.FreeApply

final case class SimpleQuery[A](
    operationType: P.OperationType,
    selectionSet: SelectionSet[A]
) {
  def compile: Query.Compiled[A] = ???
}

final case class NamedQuery[A](name: String, query: SimpleQuery[A]) {
  def compile: Query.Compiled[A] = ???
}

final case class ParameterizedQuery[A, V](
    name: String,
    query: SimpleQuery[A],
    variables: Var.Impl[V]
) {
  def compile(variables: V): Query.Compiled[A] = ???
}

object Query {
  final case class Compiled[A](
      decoder: Decoder[A],
      query: String,
      variables: Option[Json]
  )

  def renderSelectionSet(ss: SelectionSet[?]): String =
    Render.renderSelectionSet(ss).render(100)

  def findFragments(ss: SelectionSet[?]): List[Fragment[?]] = {
    val discoveryCompiler = new (Selection ~> Const[List[Fragment[?]], *]) {
      override def apply[A](fa: Selection[A]): Const[List[Fragment[?]], A] = Const {
        fa match {
          case f: Fragment[?] => f :: findFragments(f.subSelection)
          case _              => Nil
        }
      }
    }
    ss.impl.underlying.foldMap(discoveryCompiler).getConst
  }

  object Dec {
    def decoderForSubQuery[A](sq: SubQuery[A]): Decoder[A] = sq match {
      case Terminal(decoder)     => decoder
      case lm: ListModifier[a]   => Decoder.decodeList(decoderForSubQuery(lm.subQuery))
      case om: OptionModifier[a] => Decoder.decodeOption(decoderForSubQuery(om.subQuery))
      case ss: SelectionSet[A]   => decoderForSelectionSet(ss)
    }

    def decoderForFragment[A](on: String, also: Set[String], ss: SelectionSet[A]): Decoder[Option[A]] = {
      Decoder.instance { cursor =>
        cursor.get[String]("__typename").flatMap { tn =>
          if (on === tn || also.contains(tn)) cursor.as(decoderForSelectionSet(ss)).map(_.some)
          else cursor.as(Decoder.const(None))
        }
      }
    }

    def decoderForSelectionSet[A](ss: SelectionSet[A]): Decoder[A] = {
      val compiler = new (Selection ~> Decoder) {
        override def apply[A](fa: Selection[A]): Decoder[A] = {
          fa match {
            case f: Selection.Field[a] =>
              val name = f.alias.getOrElse(f.fieldName)
              Decoder.instance(_.get(name)(decoderForSubQuery(f.subQuery)))
            case f: Fragment[a]       => decoderForFragment(f.on, f.matchAlsoSet, f.subSelection)
            case f: InlineFragment[a] => decoderForFragment(f.on, f.matchAlsoSet, f.subSelection)
          }
        }
      }

      ss.impl match {
        case impl: SelectionSet.Impl[a, b] =>
          val fa: FreeApply[Selection, a] = impl.underlying
          fa.foldMap(compiler).emap(a => impl.emap(a).toEither.leftMap(_.intercalate(", ")))
      }
    }
  }

  object Render {
    def renderOperationType(op: P.OperationType): Doc =
      op match {
        case P.OperationType.Query        => Doc.text("query")
        case P.OperationType.Mutation     => Doc.text("mutation")
        case P.OperationType.Subscription => Doc.text("subscription")
      }

    def renderValue(v: P.Value): Doc = {
      import P.Value._
      v match {
        case IntValue(v)     => Doc.text(v.toString)
        case StringValue(v)  => Doc.text(s""""$v"""")
        case FloatValue(v)   => Doc.text(v.toString)
        case NullValue       => Doc.text("null")
        case BooleanValue(v) => Doc.text(v.toString)
        case ListValue(v) =>
          Doc.intercalate(Doc.comma + Doc.line, v.map(renderValue)).tightBracketBy(Doc.char('['), Doc.char(']'))
        case ObjectValue(fields) =>
          Doc
            .intercalate(
              Doc.comma + Doc.line,
              fields.map { case (k, v) => Doc.text(k) + Doc.text(": ") + renderValue(v) }
            )
            .bracketBy(Doc.char('{'), Doc.char('}'))
        case EnumValue(v)     => Doc.text(v)
        case VariableValue(v) => Doc.text(s"$$${v}")
      }
    }

    def renderVar(v: Var.One[?]): Doc = {
      val default = v.default match {
        case None          => Doc.empty
        case Some(default) => Doc.space + Doc.char('=') + Doc.space + renderValue(default)
      }
      Doc.text(s"$$${v.name}") + Doc.space + Doc.char(':') + Doc.space + Doc.text(v.tpe) + default
    }

    def renderArg(a: Arg): Doc =
      Doc.text(a.name) + Doc.char(':') + Doc.space + renderValue(a.value)

    def renderSelectionSet(ss: SelectionSet[?]): Doc = {
      type C[A] = Const[List[Doc], A]
      val compiler = new (Selection ~> C) {
        override def apply[A](fa: Selection[A]): C[A] = Const[List[Doc], A] {
          List {
            fa match {
              // Fragments are floated to the top level and handled separately
              case _: Fragment[?] => Doc.empty
              case InlineFragment(on, _, subSelection) =>
                Doc.text(s"...$on") + Doc.space + renderSelectionSet(subSelection)
              case Selection.Field(name, alias, args, sq) =>
                val aliased = alias match {
                  case None    => Doc.empty
                  case Some(a) => Doc.text(a) + Doc.char(':') + Doc.space
                }

                val lhs = aliased + Doc.text(name)

                val argsDoc = args.toNel match {
                  case None => Doc.empty
                  case Some(args) =>
                    Doc.intercalate(Doc.comma + Doc.line, args.map(renderArg).toList).bracketBy(Doc.char('('), Doc.char(')'))
                }

                def renderSubQuery(sq: SubQuery[?]): Doc = sq match {
                  case Terminal(_)              => Doc.empty
                  case ss: SelectionSet[?]      => Doc.space + renderSelectionSet(ss)
                  case ListModifier(subQuery)   => renderSubQuery(subQuery)
                  case OptionModifier(subQuery) => renderSubQuery(subQuery)
                }
                val rhs = renderSubQuery(sq)

                lhs + argsDoc + rhs
            }
          }
        }
      }
      val docs = ss.impl.underlying.foldMap(compiler)
      Doc.intercalate(Doc.comma + Doc.line, Doc.text("__typename") :: docs.getConst).bracketBy(Doc.char('{'), Doc.char('}'))
    }

    def renderFragment(frag: Fragment[?]): Doc =
      Doc.text(frag.name) + Doc.space + Doc.text("on") +
        Doc.space + Doc.text(frag.on) + Doc.space +
        renderSelectionSet(frag.subSelection)
  }
}
