package gql

import cats.effect.implicits._
import cats.implicits._
import cats.data._
import cats.effect._
import cats.mtl._
import cats.mtl.implicits._
import cats._
import io.circe._
import gql.GQLParser.Value.BooleanValue
import gql.GQLParser.Value.VariableValue
import gql.GQLParser.Value.FloatValue
import gql.GQLParser.Value.IntValue
import gql.GQLParser.Value.EnumValue
import gql.GQLParser.Value.StringValue
import gql.GQLParser.Value.ObjectValue
import gql.GQLParser.Value.NullValue
import gql.GQLParser.Value.ListValue

object PreparedQuery {
  /*
   * the query subset of the schema, with values closed in
   * lets us easily reason with query complexity, planning, re-execution and not bother with bad argument states
   *
   * `
   *   # "SubSelection" with type Fragment with fields that originate from D
   *   fragment SubSelection on D {
   *     dd # "dd" with type DataField with inner type Scalar
   *   }
   *
   *   fragment DataSelection on Data { # "DataSelection" with type Fragment with fields that originate from Data
   *     c { # "c" with type DataField and inner type Selection
   *       ... on C1 { "C1" with type InlineSpread which points to C1
   *         __typename # "__typename" with type DataField with inner type Scalar
   *       }
   *       ... on C2 { "C2" with type InlineSpread which points to C2
   *         __typename # "__typename" with type DataField with inner type Scalar
   *       }
   *     }
   *     d { # "d" with type DataField with inner type Selection
   *       ... SubSelection # "SubSelection" with type FragmentSpread which points to SubSelection
   *     }
   *   }
   *
   *   query {
   *     data { # "data" with type DataField and inner type Selection
   *       a # "a" with type DataField with inner type Scalar
   *       b { # "b" with type DataField with inner type Selection
   *         ba # "ba" with type DataField with inner type Scalar
   *       }
   *       ... DataSelection # "DataSelection" with type FragmentSpread which points to DataSelection
   *     }
   *   }
   * `
   *
   */
  sealed trait Prepared[F[_], A]

  sealed trait PreparedField[F[_], A]

  final case class PreparedDataField[F[_], I, T](
      name: String,
      resolve: I => Resolution[F, T],
      selection: Prepared[F, T]
  ) extends PreparedField[F, I]

  final case class FragmentDefinition[F[_], A](
      name: String,
      fields: NonEmptyList[PreparedField[F, A]]
  ) extends Prepared[F, A]

  final case class PreparedFragmentReference[F[_], A](
      reference: FragmentDefinition[F, A]
  ) extends PreparedField[F, A]

  final case class Selection[F[_], A](
      name: String,
      fields: NonEmptyList[PreparedField[F, A]]
  ) extends Prepared[F, A]

  final case class PreparedList[F[_], A](of: Prepared[F, A]) extends Prepared[F, List[A]]

  final case class PreparedScalarType[F[_], A](x: GQLOutputScalarType[F, A]) extends AnyVal

  /*
   * To prepare the query we must:
   * 1. Parse the query
   * 2. Typecheck the query
   * 3. Close input parameters into resolve functions
   *
   * this function will perform 2. and 3. and expect 1. as input.
   */
  def prepare[F[_]](query: NonEmptyList[GQLParser.ExecutableDefinition]): F[Unit] = ???

  final case class AnalysisState[F[_]](
      fragments: Map[String, FragmentDefinition[F, _]]
  )

  final case class Schema[F[_]](
      queries: GQLOutputObjectType[F, Unit],
      mutations: GQLOutputObjectType[F, Unit],
      subscriptions: GQLOutputObjectType[F, Unit],
      types: Map[String, GQLToplevelOutputType[F]]
  )

  def valueName(value: GQLParser.Value): String = value match {
    case ObjectValue(_)   => "object"
    case EnumValue(_)     => "enum"
    case StringValue(_)   => "string"
    case IntValue(_)      => "int"
    case BooleanValue(_)  => "boolean"
    case VariableValue(_) => "variable"
    case ListValue(_)     => "list"
    case FloatValue(_)    => "float"
    case NullValue        => "null"
  }

  def transformValueToJsonRepr(value: GQLParser.Value, variableMap: Map[String, Json]): Either[String, Json] = {
    def go(value: GQLParser.Value): Eval[Either[String, Json]] =
      value match {
        case VariableValue(v) =>
          Eval.now(Either.fromOption(variableMap.get(v), s"unable to find variable $v"))
        case FloatValue(v) => Eval.now(Right(Json.fromJsonNumber(v)))
        case ObjectValue(v) =>
          Eval
            .defer {
              v.toList.traverse { case (k, v) =>
                go(value).map(_.map(k -> _))
              }
            }
            .map(xs => xs.sequence[Either[String, *], (String, Json)].map(ys => Json.fromJsonObject(JsonObject.fromIterable(ys))))
        case ListValue(v)    => Eval.defer(v.traverse(go).map(_.sequence.map(Json.fromValues)))
        case EnumValue(v)    => Eval.now(Right(Json.fromString(v)))
        case StringValue(v)  => Eval.now(Right(Json.fromString(v)))
        case BooleanValue(v) => Eval.now(Right(Json.fromBoolean(v)))
        case IntValue(v)     => Eval.now(Right(Json.fromBigInt(v)))
        case NullValue       => Eval.now(Right(Json.Null))
      }

    go(value).value
  }

  def prepareSelections[F[_]](s: GQLParser.SelectionSet, schema: NonEmptyList[(String, GQLField[F, _, _])])(implicit
      S: Stateful[F, AnalysisState[F]],
      F: MonadError[F, String]
  ) = {
    val schemaMap = schema.toNem
    s.selections.traverse[F, Unit] {
      case GQLParser.Selection.FieldSelection(field) =>
        schemaMap.lookup(field.name) match {
          case None => F.raiseError(s"unknown field name ${field.name}")
          case Some(f) =>
            val checkArgsF =
              (f, field.arguments) match {
                case (GQLSimpleField(_, _), Some(_)) =>
                  F.raiseError(s"field ${field.name} has arguments, but none were expected")
                case (GQLSimpleField(_, graphqlType), None) => F.unit
                case (GQLArgField(args, _, _), None) =>
                  F.raiseError(s"no arguments provided for ${field.name}, expected ${args.entries.size}")
                case (GQLArgField(args, _, _), Some(provided)) =>
                  /*
                   * We need to verify a couple of things about arguments, and in the proper order:
                   * 1. There are no provided arguments that are not expected.
                   * 2. All expected arguments are either provided or have a default value.
                   * 3. All provided arguments have the correct type.
                   */
                  val expectedMap = args.entries.toNem
                  provided.nel.traverse[F, Unit] { p =>
                    expectedMap.lookup(p.name) match {
                      case None =>
                        F.raiseError(
                          s"unexpected argument ${p.name} in field ${field.name}, expected arguments are ${expectedMap.keys.mkString_(", ")}"
                        )
                      case Some(expectedType) =>
                        F.unit
                    }
                  }
                  ???
              }

            F.unit
        }
      case GQLParser.Selection.FragmentSpreadSelection(field) => F.unit
      case GQLParser.Selection.InlineFragmentSelection(field) => F.unit
    }
  }

  def prepareFragment[F[_]](f: GQLParser.FragmentDefinition, schema: Schema[F])(implicit
      S: Stateful[F, AnalysisState[F]],
      F: MonadError[F, String]
  ): F[Unit] =
    S.get.flatMap {
      case state if state.fragments.contains(f.name) => F.raiseError(s"fragment by name ${f.name} defined more than once")
      case state =>
        schema.types.get(f.typeCnd) match {
          case None => F.raiseError(s"fragment ${f.name} references unknown type ${f.typeCnd}")
          case Some(x) =>
            x match {
              case GQLOutputScalarType(name, _) =>
                F.raiseError(s"fragment ${f.name} references scalar type $name, but scalars are not allowed in fragments")
              case GQLOutputUnionType(name, xs) =>
                F.raiseError(s"fragment ${f.name} references union type $name, but unions are not allowed in fragments")
              case GQLOutputObjectType(name, fields) =>
                prepareSelections(f.selectionSet, fields).attempt.flatMap {
                  case Left(err) => F.raiseError(s"in fragment ${f.name}: $err")
                  case Right(x)  => F.pure(x)
                }
            }
        }
    }
}
