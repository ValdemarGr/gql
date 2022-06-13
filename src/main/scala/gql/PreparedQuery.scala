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
import gql.GQLParser.OperationDefinition.Detailed
import gql.GQLParser.OperationDefinition.Simple

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

  final case class Selection[F[_], A](fields: NonEmptyList[PreparedField[F, A]]) extends Prepared[F, A]

  final case class PreparedList[F[_], A](of: Prepared[F, A]) extends Prepared[F, List[A]]

  final case class PreparedLeaf[F[_], A](name: String, encode: Encoder[A]) extends Prepared[F, A]

  final case class AnalysisState[F[_]](
      cachedFragments: Map[String, FragmentDefinition[F, _]],
      cycleSet: Set[String]
  )

  final case class Schema[F[_], Q, M, S](
      queries: GQLOutputObjectType[F, Q],
      mutations: GQLOutputObjectType[F, M],
      subscriptions: GQLOutputObjectType[F, S],
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

  def prepareSelections[F[_], G[_]](
      s: GQLParser.SelectionSet,
      schema: NonEmptyList[(String, GQLField[G, _, _])],
      variableMap: Map[String, Json]
  )(implicit F: MonadError[F, String], D: Defer[F]): F[Prepared[G, Any]] = D.defer {
    val schemaMap = schema.toNem
    s.selections
      .traverse[F, PreparedField[G, Any]] {
        case GQLParser.Selection.FieldSelection(field) =>
          schemaMap.lookup(field.name) match {
            case None    => F.raiseError(s"unknown field name ${field.name}")
            case Some(f) =>
              // unify parameterized and non-prameterized fields by closing in parameters
              val closedProgram: F[(Any => Resolution[G, Any], GQLOutputType[G, Any])] =
                (f, field.arguments) match {
                  case (GQLSimpleField(_, _), Some(_)) =>
                    F.raiseError(s"field ${field.name} has arguments, but none were expected")
                  case (GQLSimpleField(resolve, graphqlType), None) =>
                    F.pure((resolve.asInstanceOf[Any => Resolution[G, Any]], graphqlType.value))
                  case (GQLArgField(args, _, _), None) =>
                    F.raiseError(s"no arguments provided for ${field.name}, expected ${args.entries.size}")
                  case (GQLArgField(args, resolve, graphqlType), Some(provided)) =>
                    val providedMap = provided.nel.toList.map(x => x.name -> x.value).toMap
                    val argResolution =
                      args.entries
                        .traverse { arg =>
                          val res =
                            providedMap
                              .get(arg.name) match {
                              case None    => arg.default.toRight(s"missing argument ${arg.name}")
                              case Some(x) => transformValueToJsonRepr(x, variableMap).flatMap(j => arg.tpe.decode(j))
                            }

                          res.map(arg.name -> _)
                        }
                        .map(_.toList.toMap)

                    F.fromEither(argResolution)
                      .map(args.decode)
                      .map { resolvedArg =>
                        ((x: Any) => resolve.asInstanceOf[Any => Resolution[G, Any]](x, resolvedArg), graphqlType.value)
                      }
                }

              // before we can finish this field, we need to prepare sub-selections
              closedProgram.flatMap { case (resolve, tpe) =>
                val prepF: F[Prepared[G, Any]] =
                  (tpe, field.selectionSet) match {
                    case (GQLOutputObjectType(_, fields), Some(ss)) =>
                      prepareSelections[F, G](ss, fields, variableMap)
                    case (GQLOutputObjectType(name, _), None) =>
                      F.raiseError(s"object type $name had no selections")
                    case (GQLOutputListType(GQLOutputObjectType(name, fields)), Some(ss)) =>
                      prepareSelections[F, G](ss, fields, variableMap)
                    case (GQLOutputListType(GQLOutputObjectType(name, fields)), None) =>
                      F.raiseError(s"object type $name in list had no selections")
                    case (GQLEnumType(name, encode, _), None) =>
                      F.pure(PreparedLeaf(name, (x: Any) => Json.fromString(encode(x))))
                    case (GQLEnumType(name, _, _), Some(_)) =>
                      F.raiseError(s"enum type $name cannot have selections")
                    case (GQLOutputScalarType(name, encoder), None) =>
                      F.pure(PreparedLeaf(name, (x: Any) => encoder(x)))
                    case (GQLOutputScalarType(name, _), Some(_)) =>
                      F.raiseError(s"scalar type $name cannot have selections")
                    case _ => ???
                  }

                prepF.map { p =>
                  PreparedDataField(
                    field.name,
                    resolve,
                    p
                  )
                }
              }
          }
        case GQLParser.Selection.FragmentSpreadSelection(field) => ???
        case GQLParser.Selection.InlineFragmentSelection(field) => ???
      }
      .map(Selection(_))
  }

  def prepareFragment[F[_], G[_]](f: GQLParser.FragmentDefinition, schema: Schema[G, _, _, _], variableMap: Map[String, Json])(implicit
      S: Stateful[F, AnalysisState[G]],
      F: MonadError[F, String],
      D: Defer[F]
  ): F[Prepared[G, Any]] =
    D.defer {
      S.get.flatMap {
        case state if state.cycleSet.contains(f.name) =>
          F.raiseError(s"fragment by name ${f.name} is cyclic, discovered through path ${state.cycleSet.mkString(" -> ")}")
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
                  prepareSelections[F, G](f.selectionSet, fields, variableMap).attempt.flatMap {
                    case Left(err) => F.raiseError(s"in fragment ${f.name}: $err")
                    case Right(x)  => F.pure(x)
                  }
              }
          }
      }
    }

  final case class PreparedSchema[F[_], Q, M, S](
      query: Prepared[F, Q],
      mutation: Option[Prepared[F, M]],
      subscription: Option[Prepared[F, S]]
  )

  // mapping from variable name to value and type
  final case class VariableMap(value: Map[String, (String, GQLParser.Type)]) extends AnyVal

  def prepare[F[_], Q, M, S](
      executabels: NonEmptyList[GQLParser.ExecutableDefinition],
      schema: Schema[F, Q, M, S],
      variableMap: Map[String, Json]
  ): Either[String, PreparedSchema[F, Q, M, S]] = {
    val (ops, frags) =
      executabels.toList.partitionEither {
        case GQLParser.ExecutableDefinition.Operation(op)  => Left(op)
        case GQLParser.ExecutableDefinition.Fragment(frag) => Right(frag)
      }

    // blabla check args first
    ops.map {
      case Simple(_)                                            => ???
      case Detailed(tpe, name, None, _, _)                      => ???
      case Detailed(tpe, name, Some(variableDefinitions), _, _) => ???
    }

    type G[A] = StateT[EitherT[Eval, String, *], AnalysisState[F], A]

    val o = prepareFragment[G, F](null, schema, variableMap)

    o
      .runA(AnalysisState(Map.empty, Set.empty))
      .value
      .value

    ???
  }
}
