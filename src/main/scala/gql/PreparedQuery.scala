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
import gql.out._
import gql.resolver._

object PreparedQuery {
  sealed trait Prepared[F[_], A]

  sealed trait PreparedField[F[_], A]

  final case class PreparedDataField[F[_], I, T](
      id: Int,
      name: String,
      resolve: Resolver[F, I, T],
      selection: Prepared[F, T],
      typename: String
  ) extends PreparedField[F, I]

  final case class PreparedFragField[F[_], A](
      id: Int,
      specify: Any => Option[A],
      selection: Selection[F, A]
  ) extends PreparedField[F, A]

  final case class FragmentDefinition[F[_], A](
      name: String,
      typeCondition: String,
      specify: Any => Option[A],
      fields: NonEmptyList[PreparedField[F, A]]
  )

  final case class Selection[F[_], A](fields: NonEmptyList[PreparedField[F, A]]) extends Prepared[F, A]

  final case class PreparedList[F[_], A](of: Prepared[F, A]) extends Prepared[F, A]
  final case class PreparedList2[F[_], A](of: Prepared[F, A]) extends Prepared[F, List[A]]

  final case class PreparedLeaf[F[_], A](name: String, encode: A => Either[String, Json]) extends Prepared[F, A]

  final case class AnalysisState(cycleSet: Set[String], nextId: Int)

  def underlyingOutputTypename[G[_]](ot: Output[G, Any]): String = ot match {
    case Enum(name, _)         => name
    case Union(name, _)        => name
    case Interface(name, _, _) => name
    case Obj(name, _)          => name
    case Scalar(name, _)       => name
    case Opt(of)               => underlyingOutputTypename(of)
    case Arr(of)               => underlyingOutputTypename(of)
  }

  def friendlyName[G[_], A](ot: Output[G, A]): String = ot match {
    case Scalar(name, _)       => name
    case Enum(name, _)         => name
    case Obj(name, _)          => name
    case Union(name, _)        => name
    case Interface(name, _, _) => name
    case x: Opt[G, _]          => s"(${friendlyName[G, Any](x.of.asInstanceOf[Output[G, Any]])} | null)"
    case x: Arr[G, _]          => s"[${friendlyName[G, Any](x.of.asInstanceOf[Output[G, Any]])}]"
  }

  def parserValueToValue(value: GQLParser.Value, variableMap: Map[String, Json]): Either[String, Value] = {
    def go[F[_]](x: GQLParser.Value)(implicit
        F: MonadError[F, String],
        D: Defer[F]
    ): F[Value] =
      x match {
        case IntValue(v) => F.pure(Value.IntValue(v))
        case ObjectValue(v) =>
          D.defer {
            v.traverse { case (k, v) =>
              go[F](v).map(k -> _)
            }.map(xs => Value.ObjectValue(xs.toMap))
          }
        case ListValue(v) =>
          D.defer(v.toVector.traverse(go[F]).map(Value.ArrayValue(_)))
        case EnumValue(v) => F.pure(Value.EnumValue(v))
        case VariableValue(v) =>
          F.fromOption(variableMap.get(v).map(Value.JsonValue(_)), s"variable $v not found")
        case NullValue       => F.pure(Value.NullValue)
        case BooleanValue(v) => F.pure(Value.BooleanValue(v))
        case FloatValue(v)   => F.fromOption(v.toBigDecimal.map(Value.FloatValue(_)), s"$v is not a vaild json number")
        case StringValue(v)  => F.pure(Value.StringValue(v))
      }

    go[EitherT[Eval, String, *]](value).value.value
  }

  def nextId[F[_]: Monad](implicit S: Stateful[F, AnalysisState]) =
    S.inspect(_.nextId) <* S.modify(x => x.copy(nextId = x.nextId + 1))

  def prepareSelections[F[_], G[_]](
      ol: ObjLike[G, Any],
      s: GQLParser.SelectionSet,
      variableMap: Map[String, Json],
      fragments: Map[String, GQLParser.FragmentDefinition]
  )(implicit S: Stateful[F, AnalysisState], F: MonadError[F, String], D: Defer[F]): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
    val schemaMap = ol.fieldMap
    s.selections.traverse[F, PreparedField[G, Any]] {
      case GQLParser.Selection.FieldSelection(field) =>
        schemaMap.get(field.name) match {
          case None                             => F.raiseError(s"unknown field name ${field.name}")
          case Some(f: Field[G, Any, Any, Any]) => prepareField[F, G](field, f, variableMap, fragments)
        }
      case GQLParser.Selection.InlineFragmentSelection(f) =>
        f.typeCondition match {
          case None => F.raiseError(s"inline fragment must have a type condition")
          case Some(typeCnd) =>
            matchType[F, G](typeCnd, ol).flatMap { case (ol, specialize) =>
              prepareSelections[F, G](ol, f.selectionSet, variableMap, fragments)
                .map(Selection(_))
                .flatMap[PreparedField[G, Any]](s => nextId[F].map(id => PreparedFragField(id, specialize, s)))
                .adaptError(e => s"in inline fragment with condition $typeCnd: $e")
            }
        }
      case GQLParser.Selection.FragmentSpreadSelection(f) =>
        fragments.get(f.fragmentName) match {
          case None => F.raiseError(s"unknown fragment name ${f.fragmentName}")
          case Some(fd) =>
            prepareFragment[F, G](ol, fd, variableMap, fragments)
              .flatMap[PreparedField[G, Any]](fd => nextId[F].map(id => PreparedFragField(id, fd.specify, Selection(fd.fields))))
              .adaptError(e => s"in fragment ${fd.name}: $e")
        }
    }
  }

  def closeFieldParameters[F[_], G[_]](
      gqlField: GQLParser.Field,
      field: Field[G, Any, Any, Any],
      variableMap: Map[String, Json]
  )(implicit
      S: Stateful[F, AnalysisState],
      F: MonadError[F, String],
      D: Defer[F]
  ): F[(Resolver[G, Any, Any], Output[G, Any])] =
    (field, gqlField.arguments.toList.flatMap(_.nel.toList)) match {
      case (Field(args, resolve, graphqlType), provided) =>
        val providedMap = provided.map(x => x.name -> x.value).toMap
        val argResolution =
          args.entries
            .traverse { arg =>
              providedMap
                .get(arg.name) match {
                case None    => arg.default.toRight(s"missing argument ${arg.name}")
                case Some(x) => parserValueToValue(x, variableMap).flatMap(j => arg.input.decode(j))
              }
            }
            .map(_.toList)

        F.fromEither(argResolution)
          .map(args.decode)
          .map { case (_, resolvedArg) =>
            val closed = resolve.contramap[Any]((_, resolvedArg))
            (closed, graphqlType.value)
          }
    }

  def prepareField[F[_], G[_]](
      gqlField: GQLParser.Field,
      field: Field[G, Any, Any, Any],
      variableMap: Map[String, Json],
      fragments: Map[String, GQLParser.FragmentDefinition]
  )(implicit
      S: Stateful[F, AnalysisState],
      F: MonadError[F, String],
      D: Defer[F]
  ): F[PreparedField[G, Any]] = {
    closeFieldParameters[F, G](gqlField, field, variableMap).flatMap { case (resolve, tpe) =>
      val prepF: F[Prepared[G, Any]] =
        (tpe, gqlField.selectionSet) match {
          case (ol: ObjLike[G, Any], Some(ss)) =>
            prepareSelections[F, G](ol, ss, variableMap, fragments)
              .map(Selection(_))
          case (Arr(ol: ObjLike[G, Any]), Some(ss)) =>
            prepareSelections[F, G](ol, ss, variableMap, fragments)
              .map(Selection(_))
              .map(x => PreparedList(x).asInstanceOf[Prepared[G, Any]])
          case (e: Enum[G, Any], None) =>
            F.pure(PreparedLeaf(e.name, x => Right(Json.fromString(e.encoder(x).get))))
          case (s: Scalar[G, Any], None) =>
            F.pure(PreparedLeaf(s.name, x => Right(s.encoder(x))))
          case (o, Some(_)) => F.raiseError(s"type ${friendlyName[G, Any](o)} cannot have selections")
          case (o, None)    => F.raiseError(s"object like type ${friendlyName[G, Any](o)} must have a selection")
        }

      prepF.flatMap(p =>
        nextId[F].map(id => PreparedDataField(id, gqlField.name, resolve, p, underlyingOutputTypename(field.output.value)))
      )
    }
  }

  // name is the type in the pattern match case
  // sel is the type we match on
  // sel match { case x if x.name == name  => ... }
  def matchType[F[_], G[_]](
      name: String,
      sel: ObjLike[G, Any]
  )(implicit F: MonadError[F, String]): F[(ObjLike[G, Any], Any => Option[Any])] =
    if (sel.name == name) F.pure((sel, Some(_)))
    else {
      /*
       TODO(also check supertypes, maybe the name is an interface that this selection implements)
       example:
         # schema
         interface A {
           x: Int!
         }

         type C implements A {
           x: Int!
           z: Boolean!
         }

         type B implements A {
           x: Int!
           y: String!
         }

         fragment AFrag on A {
           a
           ... on C {
             z
           }
           ... on B {
             y
           }
         }

         # query
         query {
           b: {
             ...AFrag
           }
         }

       We must be able to re-lift the gql type B to gql type A such that the fragment resolver for
       AFrag resolves matches on B and picks that implementation.
       */
      sel match {
        case Obj(n, _) => F.raiseError(s"tried to match with type $name on type object type $n")
        case Interface(n, instances, fields) =>
          F.fromOption(
            instances
              .get(name)
              .map(i => (i.ol, i.specify)),
            s"$name does not implement interface $n, possible implementations are ${instances.keySet.mkString(", ")}"
          )
        case Union(n, types) =>
          F.fromOption(
            types
              .lookup(name)
              .map(i => (i.ol, i.specify)),
            s"$name is not a member of the union $n, possible members are ${types.keys.mkString_(", ")}"
          )
      }
    }

  def prepareFragment[F[_], G[_]](
      ol: ObjLike[G, Any],
      f: GQLParser.FragmentDefinition,
      variableMap: Map[String, Json],
      fragments: Map[String, GQLParser.FragmentDefinition]
  )(implicit
      S: Stateful[F, AnalysisState],
      F: MonadError[F, String],
      D: Defer[F]
  ): F[FragmentDefinition[G, Any]] =
    D.defer {
      S.get.flatMap {
        case c if c.cycleSet(f.name) =>
          F.raiseError(s"fragment by name ${f.name} is cyclic, discovered through path ${c.cycleSet.mkString(" -> ")}")
        case _ =>
          val beforeF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet + f.name))
          val afterF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet - f.name))

          val programF: F[FragmentDefinition[G, Any]] =
            matchType[F, G](f.typeCnd, ol).flatMap { case (t, specialize) =>
              prepareSelections[F, G](t, f.selectionSet, variableMap, fragments).attempt.flatMap {
                case Left(err) => F.raiseError[FragmentDefinition[G, Any]](s"in fragment ${f.name}: $err")
                case Right(x)  => F.pure(FragmentDefinition(f.name, f.typeCnd, specialize, x))
              }
            }

          beforeF *> programF <* afterF
      }
    }

  def prepareParts[F[_], G[_], Q](
      ops: List[GQLParser.OperationDefinition],
      frags: List[GQLParser.FragmentDefinition],
      schema: Schema[G, Q],
      variableMap: Map[String, Json]
  )(implicit
      S: Stateful[F, AnalysisState],
      F: MonadError[F, String],
      D: Defer[F]
  ) = {
    // prepare all fragments
    // val prepped: F[Unit] = frags.traverse(frag => prepareFragment[F, G](null, frag, variableMap, null)).void

    /*prepped >>*/
    (ops.head match {
      //case Simple(_)                                            => ???
      //case Detailed(tpe, name, Some(variableDefinitions), _, _) => ???
      case Detailed(_, _, None, _, sel) =>
        prepareSelections[F, G](
          schema.shape.query.asInstanceOf[ObjLike[G, Any]],
          sel,
          variableMap,
          frags.map(f => f.name -> f).toMap
        )
    })
  }

  def prepare[F[_], Q](
      executabels: NonEmptyList[GQLParser.ExecutableDefinition],
      schema: Schema[F, Q],
      variableMap: Map[String, Json]
  ): Either[String, NonEmptyList[PreparedField[F, Any]]] = {
    val (ops, frags) =
      executabels.toList.partitionEither {
        case GQLParser.ExecutableDefinition.Operation(op)  => Left(op)
        case GQLParser.ExecutableDefinition.Fragment(frag) => Right(frag)
      }

    // blabla check args first
    // ops.map {
    //   case Simple(_)                                            => ???
    //   case Detailed(tpe, name, None, _, _)                      => ???
    //   case Detailed(tpe, name, Some(variableDefinitions), _, _) => ???
    // }

    type G[A] = StateT[EitherT[Eval, String, *], AnalysisState, A]

    val o = prepareParts[G, F, Q](ops, frags, schema, variableMap)

    o
      .runA(AnalysisState(Set.empty, 1))
      .value
      .value
  }

  sealed trait StaticOrStream[F[_]]
  object StaticOrStream {
    final case class Static[F[_]](rootFields: NonEmptyList[PreparedField[F, Any]]) extends StaticOrStream[F]
    final case class Stream[F[_]](
        dataStream: Any => fs2.Stream[F, Any],
        root: PreparedField[F, Any]
    ) extends StaticOrStream[F]
  }

  def prepare2[F[_], Q, M, S](
      executabels: NonEmptyList[GQLParser.ExecutableDefinition],
      schema: Schema[F, Q],
      variableMap: Map[String, Json]
  ): Either[String, StaticOrStream[F]] = ???
}
