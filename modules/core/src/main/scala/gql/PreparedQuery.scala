package gql

import cats.effect.implicits._
import cats.implicits._
import cats.data._
import cats.effect._
import cats.mtl._
import cats.mtl.implicits._
import cats._
import io.circe._
import gql.parser.{QueryParser => P, Pos}
import P.Value._
import gql.out._
import gql.resolver._
import cats.parse.Caret

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

  final case class PreparedOption[F[_], A](of: Prepared[F, A]) extends Prepared[F, A]

  final case class PreparedLeaf[F[_], A](name: String, encode: A => Json) extends Prepared[F, A]

  final case class PositionalError(position: PrepCursor, caret: Option[Caret], message: String)

  final case class PrepCursor(position: List[String]) {
    def add(name: String): PrepCursor = PrepCursor(name :: position)
    def pop: PrepCursor = PrepCursor(position.tail)
  }

  object PrepCursor {
    val empty: PrepCursor = PrepCursor(Nil)
  }

  final case class Prep(
      cycleSet: Set[String],
      nextId: Int,
      cursor: PrepCursor
  )

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

  def parserValueToValue[F[_]](value: P.Value, variableMap: Map[String, Json], caret: Caret)(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[Value] = {
    def go(x: P.Value): F[Value] =
      x match {
        case IntValue(v) => F.pure(Value.IntValue(v))
        case ObjectValue(v) =>
          D.defer {
            v.traverse { case (k, v) =>
              go(v).map(k -> _)
            }.map(xs => Value.ObjectValue(xs.toMap))
          }
        case ListValue(v) =>
          D.defer(v.toVector.traverse(go).map(Value.ArrayValue(_)))
        case EnumValue(v) => F.pure(Value.EnumValue(v))
        case VariableValue(v) =>
          raiseOpt(variableMap.get(v).map(Value.JsonValue(_)), s"variable $v not found", Some(caret))
        case NullValue       => F.pure(Value.NullValue)
        case BooleanValue(v) => F.pure(Value.BooleanValue(v))
        case FloatValue(v) =>
          raiseOpt(v.toBigDecimal.map(Value.FloatValue(_)), s"$v is not a vaild json number", Some(caret))
        case StringValue(v) => F.pure(Value.StringValue(v))
      }

    go(value)
  }

  def nextId[F[_]: Monad](implicit S: Stateful[F, Prep]) =
    S.inspect(_.nextId) <* S.modify(x => x.copy(nextId = x.nextId + 1))

  def raise[F[_], A](s: String, caret: Option[Caret])(implicit S: Stateful[F, Prep], F: MonadError[F, PositionalError]): F[A] =
    S.get.map(state => PositionalError(state.cursor, caret, s)).flatMap(F.raiseError[A])

  def raiseOpt[F[_], A](o: Option[A], s: String, caret: Option[Caret])(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError]
  ): F[A] =
    o.map(_.pure[F]).getOrElse(raise[F, A](s, caret))

  def raiseEither[F[_], A](e: Either[String, A], caret: Option[Caret])(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError]
  ): F[A] =
    e match {
      case Left(value)  => raise[F, A](value, caret)
      case Right(value) => F.pure(value)
    }

  def ambientPath[F[_]: Monad, A](path: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    S.inspect(_.cursor).flatMap { c =>
      S.modify(_.copy(cursor = c.add(path))) *> fa <* S.modify(_.copy(cursor = c))
    }

  def prepareSelections[F[_], G[_]: Applicative](
      ol: ObjLike[G, Any],
      s: P.SelectionSet,
      variableMap: Map[String, Json],
      fragments: Map[String, Pos[P.FragmentDefinition]]
  )(implicit S: Stateful[F, Prep], F: MonadError[F, PositionalError], D: Defer[F]): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
    // TODO this code shares much with the subtype interfaces below in matchType
    def collectLeafPrisms(inst: Instance[G, Any, Any]): Chain[(SimplePrism[Any, Any], String)] =
      inst.ol match {
        case Obj(name, _)           => Chain((inst.specify, name))
        case Union(_, types)        => Chain.fromIterableOnce(types.toIterable).flatMap(collectLeafPrisms)
        case Interface(_, types, _) => Chain.fromIterableOnce(types.values).flatMap(collectLeafPrisms)
      }

    val allPrisms: Chain[(SimplePrism[Any, Any], String)] = collectLeafPrisms(Instance(ol)(Some(_)))

    val syntheticTypename =
      Field[G, Any, String, Unit](
        Applicative[Arg].unit,
        EffectResolver[G, (Any, Unit), String] { case (input, _) =>
          val x = allPrisms.collectFirstSome { case (p, name) => p(input).as(name) }
          IorT.fromOption[G](x, "typename could not be determined, this is an implementation error")
        },
        Eval.now(Scalar("String", io.circe.Encoder.encodeString))
      )

    val schemaMap = ol.fieldMap + ("__typename" -> syntheticTypename)
    s.selections.traverse[F, PreparedField[G, Any]] {
      case Pos(caret, P.Selection.FieldSelection(field)) =>
        ambientPath(field.name) {
          schemaMap.get(field.name) match {
            case None                             => raise(s"unknown field name ${field.name}", Some(caret))
            case Some(f: Field[G, Any, Any, Any]) => prepareField[F, G](field, caret, f, variableMap, fragments)
          }
        }
      case Pos(caret, P.Selection.InlineFragmentSelection(f)) =>
        f.typeCondition match {
          case None => raise(s"inline fragment must have a type condition", Some(caret))
          case Some(typeCnd) =>
            matchType[F, G](typeCnd, ol, caret).flatMap { case (ol, specialize) =>
              prepareSelections[F, G](ol, f.selectionSet, variableMap, fragments)
                .map(Selection(_))
                .flatMap[PreparedField[G, Any]](s => nextId[F].map(id => PreparedFragField(id, specialize, s)))
            }
        }
      case Pos(caret, P.Selection.FragmentSpreadSelection(f)) =>
        ambientPath(f.fragmentName) {
          fragments.get(f.fragmentName) match {
            case None => raise(s"unknown fragment name ${f.fragmentName}", Some(caret))
            case Some(fd) =>
              prepareFragment[F, G](ol, fd, variableMap, fragments)
                .flatMap[PreparedField[G, Any]](fd => nextId[F].map(id => PreparedFragField(id, fd.specify, Selection(fd.fields))))
          }
        }
    }
  }

  def closeFieldParameters[F[_], G[_]](
      gqlField: P.Field,
      caret: Caret,
      field: Field[G, Any, Any, Any],
      variableMap: Map[String, Json]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[Resolver[G, Any, Any]] = {
    val provided = gqlField.arguments.toList.flatMap(_.nel.toList)
    val providedMap = provided.map(x => x.name -> x.value).toMap

    val Field(args, resolve, graphqlType) = field

    val argResolution =
      args.entries
        .traverse { arg =>
          providedMap
            .get(arg.name) match {
            case None => raiseOpt[F, Any](arg.default, s"missing argument ${arg.name}", Some(caret))
            case Some(x) =>
              parserValueToValue(x, variableMap, caret)
                .flatMap { j =>
                  raiseEither[F, Any](
                    arg.input
                      .decode(j),
                    Some(caret)
                  )
                }
          }
        }
        .map(_.toList)

    argResolution.map { x =>
      val (_, resolvedArg) = args.decode(x)
      val closed = resolve.contramap[Any]((_, resolvedArg))
      closed
    }
  }

  def prepareField[F[_], G[_]: Applicative](
      gqlField: P.Field,
      caret: Caret,
      field: Field[G, Any, Any, Any],
      variableMap: Map[String, Json],
      fragments: Map[String, Pos[P.FragmentDefinition]]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[PreparedField[G, Any]] = {
    closeFieldParameters[F, G](gqlField, caret, field, variableMap).flatMap { resolve =>
      val tpe = field.output.value
      val ss = gqlField.selectionSet.value
      val selCaret = gqlField.selectionSet.caret

      def typePrep(t: Output[G, Any]): F[Prepared[G, Any]] =
        (t, ss) match {
          case (Arr(inner), _) => typePrep(inner).map(PreparedList(_))
          case (Opt(inner), _) => typePrep(inner).map(PreparedOption(_))
          case (ol: ObjLike[G, Any], Some(ss)) =>
            prepareSelections[F, G](ol, ss, variableMap, fragments)
              .map(Selection(_))
          case (e: Enum[G, Any], None) =>
            F.pure(PreparedLeaf(e.name, x => Json.fromString(e.encoder(x).get)))
          case (s: Scalar[G, Any], None) =>
            F.pure(PreparedLeaf(s.name, s.encoder.apply))
          case (o, Some(_)) => raise(s"type ${friendlyName[G, Any](o)} cannot have selections", Some(selCaret))
          case (o, None)    => raise(s"object like type ${friendlyName[G, Any](o)} must have a selection", Some(selCaret))
        }

      val prepF: F[Prepared[G, Any]] = typePrep(tpe)

      prepF.flatMap(p =>
        nextId[F].map(id => PreparedDataField(id, gqlField.name, resolve, p, underlyingOutputTypename(field.output.value)))
      )
    }
  }

  // name is the type in the pattern match case
  // sel is the type we match on
  // sel match { case x if x.name == name  => ... }
  def matchType[F[_], G[_]: Applicative](
      name: String,
      sel: ObjLike[G, Any],
      caret: Caret
  )(implicit F: MonadError[F, PositionalError], S: Stateful[F, Prep]): F[(ObjLike[G, Any], Any => Option[Any])] =
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
        case Obj(n, _)                       => raise(s"tried to match with type $name on type object type $n", Some(caret))
        case Interface(n, instances, fields) =>
          // TODO follow sub-interfaces that occur in `instances`
          raiseOpt(
            instances
              .get(name)
              .map(i => (i.ol, i.specify)),
            s"$name does not implement interface $n, possible implementations are ${instances.keySet.mkString(", ")}",
            caret.some
          )
        case Union(n, types) =>
          raiseOpt(
            types
              .lookup(name)
              .map(i => (i.ol, i.specify)),
            s"$name is not a member of the union $n, possible members are ${types.keys.mkString_(", ")}",
            caret.some
          )
      }
    }

  def prepareFragment[F[_], G[_]: Applicative](
      ol: ObjLike[G, Any],
      f: Pos[P.FragmentDefinition],
      variableMap: Map[String, Json],
      fragments: Map[String, Pos[P.FragmentDefinition]]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[FragmentDefinition[G, Any]] =
    D.defer {
      S.get.flatMap {
        case c if c.cycleSet(f.value.name) => raise(s"fragment by name ${f.value.name} is cyclic", Some(f.caret))
        case _ =>
          val beforeF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet + f.value.name))
          val afterF: F[Unit] = S.modify(s => s.copy(cycleSet = s.cycleSet - f.value.name))

          val programF: F[FragmentDefinition[G, Any]] =
            matchType[F, G](f.value.typeCnd, ol, f.caret)
              .flatMap { case (t, specify) =>
                prepareSelections[F, G](t, f.value.selectionSet, variableMap, fragments)
                  .map(FragmentDefinition(f.value.name, f.value.typeCnd, specify, _))
              }

          beforeF *> programF <* afterF
      }
    }

  def prepareParts[F[_], G[_]: Applicative, Q](
      ops: List[P.OperationDefinition],
      frags: List[Pos[P.FragmentDefinition]],
      schema: Schema[G, Q],
      variableMap: Map[String, Json],
      operationName: Option[String] = None
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ) = {
    val op: F[P.OperationDefinition] =
      (ops, operationName) match {
        case (Nil, _)      => raise(s"no operations provided", None)
        case (x :: Nil, _) => F.pure(x)
        case (xs, _) if xs.exists {
              case _: P.OperationDefinition.Simple => true
              case _                               => false
            } =>
          raise(s"exactly one operation must be suplied for shorthand queries", None)
        case (x :: y :: _, None) =>
          raise(s"operation name must be supplied for multiple operations", None)
        case (xs, Some(name)) =>
          raiseOpt(
            xs.collectFirst { case d: P.OperationDefinition.Detailed if d.name.contains(name) => d },
            s"unable to find operation $name",
            None
          )
      }

    op.flatMap {
      case P.OperationDefinition.Simple(sel) =>
        prepareSelections[F, G](
          schema.shape.query.asInstanceOf[ObjLike[G, Any]],
          sel,
          variableMap,
          frags.map(f => f.value.name -> f).toMap
        )
      case P.OperationDefinition.Detailed(ot, _, vdsO, _, sel) =>
        vdsO match {
          case None => variableMap
          case Some(vars) =>
            val vds = vars.nel
            // vds.traverse{ vd =>
            //   ???
            // }
            vds.map(_.defaultValue)
        }
        // TODO decode variables
        // TODO handle other things than query
        ot match {
          case P.OperationType.Query =>
            prepareSelections[F, G](
              schema.shape.query.asInstanceOf[ObjLike[G, Any]],
              sel,
              variableMap,
              frags.map(f => f.value.name -> f).toMap
            )
        }
    }
  }

  def prepare[F[_]: Applicative, Q](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, Q],
      variableMap: Map[String, Json]
  ): Either[PositionalError, NonEmptyList[PreparedField[F, Any]]] = {
    val (ops, frags) =
      executabels.toList.partitionEither {
        case P.ExecutableDefinition.Operation(op)  => Left(op)
        case P.ExecutableDefinition.Fragment(frag) => Right(frag)
      }

    type G[A] = StateT[EitherT[Eval, PositionalError, *], Prep, A]

    val o = prepareParts[G, F, Q](ops, frags, schema, variableMap)

    o
      .runA(Prep(Set.empty, 1, PrepCursor.empty))
      .value
      .value
  }

  sealed trait OperationType[F[_]]
  object OperationType {
    final case class Query[F[_]](rootFields: NonEmptyList[PreparedField[F, Any]]) extends OperationType[F]
    final case class Mutation[F[_]](rootFields: NonEmptyList[PreparedField[F, Any]]) extends OperationType[F]
    final case class Subscription[F[_]](
        dataStream: Any => fs2.Stream[F, Any],
        root: PreparedField[F, Any]
    ) extends OperationType[F]
  }

  def prepare2[F[_], Q, M, S](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, Q],
      variableMap: Map[String, Json]
  ): Either[String, OperationType[F]] = ???
}
