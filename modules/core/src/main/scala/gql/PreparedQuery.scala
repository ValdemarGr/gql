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
import gql.ast._
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
      typename: String,
      alias: Option[String],
      parentTypename: String
  ) extends PreparedField[F, I]

  final case class PreparedFragField[F[_], A](
      id: Int,
      typename: String,
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

  // final case class PreparedList2[F[_]](
  //     of: Prepared[F, Any],
  //     resolver: Resolver[F, Any, Any]
  // ) extends Prepared[F, Any]

  final case class PreparedOption[F[_], A](of: Prepared[F, A]) extends Prepared[F, A]

  final case class PreparedLeaf[F[_], A](name: String, encode: A => Json) extends Prepared[F, A]

  final case class PositionalError(position: PrepCursor, caret: List[Caret], message: String) {
    lazy val asGraphQL: JsonObject = {
      import io.circe.syntax._
      Map(
        "message" -> Some(message.asJson),
        "locations" -> caret.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
        "path" -> position.position.toNel.map(_.asJson)
      ).collect { case (k, Some(v)) => k -> v }.asJsonObject
    }
  }

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

  // TODO use validated
  def decodeInput[A](in: ast.In[A], value: Value): Either[String, A] =
    in match {
      case ast.InOpt(inner) =>
        if (value.asJson.isNull) Right(None)
        else decodeInput(inner, value).map(Some(_))
      case ast.InArr(inner) =>
        value match {
          case gql.Value.JsonValue(ja) if ja.isArray =>
            ja.asArray.get.traverse(j => decodeInput(inner, Value.JsonValue(j)))
          case gql.Value.ArrayValue(v) => v.traverse(decodeInput(inner, _))
          case _                       => Left(s"expected array type, get ${value.name}")
        }
      case ast.Scalar(_, codec) =>
        codec.decodeJson(value.asJson).leftMap(_.show)
      case e @ ast.Enum(name, mappings) =>
        def decodeString(s: String): Either[String, A] =
          e.m.lookup(s) match {
            case Some(a) => Right(a)
            case None    => Left(s"unknown value $s for enum $name")
          }

        value match {
          case gql.Value.JsonValue(v) if v.isString => decodeString(v.asString.get)
          case gql.Value.EnumValue(s)               => decodeString(s)
          case _                                    => Left(s"expected enum $name, got ${value.name}")
        }
      // TODO unify this and arg decoding
      // Look into free applicatives
      case ast.Input(name, fields) =>
        value match {
          case gql.Value.JsonValue(jo) if jo.isObject =>
            val m = jo.asObject.get.toMap

            fields.entries
              .traverse { a =>
                m
                  .get(a.name)
                  .map(x => decodeInput(a.input, gql.Value.JsonValue(x))) match {
                  case Some(outcome) => outcome
                  case None          => a.default.toRight(s"missing field ${a.name} in input object $name")
                }
              }
              .map(_.toList)
              .map { xs =>
                val (_, o) = fields.decode(xs.asInstanceOf[List[Any]])
                o
              }
          case gql.Value.ObjectValue(xs) =>
            fields.entries
              .traverse { a =>
                xs
                  .get(a.name)
                  .map(decodeInput(a.input, _)) match {
                  case Some(outcome) => outcome
                  case None          => a.default.toRight(s"missing field ${a.name} in input object $name")
                }
              }
              .map(_.toList)
              .map { xs =>
                val (_, o) = fields.decode(xs.asInstanceOf[List[Any]])
                o
              }
          case _ => Left(s"expected object for $name, got ${value.name}")
        }
    }

  def underlyingOutputTypename[G[_]](ot: Out[G, _]): String = ot match {
    case Enum(name, _)         => name
    case Union(name, _)        => name
    case Interface(name, _, _) => name
    case Type(name, _)         => name
    case Scalar(name, _)       => name
    case OutOpt(of)            => underlyingOutputTypename(of)
    case OutArr(of)            => underlyingOutputTypename(of)
  }

  def friendlyName[G[_], A](ot: Out[G, A]): String = ot match {
    case Scalar(name, _)       => name
    case Enum(name, _)         => name
    case Type(name, _)         => name
    case Union(name, _)        => name
    case Interface(name, _, _) => name
    case OutOpt(of)            => s"(${friendlyName(of)} | null)"
    case OutArr(of)            => s"[${friendlyName(of)}]"
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
    S.get.map(state => PositionalError(state.cursor, caret.toList, s)).flatMap(F.raiseError[A])

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

  def prepareSelections[F[_], G[_]](
      ol: Selectable[G, Any],
      s: P.SelectionSet,
      variableMap: Map[String, Json],
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String
  )(implicit
      G: Applicative[G],
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[NonEmptyList[PreparedField[G, Any]]] = D.defer {
    // TODO this code shares much with the subtype interfaces below in matchType
    def collectLeafPrisms(inst: Instance[G, Any, Any]): Chain[(Any => Option[Any], String)] =
      inst.ol.value match {
        case Type(name, _)          => Chain((inst.specify, name))
        case Union(_, types)        => Chain.fromSeq(types.toList).flatMap(collectLeafPrisms)
        case Interface(_, types, _) => Chain.fromSeq(types).flatMap(collectLeafPrisms)
      }

    val allPrisms: Chain[(Any => Option[Any], String)] = collectLeafPrisms(Instance(Eval.now(ol))(Some(_)))

    val syntheticTypename =
      Field[G, Any, String, Unit](
        Applicative[Arg].unit,
        EffectResolver[G, (Any, Unit), String] { case (input, _) =>
          val x = allPrisms.collectFirstSome { case (p, name) => p(input).as(name) }
          G.pure(x.toRightIor("typename could not be determined, this is an implementation error"))
        },
        Eval.now(Scalar("String", Codec.from(Decoder.decodeString, Encoder.encodeString)))
      )

    val schemaMap = ol.fieldMap + ("__typename" -> syntheticTypename)
    s.selections.traverse[F, PreparedField[G, Any]] {
      case Pos(caret, P.Selection.FieldSelection(field)) =>
        ambientPath(field.name) {
          schemaMap.get(field.name) match {
            case None => raise(s"unknown field name ${field.name}", Some(caret))
            case Some(f: Field[G, Any, Any, Any]) =>
              prepareField[F, G](field, caret, f, variableMap, fragments, currentTypename)
          }
        }
      case Pos(caret, P.Selection.InlineFragmentSelection(f)) =>
        f.typeCondition match {
          case None => raise(s"inline fragment must have a type condition", Some(caret))
          case Some(typeCnd) =>
            matchType[F, G](typeCnd, ol, caret).flatMap { case (ol, specialize) =>
              prepareSelections[F, G](ol, f.selectionSet, variableMap, fragments, typeCnd)
                .map(Selection(_))
                .flatMap[PreparedField[G, Any]](s => nextId[F].map(id => PreparedFragField(id, typeCnd, specialize, s)))
            }
        }
      case Pos(caret, P.Selection.FragmentSpreadSelection(f)) =>
        ambientPath(f.fragmentName) {
          fragments.get(f.fragmentName) match {
            case None => raise(s"unknown fragment name ${f.fragmentName}", Some(caret))
            case Some(fd) =>
              prepareFragment[F, G](ol, fd, variableMap, fragments, fd.value.typeCnd)
                .flatMap[PreparedField[G, Any]] { fd =>
                  nextId[F].map(id => PreparedFragField(id, fd.typeCondition, fd.specify, Selection(fd.fields)))
                }
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
                .flatMap(j => raiseEither[F, Any](decodeInput(arg.input, j), Some(caret)))
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
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[PreparedField[G, Any]] = {
    closeFieldParameters[F, G](gqlField, caret, field, variableMap).flatMap { resolve =>
      val tpe = field.output.value
      val ss = gqlField.selectionSet.value
      val selCaret = gqlField.selectionSet.caret

      val tn = underlyingOutputTypename(field.output.value)

      def typePrep(t: Out[G, Any]): F[Prepared[G, Any]] =
        (t, ss) match {
          case (OutArr(inner), _) => typePrep(inner).map(PreparedList(_))
          case (OutOpt(inner), _) => typePrep(inner).map(PreparedOption(_))
          case (ol: Selectable[G, Any], Some(ss)) =>
            prepareSelections[F, G](ol, ss, variableMap, fragments, tn)
              .map(Selection(_))
          case (e: Enum[G, Any], None) =>
            F.pure(PreparedLeaf(e.name, x => Json.fromString(e.revm(x))))
          case (s: Scalar[G, Any], None) =>
            F.pure(PreparedLeaf(s.name, (s.codec: Encoder[Any]).apply))
          case (o, Some(_)) => raise(s"type ${friendlyName[G, Any](o)} cannot have selections", Some(selCaret))
          case (o, None)    => raise(s"object like type ${friendlyName[G, Any](o)} must have a selection", Some(selCaret))
        }

      val prepF: F[Prepared[G, Any]] = typePrep(tpe)

      prepF.flatMap(p =>
        nextId[F].map { id =>
          PreparedDataField(id, gqlField.name, resolve, p, tn, gqlField.alias, currentTypename)
        }
      )
    }
  }

  // name is the type in the pattern match case
  // sel is the type we match on
  // sel match { case x if x.name == name  => ... }
  def matchType[F[_], G[_]: Applicative](
      name: String,
      sel: Selectable[G, Any],
      caret: Caret
  )(implicit F: MonadError[F, PositionalError], S: Stateful[F, Prep]): F[(Selectable[G, Any], Any => Option[Any])] =
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
        case Type(n, _) =>
          raise(s"tried to match with type $name on type object type $n", Some(caret))
        case i @ Interface(n, instances, fields) =>
          // TODO follow sub-interfaces that occur in `instances`
          raiseOpt(
            i.instanceMap
              .get(name)
              .map(i => (i.ol.value, i.specify)),
            s"$name does not implement interface $n, possible implementations are ${i.instanceMap.keySet.mkString(", ")}",
            caret.some
          )
        case u @ Union(n, types) =>
          raiseOpt(
            u.instanceMap
              .get(name)
              .map(i => (i.ol.value, i.specify)),
            s"$name is not a member of the union $n, possible members are ${u.instanceMap.keySet.mkString(", ")}",
            caret.some
          )
      }
    }

  def prepareFragment[F[_], G[_]: Applicative](
      ol: Selectable[G, Any],
      f: Pos[P.FragmentDefinition],
      variableMap: Map[String, Json],
      fragments: Map[String, Pos[P.FragmentDefinition]],
      currentTypename: String
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
                prepareSelections[F, G](t, f.value.selectionSet, variableMap, fragments, currentTypename)
                  .map(FragmentDefinition(f.value.name, f.value.typeCnd, specify, _))
              }

          beforeF *> programF <* afterF
      }
    }

  // TODO positions for errors
  def getOperationDefinition[F[_]](
      ops: List[Pos[P.OperationDefinition]],
      operationName: Option[String]
  )(implicit F: MonadError[F, (String, List[Caret])]): F[P.OperationDefinition] =
    (ops, operationName) match {
      case (Nil, _)      => F.raiseError((s"no operations provided", Nil))
      case (x :: Nil, _) => F.pure(x.value)
      case (xs, _) if xs.exists {
            case Pos(_, _: P.OperationDefinition.Simple) => true
            case _                                       => false
          } =>
        F.raiseError((s"exactly one operation must be suplied for shorthand queries", xs.map(_.caret)))
      case (xs, None) if xs.size > 1 =>
        F.raiseError((s"operation name must be supplied for multiple operations"), xs.map(_.caret))
      case (xs, Some(name)) =>
        val o = xs.collectFirst { case Pos(_, d: P.OperationDefinition.Detailed) if d.name.contains(name) => d }
        F.fromOption(o, (s"unable to find operation $name", xs.map(_.caret)))
    }

  def operationType(od: P.OperationDefinition) =
    od match {
      case P.OperationDefinition.Simple(_)                => P.OperationType.Query
      case P.OperationDefinition.Detailed(ot, _, _, _, _) => ot
    }

  // TODO add another phase after finding the OperationDefinition and before this,
  // that checks all that variables have been used
  def prepareParts[F[_], G[_]: Applicative](
      op: P.OperationDefinition,
      frags: List[Pos[P.FragmentDefinition]],
      schema: Schema[G, _, _, _],
      variableMap: Map[String, Json]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[(P.OperationType, NonEmptyList[PreparedField[G, Any]])] = {
    val ot = operationType(op)

    val rootSchema: F[Type[G, _]] =
      ot match {
        case P.OperationType.Query        => raiseOpt(schema.shape.query, "no query defined in schema", None)
        case P.OperationType.Mutation     => raiseOpt(schema.shape.mutation, "no mutation defined in schema", None)
        case P.OperationType.Subscription => raiseOpt(schema.shape.subscription, "no subscription defined in schema", None)
      }

    val rootTypename =
      ot match {
        case P.OperationType.Query        => "Query"
        case P.OperationType.Mutation     => "Mutation"
        case P.OperationType.Subscription => "Subscription"
      }

    val sel = op match {
      case P.OperationDefinition.Simple(sel)                  => sel
      case P.OperationDefinition.Detailed(_, _, vdsO, _, sel) => sel
      // TODO check variables and fill defaults
      // Also embed type information into the variable map that must be checked against the parameters in the schema
      // Json can be removed from the variables map since it will be decoded up-front
      // Map[String, (Value, Type)]
      // Typechecking must occur at the usage of this map also, or else, we implement structural typing
      // vdsO match {
      //   case None => Map.empty
      //   case Some(vars) =>
      //     val vds = vars.nel
      //     vds.traverse{ vd =>
      //     }
      //     vds.map(_.defaultValue)
      // }
      // (variableMap, sel)
    }

    val fa = rootSchema.flatMap { root =>
      prepareSelections[F, G](
        root.asInstanceOf[Type[G, Any]],
        sel,
        variableMap,
        frags.map(f => f.value.name -> f).toMap,
        rootTypename
      )
    }

    fa.tupleLeft(ot)
  }

  def prepare[F[_]: Applicative](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, _, _, _],
      variableMap: Map[String, Json]
  ): Either[PositionalError, NonEmptyList[PreparedField[F, Any]]] = {
    val (ops, frags) =
      executabels.toList.partitionEither {
        case P.ExecutableDefinition.Operation(op)  => Left(op)
        case P.ExecutableDefinition.Fragment(frag) => Right(frag)
      }

    type G[A] = StateT[EitherT[Eval, PositionalError, *], Prep, A]

    getOperationDefinition[Either[(String, List[Caret]), *]](ops, None) match {
      case Left((e, carets)) => Left(PositionalError(PrepCursor.empty, carets, e))
      case Right(op) =>
        prepareParts[G, F](op, frags, schema, variableMap)
          .map { case (_, x) => x }
          .runA(Prep(Set.empty, 1, PrepCursor.empty))
          .value
          .value
    }
  }

  def prepare2[F[_]: Applicative](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, _, _, _],
      variableMap: Map[String, Json]
  ): Either[PositionalError, (P.OperationType, NonEmptyList[PreparedField[F, Any]])] = {
    val (ops, frags) = executabels.toList.partitionEither {
      case P.ExecutableDefinition.Operation(op)  => Left(op)
      case P.ExecutableDefinition.Fragment(frag) => Right(frag)
    }

    type G[A] = StateT[EitherT[Eval, PositionalError, *], Prep, A]

    getOperationDefinition[Either[(String, List[Caret]), *]](ops, None) match {
      case Left((e, carets)) => Left(PositionalError(PrepCursor.empty, carets, e))
      case Right(op) =>
        prepareParts[G, F](op, frags, schema, variableMap)
          .runA(Prep(Set.empty, 1, PrepCursor.empty))
          .value
          .value
    }
  }
}
