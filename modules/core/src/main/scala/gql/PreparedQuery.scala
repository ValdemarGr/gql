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
import gql.parser.QueryParser

object PreparedQuery {
  sealed trait Prepared[F[_], A]

  sealed trait PreparedField[F[_], A]

  final case class PreparedDataField[F[_], I, T](
      id: Int,
      name: String,
      alias: Option[String],
      cont: PreparedCont[F]
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

  final case class EdgeId(id: Int) extends AnyVal

  final case class PreparedEdge[F[_]](
      id: EdgeId,
      resolver: Resolver[F, Any, Any],
      statisticsName: String
  )

  final case class PreparedCont[F[_]](
      edges: NonEmptyChain[PreparedEdge[F]],
      cont: Prepared[F, Any]
  )

  final case class Selection[F[_], A](fields: NonEmptyList[PreparedField[F, A]]) extends Prepared[F, A]

  final case class PreparedList[F[_], A](of: Prepared[F, A]) extends Prepared[F, A]

  final case class PreparedOption[F[_], A](of: Prepared[F, A]) extends Prepared[F, A]

  final case class PreparedLeaf[F[_], A](name: String, encode: A => Json) extends Prepared[F, A]

  final case class PositionalError(position: PrepCursor, caret: List[Caret], message: String) {
    lazy val asGraphQL: JsonObject = {
      import io.circe.syntax._
      Map(
        "message" -> Some(message.asJson),
        "locations" -> caret.map(c => Json.obj("line" -> c.line.asJson, "column" -> c.col.asJson)).toNel.map(_.asJson),
        "path" -> NonEmptyChain.fromChain(position.position.map(_.name)).map(_.asJson)
      ).collect { case (k, Some(v)) => k -> v }.asJsonObject
    }
  }

  sealed trait PrepEdge {
    def name: String
  }
  object PrepEdge {
    final case class ASTEdge(edge: SchemaShape.ValidationEdge) extends PrepEdge {
      def name: String = edge.name
    }
    final case class FragmentEdge(name: String) extends PrepEdge
  }

  final case class PrepCursor(position: Chain[PrepEdge]) {
    def add(edge: PrepEdge): PrepCursor = PrepCursor(position append edge)
    def pop: PrepCursor = PrepCursor(Chain.fromOption(position.initLast).flatMap { case (xs, _) => xs })
  }

  object PrepCursor {
    val empty: PrepCursor = PrepCursor(Chain.empty)
  }

  final case class Prep(
      cycleSet: Set[String],
      nextId: Int,
      cursor: PrepCursor
  )

  object Prep {
    val empty: Prep = Prep(Set.empty, 1, PrepCursor.empty)
  }

  def flattenResolvers[F[_]: Monad, G[_]](parentName: String, resolver: Resolver[G, Any, Any])(implicit
      S: Stateful[F, Prep]
  ): F[(NonEmptyChain[PreparedEdge[G]], String)] =
    resolver match {
      case r @ BatchResolver(id, run) =>
        nextId[F].map(nid => (NonEmptyChain.of(PreparedEdge(EdgeId(nid), resolver, s"batch_${id.id}")), parentName))
      case r @ EffectResolver(_) =>
        val thisName = s"${parentName}_effect"
        nextId[F].map(nid => (NonEmptyChain.of(PreparedEdge(EdgeId(nid), resolver, thisName)), thisName))
      case r @ StreamResolver(_) =>
        val thisName = s"${parentName}_stream"
        nextId[F].map(nid => (NonEmptyChain.of(PreparedEdge(EdgeId(nid), resolver, thisName)), thisName))
      case r @ CompositionResolver(left, right) =>
        flattenResolvers[F, G](parentName, left).flatMap { case (ys, newParentName) =>
          flattenResolvers[F, G](newParentName, right).map { case (zs, outName) => (ys ++ zs, outName) }
        }
    }

  type M[A] = State[Prep, ValidatedNec[PositionalError, A]]

  object M {
    def pure[A](a: A): M[A] = State.pure(a.validNec)

    def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B] = {
      fa.flatMap {
        case Validated.Invalid(e) => State.pure(Validated.Invalid(e))
        case Validated.Valid(a)   => f(a)
      }
    }
  }

  def underlyingOutputTypename[G[_]](ot: Out[G, _]): String = ot match {
    case Enum(name, _, _)         => name
    case Union(name, _, _)        => name
    case Interface(name, _, _, _) => name
    case Type(name, _, _)         => name
    case Scalar(name, _, _, _)    => name
    case OutOpt(of)               => underlyingOutputTypename(of)
    case OutArr(of)               => underlyingOutputTypename(of)
  }

  def friendlyName[G[_], A](ot: Out[G, A]): String = ot match {
    case Scalar(name, _, _, _)    => name
    case Enum(name, _, _)         => name
    case Type(name, _, _)         => name
    case Union(name, _, _)        => name
    case Interface(name, _, _, _) => name
    case OutOpt(of)               => s"(${friendlyName(of)} | null)"
    case OutArr(of)               => s"[${friendlyName(of)}]"
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

  def ambientEdge[F[_]: Monad, A](edge: PrepEdge)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    S.inspect(_.cursor).flatMap { c =>
      S.modify(_.copy(cursor = c.add(edge))) *> fa <* S.modify(_.copy(cursor = c))
    }

  def ambientField[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.Field(name)))(fa)

  def ambientOutputType[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.OutputType(name)))(fa)

  def ambientArg[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.Arg(name)))(fa)

  def ambientInputType[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.ASTEdge(SchemaShape.ValidationEdge.InputType(name)))(fa)

  def ambientFragment[F[_]: Monad, A](name: String)(fa: F[A])(implicit S: Stateful[F, Prep]): F[A] =
    ambientEdge[F, A](PrepEdge.FragmentEdge(name))(fa)

  def prepareSelections[F[_], G[_]](
      ol: Selectable[G, Any],
      s: P.SelectionSet,
      variableMap: VariableMap[Any],
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
        case Type(name, _, _)          => Chain((inst.specify, name))
        case Union(_, types, _)        => Chain.fromSeq(types.toList).flatMap(collectLeafPrisms)
        case Interface(_, types, _, _) => Chain.fromSeq(types).flatMap(collectLeafPrisms)
      }

    val allPrisms: Chain[(Any => Option[Any], String)] = collectLeafPrisms(Instance(Eval.now(ol))(Some(_)))

    val syntheticTypename =
      Field[G, Any, String, Unit](
        Applicative[Arg].unit,
        EffectResolver[G, (Any, Unit), String] { case (input, _) =>
          val x = allPrisms.collectFirstSome { case (p, name) => p(input).as(name) }
          G.pure(x.toRightIor("typename could not be determined, this is an implementation error"))
        },
        Eval.now(gql.ast.stringScalar)
      )

    val schemaMap = ol.fieldMap + ("__typename" -> syntheticTypename)
    s.selections.traverse[F, PreparedField[G, Any]] {
      case Pos(caret, P.Selection.FieldSelection(field)) =>
        schemaMap.get(field.name) match {
          case None => raise(s"unknown field name ${field.name}", Some(caret))
          case Some(f: Field[G, Any, Any, Any]) =>
            ambientField(field.name) {
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
        fragments.get(f.fragmentName) match {
          case None => raise(s"unknown fragment name ${f.fragmentName}", Some(caret))
          case Some(fd) =>
            ambientFragment(f.fragmentName) {
              prepareFragment[F, G](ol, fd, variableMap, fragments, fd.value.typeCnd)
                .flatMap[PreparedField[G, Any]] { fd =>
                  nextId[F].map(id => PreparedFragField(id, fd.typeCondition, fd.specify, Selection(fd.fields)))
                }
            }
        }
    }
  }

  type VariableMap[A] = Map[String, (In[A], A)]

  def closeFieldParameters[F[_], G[_]](
      gqlField: P.Field,
      caret: Caret,
      field: Field[G, Any, Any, Any],
      variableMap: VariableMap[Any]
  )(implicit
      S: Stateful[F, Prep],
      F: MonadError[F, PositionalError],
      D: Defer[F]
  ): F[Resolver[G, Any, Any]] = {
    val provided = gqlField.arguments.toList.flatMap(_.nel.toList)
    val providedMap = provided.map(x => x.name -> x.value).toMap

    val Field(args, resolve, graphqlType, _) = field

    // Treat input arguments as an object
    // Decode the args as-if an input
    val argObj =
      P.Value.ObjectValue(provided.map(a => a.name -> a.value))

    val decObj = args match {
      case PureArg(value) if provided.isEmpty => F.pure(value)
      case PureArg(_) =>
        raise(s"field ${gqlField.name} does not accept arguments", Some(caret))
      case nea @ NonEmptyArg(_, _) =>
        parseInputObj[F, Any](argObj, nea, Some(variableMap), ambigiousEnum = false)
    }

    decObj.map(a => resolve.contramap[Any]((_, a)))
  }

  def prepareField[F[_], G[_]: Applicative](
      gqlField: P.Field,
      caret: Caret,
      field: Field[G, Any, Any, Any],
      variableMap: VariableMap[Any],
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
            F.pure(PreparedLeaf(s.name, x => s.encoder(x).asJson))
          case (o, Some(_)) => raise(s"type ${friendlyName[G, Any](o)} cannot have selections", Some(selCaret))
          case (o, None)    => raise(s"object like type ${friendlyName[G, Any](o)} must have a selection", Some(selCaret))
        }

      val prepF: F[Prepared[G, Any]] = typePrep(tpe)

      prepF.flatMap { p =>
        nextId[F].flatMap { id =>
          flattenResolvers[F, G](s"${currentTypename}_${gqlField.name}", resolve).map { case (edges, _) =>
            val pc = PreparedCont(edges, p)
            PreparedDataField(id, gqlField.name, gqlField.alias, pc)
          }
        }
      }
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
        case Type(n, _, _) =>
          raise(s"tried to match with type $name on type object type $n", Some(caret))
        case i @ Interface(n, instances, fields, _) =>
          // TODO follow sub-interfaces that occur in `instances`
          raiseOpt(
            i.instanceMap
              .get(name)
              .map(i => (i.ol.value, i.specify)),
            s"$name does not implement interface $n, possible implementations are ${i.instanceMap.keySet.mkString(", ")}",
            caret.some
          )
        case u @ Union(n, types, _) =>
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
      variableMap: VariableMap[Any],
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

  def pValueName(v: P.Value): String =
    v match {
      case ObjectValue(_)   => "object"
      case StringValue(_)   => "string"
      case ListValue(_)     => "list"
      case EnumValue(_)     => "enum"
      case BooleanValue(_)  => "boolean"
      case NullValue        => "null"
      case FloatValue(_)    => "float"
      case IntValue(_)      => "int"
      case VariableValue(_) => "variable"
    }

  def inName(in: In[_]): String = in match {
    case InArr(of)             => s"list of ${inName(of)}"
    case Enum(name, _, _)      => name
    case Scalar(name, _, _, _) => name
    case InOpt(of)             => s"optional of ${inName(of)}"
    case Input(name, _, _)     => name
  }

  def parseInputObj[F[_], A](
      v: P.Value.ObjectValue,
      fields: NonEmptyArg[A],
      variableMap: Option[VariableMap[A]],
      ambigiousEnum: Boolean
  )(implicit
      F: MonadError[F, PositionalError],
      S: Stateful[F, Prep]
  ): F[A] = {
    val xs = v.v

    val m = xs.toMap
    val required = fields.nec.map(x => x.name -> x).toList.toMap

    // All provided fields are defined
    val tooMuch = m.keySet -- required.keySet
    val tooMuchF =
      if (tooMuch.isEmpty) F.unit
      else raise[F, Unit](s"too many fields provided, unknown fields are ${tooMuch.toList.mkString_(", ")}", None)

    tooMuchF >> parseArg[F, A](fields, m, variableMap, ambigiousEnum)
  }

  def parseInput[F[_], A](v: P.Value, tpe: In[A], variableMap: Option[VariableMap[A]], ambigiousEnum: Boolean)(implicit
      F: MonadError[F, PositionalError],
      S: Stateful[F, Prep]
  ): F[A] =
    (tpe, v) match {
      case (_, P.Value.VariableValue(v)) =>
        variableMap match {
          case None => raise(s"variable $v may not occur here", None)
          case Some(vm) =>
            vm.get(v) match {
              case None => raise(s"variable $v is not defined", None)
              case Some((varType, a)) =>
                def cmpTpe[A](lhs: In[_], rhs: In[_]): F[Unit] =
                  (lhs, rhs) match {
                    case (InArr(expected), InArr(other)) => cmpTpe(expected, other)
                    case (Enum(name, _, _), Enum(otherName, _, _)) =>
                      if (name == otherName) F.unit
                      else raise(s"expected enum $name for variable $v, but got enum $otherName", None)
                    case (Scalar(name, _, _, _), Scalar(otherName, _, _, _)) =>
                      if (name == otherName) F.unit
                      else raise(s"expected scalar $name for variable $v, but got scalar $otherName", None)
                    case (InOpt(expected), InOpt(other)) => cmpTpe(expected, other)
                    case (Input(name, _, _), Input(otherName, _, _)) =>
                      if (name == otherName) F.unit
                      else raise(s"expected input $name for variable $v, but got input $otherName", None)
                    case _ =>
                      raise(s"expected ${inName(lhs)} type for variable $v, but got ${inName(rhs)}", None)
                  }

                cmpTpe(tpe, varType).as(a)
            }
        }
      case (e @ Enum(name, mappings, _), v) =>
        ambientInputType(name) {
          val fa: F[String] = v match {
            case P.Value.EnumValue(s)                    => F.pure(s)
            case P.Value.StringValue(s) if ambigiousEnum => F.pure(s)
            case _ =>
              raise(s"enum value expected for $name, but got ${pValueName(v)}", None)
          }
          fa.flatMap { s =>
            e.m.lookup(s) match {
              case Some(x) => F.pure(x)
              case None =>
                val names = e.m.keys.toList
                raise(
                  s"enum value $s does not occur in enum type $name, possible enum values are ${names.mkString_(", ")}",
                  None
                )
            }
          }
        }
      case (Scalar(name, _, decoder, _), x) =>
        ambientInputType(name) {
          parserValueToValue[F](x).flatMap(x => raiseEither(decoder(x), None))
        }
      case (Input(name, fields, _), o: P.Value.ObjectValue) =>
        ambientInputType(name) {
          parseInputObj[F, A](o, fields, variableMap, ambigiousEnum)
        }
      case (InArr(of), P.Value.ListValue(xs)) =>
        xs.traverse(parseInput[F, A](_, of, variableMap, ambigiousEnum)).map(_.toSeq.asInstanceOf[A])
      case (InOpt(of), P.Value.NullValue) => F.pure(None.asInstanceOf[A])
      case (InOpt(of), x)                 => parseInput[F, A](x, of, variableMap, ambigiousEnum).map(Some(_).asInstanceOf[A])
      case (i, _)                         => raise(s"expected ${inName(i)} type, but got ${pValueName(v)}", None)
    }

  def defaultToValue(default: DefaultValue[_]): Value = {
    import DefaultValue._
    default match {
      case Arr(values)       => Value.ArrayValue(values.toVector.map(defaultToValue))
      case DefaultValue.Null => Value.NullValue
      case Primitive(value, in) =>
        in match {
          case e @ Enum(_, _, _)    => Value.EnumValue(e.revm(value))
          case Scalar(_, enc, _, _) => enc(value)
        }
      case Obj(fields) => Value.ObjectValue(fields.toList.toMap.view.mapValues(defaultToValue).toMap)
    }
  }

  def parseArgValue[F[_], A](a: ArgValue[A], input: Map[String, P.Value], variableMap: Option[VariableMap[A]], ambigiousEnum: Boolean)(
      implicit
      F: MonadError[F, PositionalError],
      S: Stateful[F, Prep]
  ) = {
    val fa =
      input.get(a.name) match {
        case None =>
          a.defaultValue match {
            case None =>
              a.input.value match {
                case InOpt(_) => F.pure(P.Value.NullValue)
                case _ =>
                  raise[F, P.Value](s"required input ${a.name} was not provided and has no default value", None)
              }
            // TODO this value being parsed can probably be cached, since the default is the same for every query
            case Some(dv) => F.pure(valueToParserValue(defaultToValue(dv)))
          }
        case Some(x) => F.pure(x)
      }

    ambientArg(a.name) {
      fa.flatMap(parseInput[F, A](_, a.input.value, variableMap, ambigiousEnum))
    }
  }

  def parseArg[F[_], A](arg: Arg[A], input: Map[String, P.Value], variableMap: Option[VariableMap[A]], ambigiousEnum: Boolean)(implicit
      F: MonadError[F, PositionalError],
      S: Stateful[F, Prep]
  ): F[A] = {
    // All provided fields are of the correct type
    // All required fields are either defiend or defaulted
    val fieldsF: F[Chain[(String, Any)]] =
      arg.entries.traverse { a =>
        parseArgValue[F, Any](
          a.asInstanceOf[ArgValue[Any]],
          input,
          variableMap.asInstanceOf[Option[VariableMap[Any]]],
          ambigiousEnum
        )
          .tupleLeft(a.name)
      }

    fieldsF.map(_.toList.toMap).map(arg.decode)
  }

  def parserValueToValue[F[_]](v: P.Value)(implicit
      F: MonadError[F, PositionalError],
      S: Stateful[F, Prep]
  ): F[Value] =
    v match {
      case NullValue     => F.pure(Value.NullValue)
      case FloatValue(v) => F.pure(Value.FloatValue(v))
      case EnumValue(v)  => F.pure(Value.EnumValue(v))
      case ListValue(v) =>
        v.toVector.traverse(parserValueToValue[F]).map(Value.ArrayValue(_))
      case IntValue(v)      => F.pure(Value.IntValue(v))
      case VariableValue(v) => raise[F, Value](s"variable $v may not occur here", None)
      case ObjectValue(v) =>
        v.traverse { case (k, v) =>
          parserValueToValue[F](v).tupleLeft(k)
        }.map(xs => Value.ObjectValue(xs.toMap))
      case BooleanValue(v) => F.pure(Value.BooleanValue(v))
      case StringValue(v)  => F.pure(Value.StringValue(v))
    }

  def valueToParserValue(v: Value): P.Value =
    v match {
      case Value.BooleanValue(v)     => P.Value.BooleanValue(v)
      case Value.StringValue(v)      => P.Value.StringValue(v)
      case Value.IntValue(v)         => P.Value.IntValue(v)
      case Value.ObjectValue(fields) => P.Value.ObjectValue(fields.toList.map { case (k, v) => k -> valueToParserValue(v) })
      case Value.ArrayValue(v)       => P.Value.ListValue(v.toList.map(valueToParserValue))
      case Value.EnumValue(v)        => P.Value.EnumValue(v)
      case Value.NullValue           => P.Value.NullValue
      case Value.FloatValue(v)       => P.Value.FloatValue(v)
    }

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

    val selF = op match {
      case P.OperationDefinition.Simple(sel) => F.pure((sel, Map.empty[String, (In[Any], Any)]))
      case P.OperationDefinition.Detailed(_, _, vdsO, _, sel) =>
        val varMapF =
          vdsO.toList
            .flatMap(_.nel.toList)
            .traverse { case Pos(caret, vd) =>
              def getTpe(p: P.Type, optional: Boolean = true): F[In[Any]] = {
                def opt(tpe: In[Any]): In[Any] =
                  if (optional) InOpt(tpe).asInstanceOf[In[Any]] else tpe

                p match {
                  case P.Type.Named(name) =>
                    raiseOpt(schema.shape.discover.inputs.get(name).asInstanceOf[Option[In[Any]]], s"type $name does not exist", None)
                      .map(opt)
                  case P.Type.List(of) =>
                    getTpe(of).map(InArr[Any, Seq](_).asInstanceOf[In[Any]]).map(opt)
                  case P.Type.NonNull(of) => getTpe(of, false)
                }
              }

              getTpe(vd.tpe).flatMap { tpe =>
                val resolvedInput =
                  (variableMap.get(vd.name), vd.defaultValue) match {
                    case (Some(j), _) =>
                      parseInput[F, Any](valueToParserValue(Value.fromJson(j)), tpe, None, ambigiousEnum = true)
                    case (None, Some(default)) =>
                      parseInput[F, Any](default, tpe, None, ambigiousEnum = false)
                    case (None, None) =>
                      tpe match {
                        case InOpt(_) => F.pure(None.asInstanceOf[Any])
                        case _ =>
                          raise[F, Any](s"Variable '$$${vd.name}' is required but was not provided.", Some(caret))
                      }
                  }

                resolvedInput.map(v => vd.name -> ((tpe, v)))
              }
            }
            .map(_.toMap)

        varMapF.tupleLeft(sel)
    }

    selF.flatMap { case (sel, vm) =>
      val fa = rootSchema.flatMap { root =>
        prepareSelections[F, G](
          root.asInstanceOf[Type[G, Any]],
          sel,
          vm,
          frags.map(f => f.value.name -> f).toMap,
          rootTypename
        )
      }

      fa.tupleLeft(ot)
    }
  }

  type H[A] = StateT[EitherT[Eval, PositionalError, *], Prep, A]

  def prepare[F[_]: Applicative](
      executabels: NonEmptyList[P.ExecutableDefinition],
      schema: Schema[F, _, _, _],
      variableMap: Map[String, Json],
      operationName: Option[String]
  ): Either[PositionalError, (P.OperationType, NonEmptyList[PreparedField[F, Any]])] = {
    val (ops, frags) = executabels.toList.partitionEither {
      case P.ExecutableDefinition.Operation(op)  => Left(op)
      case P.ExecutableDefinition.Fragment(frag) => Right(frag)
    }

    getOperationDefinition[Either[(String, List[Caret]), *]](ops, operationName) match {
      case Left((e, carets)) => Left(PositionalError(PrepCursor.empty, carets, e))
      case Right(op) =>
        prepareParts[H, F](op, frags, schema, variableMap)
          .runA(Prep.empty)
          .value
          .value
    }
  }
}
