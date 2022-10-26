package gql

import cats._
import cats.implicits._
import cats.mtl._
import cats.data._
import gql.ast._
import gql.parser.QueryParser
import org.typelevel.paiges.Doc

final case class SchemaShape[F[_], Q, M, S](
    query: Type[F, Q],
    mutation: Option[Type[F, M]] = Option.empty[Type[F, Unit]],
    subscription: Option[Type[F, S]] = Option.empty[Type[F, Unit]],
    outputTypes: List[OutToplevel[F, ?]] = Nil,
    inputTypes: List[InToplevel[?]] = Nil
) {
  def mapK[G[_]: Functor](fk: F ~> G): SchemaShape[G, Q, M, S] =
    SchemaShape(query.mapK(fk), mutation.map(_.mapK(fk)), subscription.map(_.mapK(fk)), outputTypes.map(_.mapK(fk)), inputTypes)

  def addOutputTypes(t: OutToplevel[F, ?]*): SchemaShape[F, Q, M, S] =
    copy(outputTypes = t.toList ++ outputTypes)

  def addInputTypes(t: InToplevel[?]*): SchemaShape[F, Q, M, S] =
    copy(inputTypes = t.toList ++ inputTypes)

  lazy val discover = SchemaShape.discover[F](this)

  lazy val validate = SchemaShape.validate[F](this)

  lazy val render = SchemaShape.render[F](this)

  lazy val introspection = SchemaShape.introspect[F](this)
}

object SchemaShape {
  final class PartiallyAppliedSchemaShape[F[_]](val dummy: Boolean = false) extends AnyVal {
    def apply[Q, M, S](
        query: Type[F, Q],
        mutation: Option[Type[F, M]] = Option.empty[Type[F, Unit]],
        subscription: Option[Type[F, S]] = Option.empty[Type[F, Unit]],
        outputTypes: List[OutToplevel[F, ?]] = Nil,
        inputTypes: List[InToplevel[?]] = Nil
    ): SchemaShape[F, Q, M, S] =
      SchemaShape(query, mutation, subscription, outputTypes, inputTypes)
  }

  def make[F[_]] = new PartiallyAppliedSchemaShape[F]

  final case class DiscoveryState[F[_]](
      inputs: Map[String, InToplevel[?]],
      outputs: Map[String, OutToplevel[F, ?]],
      // Key is the interface
      // Values:
      //   Key is the typename of the object
      //   Values:
      //     1. The object like type that extends the interface
      //     2. The function to map from the interface to the object)
      //
      // There is no implicit interface implementations; all transitive implementations should be explicit
      // (https://spec.graphql.org/draft/#IsValidImplementation())
      implementations: Map[String, Map[String, (ObjectLike[F, Any], Any => Option[Any])]]
  )

  def discover[F[_]](shape: SchemaShape[F, ?, ?, ?]): DiscoveryState[F] = {
    def inputNotSeen[G[_], A](
        tl: InToplevel[?]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.inputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(inputs = s.inputs + (tl.name -> tl))) *> ga
      }

    def outputNotSeen[G[_], A](
        tl: OutToplevel[F, ?]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.outputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(outputs = s.outputs + (tl.name -> tl))) *> ga
      }

    def goOutput[G[_]](out: Out[F, ?])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      out match {
        case OutArr(of, _) => goOutput[G](of.asInstanceOf[Out[F, Any]])
        case OutOpt(of)    => goOutput[G](of.asInstanceOf[Out[F, Any]])
        case t: OutToplevel[F, ?] =>
          outputNotSeen(t) {
            def handleFields(o: ObjectLike[F, ?]): G[Unit] =
              o.fieldsList.traverse_ { case (_, x) =>
                goOutput[G](x.output.value) >>
                  x.args.entries.traverse_(x => goInput[G](x.input.value.asInstanceOf[In[Any]]))
              }

            t match {
              case ol: ObjectLike[F, ?] =>
                val xs = ol.implementsMap.values.toList
                S.modify { s =>
                  val newImpls = xs.foldLeft(s.implementations) { case (accum, next) =>
                    val name = next.implementation.value.name
                    val thisEntry =
                      (ol.name -> ((ol, next.specify)).asInstanceOf[(gql.ast.ObjectLike[F, Any], Any => Option[Any])])
                    accum.get(name) match {
                      case None    => accum + (name -> Map(thisEntry))
                      case Some(m) => accum + (name -> (m + thisEntry))
                    }
                  }

                  s.copy(implementations = newImpls)
                } >>
                  handleFields(ol) >>
                  xs.traverse_(impl => goOutput[G](impl.implementation.value))
              case Union(_, instances, _) =>
                instances.toList.traverse_(inst => goOutput[G](inst.tpe.value))
              case _ => G.unit
            }
          }
      }

    def goInput[G[_]](inp: In[?])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      inp match {
        case InArr(of, _) => goInput[G](of)
        case InOpt(of)    => goInput[G](of)
        case t: InToplevel[?] =>
          inputNotSeen(t) {
            t match {
              case Input(_, fields, _) =>
                fields.entries.traverse_(x => goInput[G](x.input.value.asInstanceOf[In[Any]]))
              case _ => G.unit
            }
          }
      }

    val outs = (shape.query :: (shape.mutation ++ shape.subscription).toList ++ shape.outputTypes)
      .traverse_(goOutput[State[DiscoveryState[F], *]])

    val ins = shape.inputTypes.traverse_(goInput[State[DiscoveryState[F], *]])

    (ins, outs).tupled
      .runS(DiscoveryState(Map.empty, Map.empty, Map.empty))
      .value
  }

  sealed trait ValidationError {
    def message: String
  }
  object ValidationError {
    final case class DivergingTypeReference(typename: String) extends ValidationError {
      def message: String =
        s"`$typename` is not reference equal. Use lazy val or `cats.Eval` to declare this type."
    }
    final case class CyclicDivergingTypeReference(typename: String) extends ValidationError {
      def message: String =
        s"Cyclic type `$typename` is not reference equal. Use lazy val or `cats.Eval` to declare this type."
    }
    final case class InvalidTypeName(name: String) extends ValidationError {
      def message: String =
        s"Invalid type name '$name', the argument name must match /[_A-Za-z][_0-9A-Za-z]*/"
    }
    final case class InvalidFieldName(name: String) extends ValidationError {
      def message: String =
        s"Invalid field name '$name', the field name must match /[_A-Za-z][_0-9A-Za-z]*/"
    }
    final case class DuplicateArg(conflict: String) extends ValidationError {
      def message: String = s"Duplicate arg `$conflict`."
    }
    final case class DuplicateField(conflict: String) extends ValidationError {
      def message: String = s"Duplicate field `$conflict`."
    }
    final case class DuplicateUnionInstance(conflict: String) extends ValidationError {
      def message: String = s"Duplicate union instance `$conflict`."
    }
    final case class DuplicateInterfaceInstance(conflict: String) extends ValidationError {
      def message: String = s"Duplicate interface instance `$conflict`."
    }
    final case class InvalidInput(pe: PreparedQuery.PositionalError) extends ValidationError {
      def message: String = s"Invalid argument input: ${pe.message}."
    }
    final case class MissingInterfaceFields(
        typename: String,
        interfaceName: String,
        missing: List[(String, String)]
    ) extends ValidationError {
      def message: String = {
        val missingFields = missing.map { case (name, tpe) => s"$name: $tpe" }.mkString(", ")
        s"Type `$typename` does not implement all of the fields defined in interface `$interfaceName`, missing fields: $missingFields."
      }
    }
    final case class CyclicInterfaceImplementation(typename: String) extends ValidationError {
      def message: String = s"`$typename` is an interface implementation of itself."
    }
    final case class TransitiveInterfacesNotImplemented(typename: String, interfaces: Set[String]) extends ValidationError {
      def message: String =
        s"$typename does not implement all interfaces ${interfaces.map(i => s"`$i`").mkString(",")} of the transitive interfaces."
    }
    final case class WrongInterfaceFieldType(sourceInterface: String, fieldName: String, expected: String, actual: String)
        extends ValidationError {
      def message: String = s"Field $fieldName is of type `$actual` but expected `$expected` from interface $sourceInterface."
    }
  }

  sealed trait ValidationEdge {
    def name: String
  }
  object ValidationEdge {
    final case class Field(name: String) extends ValidationEdge
    final case class OutputType(name: String) extends ValidationEdge
    final case class Arg(name: String) extends ValidationEdge
    final case class InputType(name: String) extends ValidationEdge
  }

  final case class Problem(
      error: ValidationError,
      path: Chain[ValidationEdge]
  ) {
    override def toString() =
      s"${error.message} at ${path
        .map {
          case ValidationEdge.Field(name)      => s".$name"
          case ValidationEdge.OutputType(name) => s"($name)"
          case ValidationEdge.Arg(name)        => s".$name"
          case ValidationEdge.InputType(name)  => s"($name)"
        }
        .mkString_("")}"
  }
  // TODO has really bad running time on some inputs
  // since it doesn't remember what it has seen
  // Update: when #55 is fixed, this should be implicitly be fixed
  def validate[F[_]](schema: SchemaShape[F, ?, ?, ?]): Chain[Problem] = {
    final case class ValidationState(
        problems: Chain[Problem],
        currentPath: Chain[ValidationEdge],
        seenOutputs: Map[String, OutToplevel[F, ?]],
        seenInputs: Map[String, InToplevel[?]]
    )
    import ValidationError._

    def raise[G[_]](err: ValidationError, suffix: Chain[ValidationEdge] = Chain.empty)(implicit S: Stateful[G, ValidationState]): G[Unit] =
      S.modify(s => s.copy(problems = s.problems :+ Problem(err, s.currentPath ++ suffix)))

    def useEdge[G[_], A](edge: ValidationEdge)(
        fa: G[A]
    )(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[A] =
      S.get.flatMap { s =>
        S.set(s.copy(currentPath = s.currentPath :+ edge)) *>
          fa <*
          S.modify(_.copy(currentPath = s.currentPath))
      }

    def useOutputEdge[G[_]](ot: OutToplevel[F, ?])(
        fa: G[Unit]
    )(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      useEdge(ValidationEdge.OutputType(ot.name)) {
        S.get.flatMap { s =>
          s.seenOutputs.get(ot.name) match {
            case Some(o) if (o eq ot) => G.unit
            case Some(_)              => raise(CyclicDivergingTypeReference(ot.name))
            case None =>
              schema.discover.outputs
                .get(ot.name)
                .traverse_ {
                  case o if (o eq ot) => G.unit
                  case _              => raise(DivergingTypeReference(ot.name))
                } >>
                S.set(s.copy(seenOutputs = s.seenOutputs + (ot.name -> ot))) *>
                fa <*
                S.modify(_.copy(seenOutputs = s.seenOutputs))
          }
        }
      }

    def useInputEdge[G[_]](it: InToplevel[?])(
        fa: G[Unit]
    )(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      useEdge(ValidationEdge.InputType(it.name)) {
        S.get.flatMap { s =>
          s.seenInputs.get(it.name) match {
            case Some(i) if (i eq it) => G.unit
            case Some(_)              => raise(CyclicDivergingTypeReference(it.name))
            case None =>
              schema.discover.inputs
                .get(it.name)
                .traverse_ {
                  case i if (i eq it) => G.unit
                  case _              => raise(DivergingTypeReference(it.name))
                } >>
                S.set(s.copy(seenInputs = s.seenInputs + (it.name -> it))) *>
                fa <*
                S.modify(_.copy(seenInputs = s.seenInputs))
          }
        }
      }

    def allUnique[G[_]](f: String => ValidationError, xs: List[String])(implicit
        G: Applicative[G],
        S: Stateful[G, ValidationState]
    ): G[Unit] =
      xs
        .groupBy(identity)
        .toList
        .collect { case (name, xs) if xs.size > 1 => name }
        .traverse_(name => raise(f(name)))

    def validateTypeName[G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      QueryParser.name.parseAll(name) match {
        case Left(_)  => raise(InvalidTypeName(name))
        case Right(_) => G.unit
      }

    def validateFieldName[G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      QueryParser.name.parseAll(name) match {
        case Left(_)  => raise(InvalidFieldName(name))
        case Right(_) => G.unit
      }

    def validateInput[G[_]: Monad](input: In[?])(implicit S: Stateful[G, ValidationState]): G[Unit] = {
      input match {
        case InArr(of, _) => validateInput[G](of)
        case InOpt(of)    => validateInput[G](of)
        case t @ Input(name, fields, _) =>
          useInputEdge(t) {
            validateTypeName[G](name) *> validateArg[G](fields)
          }
        case Enum(name, _, _)      => validateTypeName[G](name)
        case Scalar(name, _, _, _) => validateTypeName[G](name)
      }
    }

    def validateArg[G[_]](arg: Arg[?])(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      allUnique[G](DuplicateArg, arg.entries.toList.map(_.name)) >> {
        // A trick;
        // We check the arg like we would in a user-supplied query
        // Except, we use default as the "input" such that it is verified against the arg
        val checkArgsF =
          arg.entries
            .map(x => x.defaultValue.tupleRight(x))
            .collect { case Some((dv, x)) =>
              val pv = PreparedQuery.valueToParserValue(dv)
              (x, pv)
            }
            .traverse { case (a, pv) =>
              PreparedQuery
                .parseInput[PreparedQuery.H, Any](pv, a.input.value.asInstanceOf[In[Any]], None, ambigiousEnum = false)
                .runA(PreparedQuery.Prep.empty)
                .value
                .value match {
                case Left(err) =>
                  val suf = err.position.position.collect { case PreparedQuery.PrepEdge.ASTEdge(x) => x }
                  raise(InvalidInput(err), suf)
                case Right(_) => G.unit
              }
            }

        checkArgsF >>
          arg.entries.traverse_ { entry =>
            useEdge(ValidationEdge.Arg(entry.name)) {
              validateFieldName[G](entry.name) >> validateInput[G](entry.input.value)
            }
          }
      }

    def validateFields[G[_]: Monad](fields: NonEmptyList[(String, Field[F, ?, ?, ?])])(implicit
        S: Stateful[G, ValidationState]
    ): G[Unit] =
      allUnique[G](DuplicateField, fields.toList.map { case (name, _) => name }) >>
        fields.traverse_ { case (name, field) =>
          useEdge(ValidationEdge.Field(name)) {
            validateFieldName[G](name) >>
              validateArg[G](field.args) >>
              validateOutput[G](field.output.value)
          }
        }

    def validateToplevel[G[_]](tl: OutToplevel[F, ?])(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] = {
      useOutputEdge[G](tl) {
        validateTypeName[G](tl.name) >> {
          tl match {
            case Union(_, types, _) =>
              val ols = types.toList.map(_.tpe)

              allUnique[G](DuplicateUnionInstance, ols.map(_.value.name)) >>
                ols.traverse_(x => validateOutput[G](x.value))
            // TODO on both (interface extension)
            case Type(_, fields, _, _)      => validateFields[G](fields)
            case Interface(_, fields, _, _) => validateFields[G](fields)
            // case Interface(_, instances, fields, _, _) =>
            //   val insts = instances

            //   val ols = insts.toList.map(_.ol)
            //   allUnique[G](DuplicateInterfaceInstance, ols.map(_.value.name)) >>
            //     ols.traverse_(x => validateOutput[G](x.value)) >>
            //     validateFields[G](fields)
            case Enum(_, _, _)      => G.unit
            case Scalar(_, _, _, _) => G.unit
          }
        }
      }
    }

    def validateOutput[G[_]: Monad](tl: Out[F, ?])(implicit S: Stateful[G, ValidationState]): G[Unit] =
      tl match {
        case x: OutToplevel[F, ?] => validateToplevel[G](x)
        case OutArr(of, _)        => validateOutput[G](of.asInstanceOf[Out[F, Any]])
        case OutOpt(of)           => validateOutput[G](of.asInstanceOf[Out[F, Any]])
      }

    val outs = (schema.query :: (schema.mutation ++ schema.subscription).toList ++ schema.outputTypes)
      .traverse_(validateOutput[State[ValidationState, *]])

    val ins = schema.inputTypes
      .traverse_(validateInput[State[ValidationState, *]])

    Chain.fromSeq {
      (outs, ins).tupled
        .runS(ValidationState(Chain.empty, Chain.empty, Map.empty, Map.empty))
        .value
        .problems
        .toList
        .distinct
    }
  }

  def renderValueDoc(v: Value): Doc = {
    import Value._
    v match {
      case IntValue(v)     => Doc.text(v.toString)
      case StringValue(v)  => Doc.text(s""""$v"""")
      case FloatValue(v)   => Doc.text(v.toString)
      case NullValue       => Doc.text("null")
      case BooleanValue(v) => Doc.text(v.toString)
      case ArrayValue(v) =>
        Doc.intercalate(Doc.comma + Doc.line, v.map(renderValueDoc)).tightBracketBy(Doc.char('['), Doc.char(']'))
      case ObjectValue(fields) =>
        Doc
          .intercalate(
            Doc.comma + Doc.line,
            fields.map { case (k, v) => Doc.text(k) + Doc.text(": ") + renderValueDoc(v) }
          )
          .bracketBy(Doc.char('{'), Doc.char('}'))
      case EnumValue(v) => Doc.text(v)
    }
  }
  def render[F[_]](shape: SchemaShape[F, ?, ?, ?]) = {
    lazy val triple = Doc.text("\"\"\"")

    def doc(d: Option[String]) =
      d match {
        case None => Doc.empty
        case Some(x) =>
          val o =
            if (x.contains("\n")) {
              triple + Doc.hardLine + Doc.text(x) + Doc.hardLine + triple
            } else {
              Doc.text("\"") + Doc.text(x) + Doc.text("\"")
            }
          o + Doc.hardLine
      }

    def getInputNameDoc(in: In[?], optional: Boolean = false): Doc =
      in match {
        case t: Toplevel[?] => Doc.text(if (optional) t.name else t.name + "!")
        case InArr(of, _) =>
          lazy val d = getInputNameDoc(of, optional = false)
          d.tightBracketBy(Doc.char('['), Doc.char(']')) + (if (optional) Doc.empty else Doc.char('!'))
        case InOpt(of) => getInputNameDoc(of, optional = true)
      }

    def renderArgValueDoc(av: ArgValue[?]): Doc = {
      val o = av.defaultValue.map(dv => Doc.text(" = ") + renderValueDoc(dv)).getOrElse(Doc.empty)
      doc(av.description) +
        Doc.text(av.name) + Doc.text(": ") + getInputNameDoc(av.input.value) + o
    }

    def renderOutputDoc[G[_]](o: Out[G, ?], optional: Boolean = false): Doc =
      o match {
        case ot: OutToplevel[G, ?] => Doc.text(if (optional) ot.name else ot.name + "!")
        case OutArr(of, _) =>
          lazy val d = renderOutputDoc(of, optional = false)
          d.tightBracketBy(Doc.char('['), Doc.char(']')) + (if (optional) Doc.empty else Doc.char('!'))
        case OutOpt(of) => renderOutputDoc(of, optional = true)
      }

    def renderFieldDoc[G[_]](name: String, field: Field[G, ?, ?, ?]): Doc = {
      val args = NonEmptyChain
        .fromChain(field.args.entries)
        .map(nec =>
          Doc.intercalate(Doc.comma + Doc.lineOrSpace, nec.toList.map(renderArgValueDoc)).tightBracketBy(Doc.char('('), Doc.char(')'))
        )
        .getOrElse(Doc.empty)

      doc(field.description) +
        Doc.text(name) + args + Doc.text(": ") + renderOutputDoc(field.output.value)
    }

    lazy val discovery: DiscoveryState[F] = shape.discover

    lazy val all = discovery.inputs ++ discovery.outputs

    lazy val exclusion = Set("String", "Int", "Float", "ID", "Boolean")

    val docs =
      all.values.toList
        .filterNot(x => exclusion.contains(x.name))
        .map { tl =>
          tl match {
            case e @ Enum(name, _, desc) =>
              doc(desc) +
                Doc.text(s"enum $name {") + Doc.hardLine +
                Doc.intercalate(
                  Doc.hardLine,
                  e.mappings.toList.map { case (name, value) => doc(value.description) + Doc.text(name) }
                ) +
                Doc.hardLine + Doc.text("}")
            case Input(name, fields, desc) =>
              doc(desc) +
                Doc.text(s"input $name") + (Doc.text(" {") + Doc.hardLine + Doc
                  .intercalate(Doc.hardLine, fields.nec.toList.map(renderArgValueDoc))
                  .indent(2) + Doc.hardLine + Doc.text("}"))
            // Dont render built-in scalars
            case Scalar(name, _, _, desc) => doc(desc) + Doc.text(s"scalar $name")
            case ol @ Interface(name, fields, _, desc) =>
              val fieldsDoc = Doc
                .intercalate(
                  Doc.hardLine,
                  fields.toList.map { case (name, field) => renderFieldDoc(name, field) }
                )
                .indent(2)

              val interfaces = ol.implementsMap.keySet.toList.toNel
                .map { nel => Doc.text(" implements ") + Doc.intercalate(Doc.text(" & "), nel.toList.map(Doc.text)) }
                .getOrElse(Doc.empty)

              doc(desc) +
                (Doc.text(s"interface $name") + interfaces + Doc.text(" {") + Doc.hardLine +
                  fieldsDoc +
                  Doc.hardLine + Doc.text("}"))
            case ol @ Type(name, fields, _, desc) =>
              val fieldsDoc = Doc
                .intercalate(
                  Doc.hardLine,
                  fields.toList.map { case (name, field) => renderFieldDoc(name, field) }
                )
                .indent(2)

              val interfaces = ol.implementsMap.keySet.toList.toNel
                .map { nel => Doc.text(" implements ") + Doc.intercalate(Doc.text(" & "), nel.toList.map(Doc.text)) }
                .getOrElse(Doc.empty)

              doc(desc) +
                Doc.text(s"type $name") + interfaces + (Doc.text(" {") + Doc.hardLine +
                  fieldsDoc +
                  Doc.hardLine + Doc.text("}"))
            case Union(name, types, desc) =>
              val names = types.toList.map(x => Doc.text(x.tpe.value.name))
              val xs =
                if (names.size <= 3) Doc.intercalate(Doc.text(" | "), names)
                else Doc.hardLine + Doc.intercalate(Doc.hardLine, names.map(d => Doc.text("| ").indent(2) + d))

              doc(desc) + (Doc.text(s"union $name = ") + xs)
          }
        }

    Doc.intercalate(Doc.hardLine + Doc.hardLine, docs).render(80)
  }

  sealed trait __TypeKind
  object __TypeKind {
    case object SCALAR extends __TypeKind
    case object OBJECT extends __TypeKind
    case object INTERFACE extends __TypeKind
    case object UNION extends __TypeKind
    case object ENUM extends __TypeKind
    case object INPUT_OBJECT extends __TypeKind
    case object LIST extends __TypeKind
    case object NON_NULL extends __TypeKind
  }

  def introspect[F[_]](ss: SchemaShape[F, ?, ?, ?]): NonEmptyList[(String, Field[F, Unit, ?, ?])] = {
    import gql.dsl._
    // We do a little lazy evaluation trick to include the introspection schema in itself
    lazy val d = {
      val ds = ss.discover
      val introspectionDiscovery = discover[F](
        SchemaShape.make[F](
          tpe[F, Unit](
            "Query",
            rootFields.head,
            rootFields.tail: _*
          )
        )
      )
      DiscoveryState[F](
        ds.inputs ++ introspectionDiscovery.inputs,
        ds.outputs ++ introspectionDiscovery.outputs,
        ds.implementations ++ introspectionDiscovery.implementations
      )
    }

    implicit lazy val __typeKind: Enum[F, __TypeKind] = enumType[F, __TypeKind](
      "__TypeKind",
      "SCALAR" -> enumVal(__TypeKind.SCALAR),
      "OBJECT" -> enumVal(__TypeKind.OBJECT),
      "INTERFACE" -> enumVal(__TypeKind.INTERFACE),
      "UNION" -> enumVal(__TypeKind.UNION),
      "ENUM" -> enumVal(__TypeKind.ENUM),
      "INPUT_OBJECT" -> enumVal(__TypeKind.INPUT_OBJECT),
      "LIST" -> enumVal(__TypeKind.LIST),
      "NON_NULL" -> enumVal(__TypeKind.NON_NULL)
    )

    implicit lazy val __inputValue: Type[F, ArgValue[?]] = tpe[F, ArgValue[?]](
      "__InputValue",
      "name" -> pure(_.name),
      "description" -> pure(_.description),
      "type" -> pure(x => (TypeInfo.InInfo(x.input.value): TypeInfo)),
      "defaultValue" -> pure(x => x.defaultValue.map(renderValueDoc(_).render(80)))
    )

    final case class NamedField(
        name: String,
        field: Field[F, ?, ?, ?]
    )

    implicit lazy val namedField: Type[F, NamedField] = tpe[F, NamedField](
      "__Field",
      "name" -> pure(_.name),
      "description" -> pure(_.field.description),
      "args" -> pure(_.field.args.entries.toList),
      "type" -> pure(x => (TypeInfo.OutInfo(x.field.output.value): TypeInfo)),
      "isDeprecated" -> pure(_ => false),
      "deprecationReason" -> pure(_ => Option.empty[String])
    )

    sealed trait TypeInfo {
      def asToplevel: Option[Toplevel[?]]
    }
    object TypeInfo {
      // TODO unify this and the schema shape modifier stack code
      final case class OutInfo(t: Out[F, ?]) extends TypeInfo {
        lazy val inner: OutToplevel[F, ?] = {
          val (ot, _) = partition
          ot
        }

        override lazy val asToplevel: Option[Toplevel[?]] = Some(inner)

        lazy val partition: (OutToplevel[F, ?], Option[ModifierStack]) = {
          def go(t: Out[F, ?], inOption: Boolean = false): (OutToplevel[F, ?], Chain[Modifier]) = {
            val suffix = if (inOption) Chain.empty else Chain(Modifier.NonNull)
            t match {
              case t: OutToplevel[F, ?] => (t, suffix)
              case OutArr(x, _) =>
                val (t, stack) = go(x.asInstanceOf[Out[F, Any]], inOption = false)
                (t, stack append Modifier.List concat suffix)
              case OutOpt(x) =>
                val (t, stack) = go(x.asInstanceOf[Out[F, Any]], inOption = true)
                (t, stack append Modifier.NonNull)
            }
          }
          val (ot, stack) = go(t)
          (ot, stack.toList.toNel.map(ModifierStack(_)))
        }
      }
      final case class InInfo(t: In[?]) extends TypeInfo {
        lazy val inner: InToplevel[?] = {
          val (ot, _) = partition
          ot
        }

        override lazy val asToplevel: Option[Toplevel[?]] = Some(inner)

        lazy val partition: (InToplevel[?], Option[ModifierStack]) = {
          def go(t: In[?], inOption: Boolean = false): (InToplevel[?], Chain[Modifier]) = {
            val suffix = if (inOption) Chain.empty else Chain(Modifier.NonNull)
            t match {
              case t: InToplevel[?] => (t, suffix)
              case InArr(x, _) =>
                val (t, stack) = go(x, inOption = false)
                (t, stack append Modifier.List concat suffix)
              case InOpt(x) =>
                val (t, stack) = go(x, inOption = true)
                (t, stack append Modifier.NonNull)
            }
          }
          val (ot, stack) = go(t)
          (ot, stack.toList.toNel.map(ModifierStack(_)))
        }
      }

      sealed trait Modifier
      object Modifier {
        case object List extends Modifier
        case object NonNull extends Modifier
      }
      final case class ModifierStack(t: NonEmptyList[Modifier]) extends TypeInfo {
        override val asToplevel = None
      }
    }
    def inclDeprecated = arg[Boolean]("includeDeprecated", value.scalar(false))

    implicit lazy val __type: Type[F, TypeInfo] = tpe[F, TypeInfo](
      "__Type",
      "kind" -> pure[F, TypeInfo, __TypeKind] {
        case TypeInfo.ModifierStack(x) =>
          x.head match {
            case TypeInfo.Modifier.List    => __TypeKind.LIST
            case TypeInfo.Modifier.NonNull => __TypeKind.NON_NULL
          }
        case oi: TypeInfo.OutInfo =>
          oi.inner match {
            case _: Scalar[F, ?]    => __TypeKind.SCALAR
            case _: Enum[F, ?]      => __TypeKind.ENUM
            case _: Type[F, ?]      => __TypeKind.OBJECT
            case _: Interface[F, ?] => __TypeKind.INTERFACE
            case _: Union[F, ?]     => __TypeKind.UNION
          }
        case ii: TypeInfo.InInfo =>
          ii.inner match {
            case Scalar(_, _, _, _) => __TypeKind.SCALAR
            case Enum(_, _, _)      => __TypeKind.ENUM
            case _: Input[?]        => __TypeKind.INPUT_OBJECT
          }
      },
      "name" -> pure(_.asToplevel.map(_.name)),
      "description" -> pure(_.asToplevel.flatMap(_.description)),
      "fields" -> pure(inclDeprecated) {
        case (oi: TypeInfo.OutInfo, _) =>
          oi.inner match {
            case Type(_, fields, _, _)      => Some(fields.toList.map { case (k, v) => NamedField(k, v) })
            case Interface(_, fields, _, _) => Some(fields.toList.map { case (k, v) => NamedField(k, v) })
            case _                          => None
          }
        case _ => None
      },
      "interfaces" -> pure {
        case oi: TypeInfo.OutInfo =>
          oi.inner match {
            case Type(_, _, impls, _)      => impls.map[TypeInfo](impl => TypeInfo.OutInfo(impl.implementation.value)).some
            case Interface(_, _, impls, _) => impls.map[TypeInfo](impl => TypeInfo.OutInfo(impl.implementation.value)).some
            case _                         => None
          }
        case _ => None
      },
      "possibleTypes" -> pure {
        case oi: TypeInfo.OutInfo =>
          oi.inner match {
            case Interface(name, _, _, _) =>
              d.implementations
                .get(name)
                .toList
                .flatMap(_.values.toList)
                .map { case (ol, _) => TypeInfo.OutInfo(ol) }
                .some
            case Union(_, instances, _) => instances.toList.map[TypeInfo](x => TypeInfo.OutInfo(x.tpe.value)).some
            case _                      => None
          }
        case _ => None
      },
      "enumValues" -> pure(inclDeprecated) { case (ti, _) =>
        ti.asToplevel.collect { case Enum(_, m, _) => m.toList.map { case (k, v) => NamedEnumValue(k, v) } }
      },
      "inputFields" -> pure {
        case ii: TypeInfo.InInfo =>
          ii.inner match {
            case Input(_, fields, _) => Some(fields.entries.toList)
            case _                   => None
          }
        case _ => None
      },
      "ofType" -> pure {
        case TypeInfo.ModifierStack(NonEmptyList(_, tl)) =>
          tl.toNel.map[TypeInfo](TypeInfo.ModifierStack(_))
        case _ => None
      }
    )

    final case class NamedEnumValue(
        name: String,
        value: EnumValue[?]
    )
    implicit lazy val enumValue: Type[F, NamedEnumValue] = tpe[F, NamedEnumValue](
      "__EnumValue",
      "name" -> pure(_.name),
      "description" -> pure(_.value.description),
      "isDeprecated" -> pure(_ => false),
      "deprecationReason" -> pure(_ => Option.empty[String])
    )

    case object PhantomSchema
    implicit lazy val schema: Type[F, PhantomSchema.type] = tpe[F, PhantomSchema.type](
      "__Schema",
      "description" -> pure(_ => Option.empty[String]),
      "types" -> pure(_ => d.outputs.values.toList.map[TypeInfo](TypeInfo.OutInfo(_))),
      "queryType" -> pure(_ => TypeInfo.OutInfo(ss.query): TypeInfo),
      "mutationType" -> pure(_ => ss.mutation.map[TypeInfo](TypeInfo.OutInfo(_))),
      "subscriptionType" -> pure(_ => ss.subscription.map[TypeInfo](TypeInfo.OutInfo(_))),
      "directives" -> pure(_ => List.empty[String])
    )

    lazy val rootFields: NonEmptyList[(String, Field[F, Unit, ?, ?])] =
      NonEmptyList.of(
        "__schema" -> pure(_ => PhantomSchema),
        "__type" -> pure(arg[String]("name")) { case (_, name) =>
          d.inputs
            .get(name)
            .map[TypeInfo](TypeInfo.InInfo(_))
            .orElse(d.outputs.get(name).map[TypeInfo](TypeInfo.OutInfo(_)))
        }
      )

    rootFields
  }
}
