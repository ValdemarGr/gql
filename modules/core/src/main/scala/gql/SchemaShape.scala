package gql

import cats.effect._
import cats._
import cats.implicits._
import cats.mtl._
import cats.data._
import gql.ast._
import gql.parser.QueryParser
import org.typelevel.paiges.Doc
import gql.resolver.EffectResolver
import gql.resolver.Resolver

final case class SchemaShape[F[_], Q, M, S](
    query: Option[Type[F, Q]] = Option.empty[Type[F, Unit]],
    mutation: Option[Type[F, M]] = Option.empty[Type[F, Unit]],
    subscription: Option[Type[F, S]] = Option.empty[Type[F, Unit]]
) {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): SchemaShape[G, Q, M, S] =
    SchemaShape(query.map(_.mapK(fk)), mutation.map(_.mapK(fk)), subscription.map(_.mapK(fk)))

  lazy val discover = SchemaShape.discover[F](this)

  lazy val validate = SchemaShape.validate[F](this)

  lazy val render = SchemaShape.render[F](this)
}

object SchemaShape {
  final case class DiscoveryState[F[_]](
      inputs: Map[String, InToplevel[_]],
      outputs: Map[String, OutToplevel[F, _]],
      // Key is the type, values are the interfaces it implements
      interfaceImplementations: Map[String, Set[String]]
  )

  def discover[F[_]](shape: SchemaShape[F, _, _, _]): DiscoveryState[F] = {
    def inputNotSeen[G[_], A](
        tl: InToplevel[Any]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.inputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(inputs = s.inputs + (tl.name -> tl))) *> ga
      }

    def outputNotSeen[G[_], A](
        tl: OutToplevel[F, _]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.outputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(outputs = s.outputs + (tl.name -> tl))) *> ga
      }

    def goOutput[G[_]](out: Out[F, _])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      out match {
        case OutArr(of) => goOutput[G](of)
        case OutOpt(of) => goOutput[G](of)
        case t: OutToplevel[F, _] =>
          outputNotSeen(t) {
            def handleFields(o: Selectable[F, _]): G[Unit] =
              o.fieldsList.traverse_ { case (_, x) =>
                goOutput[G](x.output.value) >>
                  x.args.entries.traverse_(x => goInput[G](x.input.value.asInstanceOf[In[Any]]))
              }

            t match {
              case o @ Type(_, _, _) => handleFields(o)
              case o @ Interface(_, instances, _, _) =>
                S.modify { s =>
                  val newMap =
                    o.instanceMap.keySet.toList.foldLeft(s.interfaceImplementations) { case (m, instance) =>
                      val y = m.get(instance) match {
                        case None    => Set(o.name)
                        case Some(x) => x + o.name
                      }
                      m + (instance -> y)
                    }
                  s.copy(interfaceImplementations = newMap)
                } >>
                  handleFields(o) >>
                  instances.traverse_(inst => handleFields(inst.ol.value))
              case Union(_, instances, _) =>
                instances.toList.traverse_(inst => goOutput[G](inst.ol.value))
              case _ => G.unit
            }
          }
      }

    def goInput[G[_]](inp: In[Any])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      inp match {
        case InArr(of) => goInput[G](of)
        case InOpt(of) => goInput[G](of)
        case t: InToplevel[Any] =>
          inputNotSeen(t) {
            t match {
              case Input(_, fields, _) =>
                fields.entries.traverse_(x => goInput[G](x.input.value.asInstanceOf[In[Any]]))
              case _ => G.unit
            }
          }
      }

    (shape.query ++ shape.mutation ++ shape.subscription).toList
      .traverse_(goOutput[State[DiscoveryState[F], *]])
      .runS(DiscoveryState(Map.empty, Map.empty, Map.empty))
      .value
  }

  sealed trait ValidationError {
    def message: String
  }
  object ValidationError {
    final case class CyclicOutputType(typename: String) extends ValidationError {
      def message: String =
        s"cyclic type $typename is not reference equal use lazy val or `cats.Eval` to declare this type"
    }
    final case class CyclicInputType(typename: String) extends ValidationError {
      def message: String =
        s"cyclic input type $typename is not reference equal use lazy val or `cats.Eval` to declare this type"
    }
    final case class InvalidTypeName(name: String) extends ValidationError {
      def message: String =
        s"invalid type name $name, must match /[_A-Za-z][_0-9A-Za-z]*/"
    }
    final case class InvalidFieldName(name: String) extends ValidationError {
      def message: String =
        s"invalid field name $name, must match /[_A-Za-z][_0-9A-Za-z]*/"
    }
    final case class DuplicateArg(conflict: String) extends ValidationError {
      def message: String = s"duplicate arg $conflict"
    }
    final case class DuplicateField(conflict: String) extends ValidationError {
      def message: String = s"duplicate field $conflict"
    }
    final case class DuplicateUnionInstance(conflict: String) extends ValidationError {
      def message: String = s"duplicate union instance $conflict"
    }
    final case class DuplicateInterfaceInstance(conflict: String) extends ValidationError {
      def message: String = s"duplicate interface instance $conflict"
    }
    final case class InvalidInput(pe: PreparedQuery.PositionalError) extends ValidationError {
      def message: String = s"invalid argument input: ${pe.message}"
    }
    final case class MissingInterfaceFields(
        typename: String,
        interfaceName: String,
        missing: List[(String, String)]
    ) extends ValidationError {
      def message: String = {
        val missingFields = missing.map { case (name, tpe) => s"$name: $tpe" }.mkString(", ")
        s"type $typename does not implement all of the fields defined in interface $interfaceName, missing fields: $missingFields"
      }
    }
    final case class CyclicInterfaceImplementation(typename: String) extends ValidationError {
      def message: String = s"$typename is an interface implementation of itself"
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
  def validate[F[_]](schema: SchemaShape[F, _, _, _]): Chain[Problem] = {
    final case class ValidationState(
        problems: Chain[Problem],
        currentPath: Chain[ValidationEdge],
        seenOutputs: Map[String, OutToplevel[F, _]],
        seenInputs: Map[String, InToplevel[_]]
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

    def useOutputEdge[G[_]](ot: OutToplevel[F, _])(
        fa: G[Unit]
    )(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      useEdge(ValidationEdge.OutputType(ot.name)) {
        S.get.flatMap { s =>
          s.seenOutputs.get(ot.name) match {
            case Some(o) if (o eq ot) => G.unit
            case Some(o)              => raise(CyclicOutputType(ot.name))
            case None =>
              S.set(s.copy(seenOutputs = s.seenOutputs + (ot.name -> ot))) *>
                fa <*
                S.modify(_.copy(seenOutputs = s.seenOutputs))
          }
        }
      }

    def useInputEdge[G[_]](it: InToplevel[_])(
        fa: G[Unit]
    )(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      useEdge(ValidationEdge.InputType(it.name)) {
        S.get.flatMap { s =>
          s.seenInputs.get(it.name) match {
            case Some(i) if (i eq it) => G.unit
            case Some(i)              => raise(CyclicInputType(it.name))
            case None =>
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

    def validateInput[G[_]: Monad](input: In[_])(implicit S: Stateful[G, ValidationState]): G[Unit] = {
      input match {
        case InArr(of) => validateInput[G](of)
        case InOpt(of) => validateInput[G](of)
        case t @ Input(name, fields, _) =>
          useInputEdge(t) {
            validateTypeName[G](name) *> validateArg[G](fields)
          }
        case Enum(name, _, _)      => validateTypeName[G](name)
        case Scalar(name, _, _, _) => validateTypeName[G](name)
      }
    }

    def validateArg[G[_]](arg: Arg[_])(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      allUnique[G](DuplicateArg, arg.entries.toList.map(_.name)) >> {
        // A trick;
        // We check the arg like we would in a user-supplied query
        // Except, we use default as the "input" such that it is verified against the arg
        val checkArgsF =
          arg.entries
            .map(x => x.defaultValue.tupleRight(x))
            .collect { case Some((dv, x)) =>
              val pv = PreparedQuery.valueToParserValue(PreparedQuery.defaultToValue(dv))
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

    def validateFields[G[_]: Monad](fields: NonEmptyList[(String, Field[F, _, _, _])])(implicit
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

    def validateToplevel[G[_]: Monad](tl: OutToplevel[F, _])(implicit S: Stateful[G, ValidationState]): G[Unit] = {
      useOutputEdge[G](tl) {
        validateTypeName[G](tl.name) >> {
          tl match {
            case Type(_, fields, _) => validateFields[G](fields)
            case Union(_, types, _) =>
              val ols = types.toList.map(_.ol)

              allUnique[G](DuplicateUnionInstance, ols.map(_.value.name)) >>
                ols.traverse_(x => validateOutput[G](x.value))
            case Interface(_, instances, fields, _) =>
              val insts = instances

              val ols = insts.toList.map(_.ol)
              allUnique[G](DuplicateInterfaceInstance, ols.map(_.value.name)) >>
                ols.traverse_(x => validateOutput[G](x.value)) >>
                validateFields[G](fields)
            case Enum(name, _, _)      => validateTypeName[G](name)
            case Scalar(name, _, _, _) => validateTypeName[G](name)
          }
        }
      }
    }

    def validateOutput[G[_]: Monad](tl: Out[F, _])(implicit S: Stateful[G, ValidationState]): G[Unit] =
      tl match {
        case x: OutToplevel[F, _] => validateToplevel[G](x)
        case OutArr(of)           => validateOutput[G](of)
        case OutOpt(of)           => validateOutput[G](of)
      }

    Chain.fromSeq {
      (schema.query ++ schema.mutation ++ schema.subscription).toList
        .traverse_(validateOutput[State[ValidationState, *]])
        .runS(ValidationState(Chain.empty, Chain.empty, Map.empty, Map.empty))
        .value
        .problems
        .toList
        .distinct
    }
  }

  def render[F[_]](shape: SchemaShape[F, _, _, _]) = {

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

    def getInputNameDoc(in: In[_], optional: Boolean = false): Doc =
      in match {
        case t: Toplevel[_] => Doc.text(if (optional) t.name else t.name + "!")
        case InArr(of) =>
          lazy val d = getInputNameDoc(of, optional = false)
          d.tightBracketBy(Doc.char('['), Doc.char(']')) + (if (optional) Doc.empty else Doc.char('!'))
        case InOpt(of) => getInputNameDoc(of, optional = true)
      }

    import io.circe._
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

    def renderArgValueDoc(av: ArgValue[_]): Doc = {
      val o = av.defaultValue.map(dv => Doc.text(" = ") + renderValueDoc(PreparedQuery.defaultToValue(dv))).getOrElse(Doc.empty)
      doc(av.description) +
        Doc.text(av.name) + Doc.text(": ") + getInputNameDoc(av.input.value) + o
    }

    def renderOutputDoc[G[_]](o: Out[G, _], optional: Boolean = false): Doc =
      o match {
        case ot: OutToplevel[G, _] => Doc.text(if (optional) ot.name else ot.name + "!")
        case OutArr(of) =>
          lazy val d = renderOutputDoc(of, optional = false)
          d.tightBracketBy(Doc.char('['), Doc.char(']')) + (if (optional) Doc.empty else Doc.char('!'))
        case OutOpt(of) => renderOutputDoc(of, optional = true)
      }

    def renderFieldDoc[G[_]](name: String, field: Field[G, _, _, _]): Doc = {
      val args = NonEmptyChain
        .fromChain(field.args.entries)
        .map(nec =>
          Doc.intercalate(Doc.comma + Doc.lineOrSpace, nec.toList.map(renderArgValueDoc)).tightBracketBy(Doc.char('('), Doc.char(')'))
        )
        .getOrElse(Doc.empty)

      doc(field.description) +
        Doc.text(name) + args + Doc.text(": ") + renderOutputDoc(field.output.value)
    }

    val discovery: DiscoveryState[F] = shape.discover

    val all = discovery.inputs ++ discovery.outputs

    lazy val exclusion = Set("String", "Int", "Float", "ID", "Boolean")

    val docs =
      all.values.toList
        .filterNot(x => exclusion.contains(x.name))
        .map { tl =>
          tl match {
            case e @ Enum(name, mappings, desc) =>
              doc(desc) +
                Doc.text(s"enum $name {") + Doc.hardLine +
                Doc.intercalate(
                  Doc.hardLine,
                  e.mappings.toList.map(x => doc(x.description) + Doc.text(x.encodedName))
                ) +
                Doc.hardLine + Doc.text("}")
            case Input(name, fields, desc) =>
              doc(desc) +
                Doc.text(s"input $name") + (Doc.text(" {") + Doc.hardLine + Doc
                  .intercalate(Doc.hardLine, fields.nec.toList.map(renderArgValueDoc))
                  .indent(2) + Doc.hardLine + Doc.text("}"))
            // Dont render built-in scalars
            case Scalar(name, _, _, desc) => doc(desc) + Doc.text(s"scalar $name")
            case Interface(name, _, fields, desc) =>
              val fieldsDoc = Doc
                .intercalate(
                  Doc.hardLine,
                  fields.toList.map { case (name, field) => renderFieldDoc(name, field) }
                )
                .indent(2)

              doc(desc) +
                (Doc.text(s"interface $name") + Doc.text(" {") + Doc.hardLine +
                  fieldsDoc +
                  Doc.hardLine + Doc.text("}"))
            case Type(name, fields, desc) =>
              val fieldsDoc = Doc
                .intercalate(
                  Doc.hardLine,
                  fields.toList.map { case (name, field) => renderFieldDoc(name, field) }
                )
                .indent(2)

              val interfaces =
                discovery.interfaceImplementations
                  .get(name)
                  .flatMap(_.toList.toNel)
                  .map { nel => Doc.text(" implements ") + Doc.intercalate(Doc.text(" & "), nel.toList.map(Doc.text)) }
                  .getOrElse(Doc.empty)

              doc(desc) +
                Doc.text(s"type $name") + interfaces + (Doc.text(" {") + Doc.hardLine +
                  fieldsDoc +
                  Doc.hardLine + Doc.text("}"))
            case Union(name, types, desc) =>
              val names = types.toList.map(x => Doc.text(x.ol.value.name))
              val xs =
                if (names.size <= 3) Doc.intercalate(Doc.text(" | "), names)
                else Doc.hardLine + Doc.intercalate(Doc.hardLine, names.map(d => Doc.text("| ").indent(2) + d))

              doc(desc) + (Doc.text(s"union $name = ") + xs)
          }
        }

    Doc.intercalate(Doc.hardLine + Doc.hardLine, docs).render(80)
  }
}
