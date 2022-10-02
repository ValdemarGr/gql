package gql

import cats.effect._
import cats._
import cats.implicits._
import cats.mtl._
import cats.data._
import gql.ast._
import gql.parser.QueryParser

final case class SchemaShape[F[_], Q, M, S](
    query: Option[Type[F, Q]] = None,
    mutation: Option[Type[F, M]] = None,
    subscription: Option[Type[F, S]] = None
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
              case o @ Type(_, _) => handleFields(o)
              case o @ Interface(_, instances, _) =>
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
              case Union(_, instances) =>
                instances.toList.traverse_(inst => goOutput[G](inst.ol.value))
              case _ => G.unit
            }
          }
      }

    def goInput[G[_]](inp: In[Any])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      inp match {
        // TODO
        case InArr(of) => goInput[G](of)
        case InOpt(of) => goInput[G](of)
        case t: InToplevel[Any] =>
          inputNotSeen(t) {
            t match {
              case Input(_, fields) =>
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

  sealed trait ValidationEdge
  object ValidationEdge {
    final case class Field(name: String) extends ValidationEdge
    final case class OutputType(name: String) extends ValidationEdge
    final case class Arg(name: String) extends ValidationEdge
    final case class InputType(name: String) extends ValidationEdge
  }

  final case class Problem(
      message: String,
      path: Chain[ValidationEdge]
  ) {
    override def toString() =
      s"$message at ${path
        .map {
          case ValidationEdge.Field(name)      => s".$name"
          case ValidationEdge.OutputType(name) => s":$name"
          case ValidationEdge.Arg(name)        => s".$name"
          case ValidationEdge.InputType(name)  => s":$name"
        }
        .mkString_("")}"
  }
  // TODO has really bad running time on some inputs
  // since it doesn't remember what references it has seen
  def validate[F[_]](schema: SchemaShape[F, _, _, _]) = {
    final case class ValidationState(
        problems: Chain[Problem],
        currentPath: Chain[ValidationEdge],
        seenOutputs: Map[String, OutToplevel[F, _]],
        seenInputs: Map[String, InToplevel[_]]
    )

    def raise[G[_]](msg: String)(implicit S: Stateful[G, ValidationState]): G[Unit] =
      S.modify(s => s.copy(problems = s.problems :+ Problem(msg, s.currentPath)))

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
            case Some(o) =>
              raise(s"cyclic type ${ot.name} is not reference equal use lazy val or `cats.Eval` to declare this type")
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
            case Some(i) =>
              raise(s"cyclic input type ${it.name} is not reference equal use lazy val or `cats.Eval` to declare this type")
            case None =>
              S.set(s.copy(seenInputs = s.seenInputs + (it.name -> it))) *>
                fa <*
                S.modify(_.copy(seenInputs = s.seenInputs))
          }
        }
      }

    def allUnique[G[_]](context: String, xs: List[String])(implicit G: Applicative[G], S: Stateful[G, ValidationState]): G[Unit] =
      xs
        .groupBy(identity)
        .toList
        .collect { case (name, xs) if xs.size > 1 => name }
        .traverse_(name => raise(s"$context: $name"))

    def validateTypeName[G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      QueryParser.name.parseAll(name) match {
        case Left(_)  => raise(s"invalid type name $name, must match /[_A-Za-z][_0-9A-Za-z]*/")
        case Right(_) => G.unit
      }

    def validateFieldName[G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState]): G[Unit] =
      QueryParser.name.parseAll(name) match {
        case Left(_)  => raise(s"invalid field name $name, must match /[_A-Za-z][_0-9A-Za-z]*/")
        case Right(_) => G.unit
      }

    def validateInput[G[_]: Monad](input: In[_])(implicit S: Stateful[G, ValidationState]): G[Unit] = {
      input match {
        case InArr(of) => validateInput[G](of)
        case InOpt(of) => validateInput[G](of)
        case t @ Input(name, fields) =>
          useInputEdge(t) {
            validateTypeName[G](name) *> validateArg[G](fields)
          }
        case Enum(name, _)     => validateTypeName[G](name)
        case Scalar(name, dec) => validateTypeName[G](name)
      }
    }

    def validateArg[G[_]: Monad](arg: Arg[_])(implicit S: Stateful[G, ValidationState]): G[Unit] =
      allUnique[G]("duplicate arg", arg.entries.toList.map(_.name)) >>
        arg.entries.traverse_ { entry =>
          useEdge(ValidationEdge.Arg(entry.name)) {
            validateFieldName[G](entry.name) >> validateInput[G](entry.input.value)
          }
        }

    def validateFields[G[_]: Monad](fields: NonEmptyList[(String, Field[F, _, _, _])])(implicit
        S: Stateful[G, ValidationState]
    ): G[Unit] =
      allUnique[G]("duplicate field", fields.toList.map { case (name, _) => name }) >>
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
            case Type(_, fields) => validateFields[G](fields)
            case Union(_, types) =>
              val ols = types.toList.map(_.ol)

              allUnique[G]("duplicate union instance", ols.map(_.value.name)) >>
                ols.traverse_(x => validateOutput[G](x.value))
            case Interface(_, instances, fields) =>
              val insts = instances

              val ols = insts.toList.map(_.ol)
              allUnique[G]("duplicate interface instance", ols.map(_.value.name)) >>
                ols.traverse_(x => validateOutput[G](x.value)) >>
                validateFields[G](fields)
            case Enum(name, _)   => validateTypeName[G](name)
            case Scalar(name, _) => validateTypeName[G](name)
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

    (schema.query ++ schema.mutation ++ schema.subscription).toList
      .traverse_(validateOutput[State[ValidationState, *]])
      .runS(ValidationState(Chain.empty, Chain.empty, Map.empty, Map.empty))
      .value
      .problems
  }

  def render[F[_]](shape: SchemaShape[F, _, _, _]) = {
    // lets the caller pick the implementation
    def getInputName_(in: In[_]): (String, String) =
      in match {
        case t: Toplevel[_] => (t.name, t.name + "!")
        case InArr(of) =>
          val (_, r) = getInputName_(of)
          val out = s"[$r]"
          (out, out + "!")
        case InOpt(of) =>
          val (o, _) = getInputName_(of)
          (o, o)
      }
    def getInputName(in: In[_]): String =
      getInputName_(in)._2

    def renderDefault(default: DefaultValue[_]): String = {
      import DefaultValue._
      default match {
        case Arr(values)       => "[" + values.map(renderDefault).mkString(", ") + "]"
        case DefaultValue.Null => "null"
        case Primitive(value, in) =>
          in match {
            case e @ Enum(_, _)   => e.revm(value)
            case Scalar(_, codec) => codec.apply(value).noSpaces
          }
        case Obj(fields) => fields.map { case (k, v) => s"$k: ${renderDefault(v)}" }.mkString_("\n")
      }
    }

    def renderArgValue(av: ArgValue[_]): String = {
      val o = av.defaultValue.map(dv => s" = ${renderDefault(dv)}}").mkString
      s"${av.name}: ${getInputName(av.input.value)}$o"
    }

    def renderOutput_[G[_]](o: Out[G, _]): (String, String) =
      o match {
        case ot: OutToplevel[G, _] => (ot.name, ot.name + "!")
        case OutArr(of) =>
          val (_, r) = renderOutput_(of)
          val out = s"[$r]"
          (out, out + "!")
        case OutOpt(of) =>
          val (o, _) = renderOutput_(of)
          (o, o)
      }
    def renderOutput[G[_]](o: Out[G, _]): String =
      renderOutput_(o)._2

    def renderField[G[_]](name: String, field: Field[G, _, _, _]): String = {
      val args = NonEmptyChain
        .fromChain(field.args.entries)
        .map(nec => s"(${nec.map(renderArgValue).mkString_(", ")})")
        .mkString
      s"$name$args: ${renderOutput(field.output.value)}"
    }

    val discovery: DiscoveryState[F] = shape.discover

    val all = discovery.inputs ++ discovery.outputs

    all.values.toList
      .map { tl =>
        tl match {
          case Enum(name, mappings) =>
            s"""
            |enum $name {
              ${mappings.map { case (str, _) => s"|  $str" }.mkString_("\n")}
            |}
            """.stripMargin
          case Input(name, fields) =>
            s"""
            |input $name {
              ${fields.nec.map { av => s"|  ${renderArgValue(av)}" }.mkString_("\n")}
            |}
            """.stripMargin
          case Scalar(name, _) => s"\nscalar $name\n"
          case Interface(name, _, fields) =>
            s"""
            |interface $name {
              ${fields
              .map { case (name, field) => s"|  ${renderField(name, field)}" }
              .mkString_("\n")}
            |}
            """.stripMargin
          case Type(name, fields) =>
            val interfaces =
              discovery.interfaceImplementations
                .get(name)
                .flatMap(_.toList.toNel)
                .map { nel =>
                  s" implements ${nel.mkString_(" & ")}"
                }
                .mkString
            s"""
            |type $name $interfaces {
              ${fields
              .map { case (name, field) => s"|  ${renderField(name, field)}" }
              .mkString_("\n")}
            |}
            """.stripMargin
          case Union(name, types) =>
            s"""
            |union $name = ${types.map(_.ol.value.name).mkString_(" | ")}
            """.stripMargin
        }
      }
      .mkString("\n")
  }
}
