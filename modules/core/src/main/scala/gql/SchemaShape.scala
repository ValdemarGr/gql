package gql

import cats._
import cats.implicits._
import cats.mtl._
import cats.data._
import gql.out._
import gql.parser.QueryParser

final case class SchemaShape[F[_], Q](
    query: Obj[F, Q]
    // mutation: Output.Obj[F, M],
    // subscription: Output.Obj[F, M],
) {
  lazy val discover = SchemaShape.discover[F, Q](this)

  lazy val validate = SchemaShape.validate[F, Q](this)
}

object SchemaShape {
  final case class DiscoveryState[F[_]](
      inputs: Map[String, ToplevelInput[Any]],
      outputs: Map[String, gql.out.Toplevel[F, Any]],
      // Key is the type, values are the interfaces it implements
      interfaceImplementations: Map[String, Set[String]]
  )

  def discover[F[_], Q](shape: SchemaShape[F, Q]): DiscoveryState[F] = {
    def inputNotSeen[G[_], A](
        tl: ToplevelInput[Any]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.inputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(inputs = s.inputs + (tl.name -> tl))) *> ga
      }

    def outputNotSeen[G[_], A](
        tl: gql.out.Toplevel[F, Any]
    )(ga: G[A])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]], M: Monoid[A]): G[A] =
      S.get.flatMap { s =>
        if (s.outputs.contains(tl.name)) G.pure(M.empty)
        else S.modify(_.copy(outputs = s.outputs + (tl.name -> tl))) *> ga
      }

    def goOutput[G[_]](out: Output[F, Any])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      out match {
        case gql.out.Arr(of) => goOutput[G](of)
        case gql.out.Opt(of) => goOutput[G](of)
        case t: gql.out.Toplevel[F, Any] =>
          outputNotSeen(t) {
            def handleFields(o: ObjLike[F, Any]): G[Unit] =
              o.fieldsList.traverse_ { case (_, x) =>
                goOutput[G](x.output.value) >>
                  x.args.entries.traverse_(x => goInput[G](x.input.asInstanceOf[Input[Any]]))
              }

            t match {
              case o @ gql.out.Obj(_, _) => handleFields(o)
              case o @ gql.out.Interface(_, instances, _) =>
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
                  instances.traverse_(inst => handleFields(inst.ol))
              case gql.out.Union(_, instances) =>
                instances.toList.traverse_(inst => goOutput[G](inst.ol))
              case _ => G.unit
            }
          }
      }

    def goInput[G[_]](in: Input[Any])(implicit G: Monad[G], S: Stateful[G, DiscoveryState[F]]): G[Unit] =
      in match {
        case Input.Arr(of) => goInput[G](of)
        case Input.Opt(of) => goInput[G](of)
        case t: ToplevelInput[Any] =>
          inputNotSeen(t) {
            t match {
              case Input.Obj(_, fields) =>
                fields.entries.traverse_(x => goInput[G](x.input.asInstanceOf[Input[Any]]))
              case _ => G.unit
            }
          }
      }

    goOutput[State[DiscoveryState[F], *]](shape.query).runS(DiscoveryState(Map.empty, Map.empty, Map.empty)).value
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
  def validate[F[_], Q](schema: SchemaShape[F, Q]) = {
    final case class ValidationState(
        problems: Chain[Problem],
        currentPath: Chain[ValidationEdge],
        seenOutputs: Map[String, gql.out.Toplevel[F, Any]],
        seenInputs: Map[String, ToplevelInput[_]]
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

    def useOutputEdge[G[_]](ot: gql.out.Toplevel[F, Any])(
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

    def useInputEdge[G[_]](it: ToplevelInput[_])(
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

    def validateInput[G[_]: Monad](input: Input[_])(implicit S: Stateful[G, ValidationState]): G[Unit] = {
      input match {
        case Input.Arr(of) => validateInput[G](of)
        case Input.Opt(of) => validateInput[G](of)
        case t @ Input.Obj(name, fields) =>
          useInputEdge(t) {
            validateTypeName[G](name) *> validateArg[G](fields)
          }
        case Input.Enum(name, _)     => validateTypeName[G](name)
        case Input.Scalar(name, dec) => validateTypeName[G](name)
      }
    }

    def validateArg[G[_]: Monad](arg: Arg[_])(implicit S: Stateful[G, ValidationState]): G[Unit] =
      allUnique[G]("duplicate arg", arg.entries.toList.map(_.name)) >>
        arg.entries.traverse_ { entry =>
          useEdge(ValidationEdge.Arg(entry.name)) {
            validateFieldName[G](entry.name) >> validateInput[G](entry.input)
          }
        }

    def validateFields[G[_]: Monad](fields: NonEmptyList[(String, Field[F, Any, _, _])])(implicit
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

    def validateToplevel[G[_]: Monad](tl: gql.out.Toplevel[F, Any])(implicit S: Stateful[G, ValidationState]): G[Unit] = {
      useOutputEdge[G](tl) {
        validateTypeName[G](tl.name) >> {
          tl match {
            case gql.out.Obj(_, fields) => validateFields[G](fields)
            case gql.out.Union(_, types) =>
              val ols = types.toList.map(_.ol)

              allUnique[G]("duplicate union instance", ols.map(_.name)) >> ols.traverse_(validateOutput[G])
            case gql.out.Interface(_, instances, fields) =>
              val insts = instances

              val ols = insts.toList.map(_.ol)
              allUnique[G]("duplicate interface instance", ols.map(_.name)) >> ols.traverse_(validateOutput[G]) >>
                validateFields[G](fields)
            case Enum(name, _)   => validateTypeName[G](name)
            case Scalar(name, _) => validateTypeName[G](name)
          }
        }
      }
    }

    def validateOutput[G[_]: Monad](tl: Output[F, Any])(implicit S: Stateful[G, ValidationState]): G[Unit] =
      tl match {
        case x: gql.out.Toplevel[F, Any] => validateToplevel[G](x)
        case Arr(of)                     => validateOutput[G](of)
        case Opt(of)                     => validateOutput[G](of)
      }

    validateOutput[State[ValidationState, *]](schema.query)
      .runS(ValidationState(Chain.empty, Chain.empty, Map.empty, Map.empty))
      .value
      .problems
  }
}
