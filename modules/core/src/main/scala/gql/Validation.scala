package gql

import cats.data._
import cats._
import cats.implicits._
import cats.mtl._
import gql.ast._
import gql.parser.QueryParser

object Validation {
  sealed trait Error {
    def message: String
  }
  object Error {
    final case class DivergingTypeReference(typename: String) extends Error {
      def message: String =
        s"`$typename` is not reference equal. Use lazy val or `cats.Eval` to declare this type."
    }
    final case class CyclicDivergingTypeReference(typename: String) extends Error {
      def message: String =
        s"Cyclic type `$typename` is not reference equal. Use lazy val or `cats.Eval` to declare this type."
    }
    final case class InvalidTypeName(name: String) extends Error {
      def message: String =
        s"Invalid type name '$name', the argument name must match /[_A-Za-z][_0-9A-Za-z]*/"
    }
    final case class InvalidFieldName(name: String) extends Error {
      def message: String =
        s"Invalid field name '$name', the field name must match /[_A-Za-z][_0-9A-Za-z]*/"
    }
    final case class DuplicateArg(conflict: String) extends Error {
      def message: String = s"Duplicate arg `$conflict`."
    }
    final case class DuplicateField(conflict: String) extends Error {
      def message: String = s"Duplicate field `$conflict`."
    }
    final case class DuplicateUnionInstance(conflict: String) extends Error {
      def message: String = s"Duplicate union instance `$conflict`."
    }
    final case class DuplicateInterfaceInstance(conflict: String) extends Error {
      def message: String = s"Duplicate interface instance `$conflict`."
    }
    final case class InvalidInput(pe: PreparedQuery.PositionalError) extends Error {
      def message: String = s"Invalid argument input: ${pe.message}."
    }
    final case class MissingInterfaceFields(
        typename: String,
        interfaceName: String,
        fieldName: String,
        fieldType: String
    ) extends Error {
      def message: String = {
        s"Type `$typename` does not implement all of the fields defined in interface `$interfaceName`, missing field '$fieldName' of type `$fieldType`."
      }
    }
    final case class CyclicInterfaceImplementation(typename: String) extends Error {
      def message: String = s"`$typename` is an interface implementation of itself."
    }
    final case class TransitiveInterfacesNotImplemented(typename: String, interfaces: List[(String, String)]) extends Error {
      def message: String =
        s"$typename does not implement all interfaces: ${interfaces.map { case (through, name) => s"`$name` through `$through`" }.mkString(" and ")}."
    }
    final case class WrongInterfaceFieldType(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        expected: String,
        actual: String
    ) extends Error {
      def message: String =
        s"Field '$fieldName' in `$typename` is of type `$actual` but expected `$expected` from interface `$sourceInterface`."
    }
    final case class WrongInterfaceArgument(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        expected: String,
        actual: String
    ) extends Error {
      def message: String = s"Field '$fieldName' in `$typename` has argument ''"
    }
  }

  sealed trait Edge {
    def name: String
  }
  object Edge {
    final case class Field(name: String) extends Edge
    final case class OutputType(name: String) extends Edge
    final case class Arg(name: String) extends Edge
    final case class InputType(name: String) extends Edge
    final case class Index(i: Int) extends Edge {
      def name: String = i.toString
    }
  }

  final case class Problem(
      error: Error,
      path: Chain[Edge]
  ) {
    override def toString() =
      s"${error.message} at ${path
        .map {
          case Edge.Field(name)      => s".$name"
          case Edge.OutputType(name) => s"($name)"
          case Edge.Arg(name)        => s".$name"
          case Edge.InputType(name)  => s"($name)"
          case Edge.Index(i)         => s"[$i]"
        }
        .mkString_("")}"
  }

  final case class ValidationState[F[_]](
      problems: Chain[Problem],
      currentPath: Chain[Edge],
      seenOutputs: Map[String, OutToplevel[F, ?]],
      seenInputs: Map[String, InToplevel[?]]
  )
  import Error._

  // TODO has really bad running time on some inputs
  // since it doesn't remember what it has seen
  // Update: when #55 is fixed, this should be implicitly be fixed
  def validate[F[_]](schema: SchemaShape[F, ?, ?, ?]): Chain[Problem] = {
    val outs = (schema.query :: (schema.mutation ++ schema.subscription).toList ++ schema.outputTypes)
      .traverse_(validateOutput[F, State[ValidationState[F], *]](_, schema.discover))

    val ins = schema.inputTypes
      .traverse_(validateInput[F, State[ValidationState[F], *]](_, schema.discover))

    Chain.fromSeq {
      (outs, ins).tupled
        .runS(ValidationState(Chain.empty, Chain.empty, Map.empty, Map.empty))
        .value
        .problems
        .toList
        .distinct
    }
  }

  def raise[F[_], G[_]](err: Error, suffix: Chain[Edge] = Chain.empty)(implicit S: Stateful[G, ValidationState[F]]): G[Unit] =
    S.modify(s => s.copy(problems = s.problems :+ Problem(err, s.currentPath ++ suffix)))

  def useEdge[F[_], G[_], A](edge: Edge)(
      fa: G[A]
  )(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[A] =
    S.get.flatMap { s =>
      S.set(s.copy(currentPath = s.currentPath :+ edge)) *>
        fa <*
        S.modify(_.copy(currentPath = s.currentPath))
    }

  def useOutputEdge[F[_], G[_]](ot: OutToplevel[F, ?], discovery: SchemaShape.DiscoveryState[F])(
      fa: G[Unit]
  )(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    useEdge(Edge.OutputType(ot.name)) {
      S.get.flatMap { s =>
        s.seenOutputs.get(ot.name) match {
          case Some(o) if (o eq ot) => G.unit
          case Some(_)              => raise(CyclicDivergingTypeReference(ot.name))
          case None =>
            discovery.outputs
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

  def useInputEdge[F[_], G[_]](it: InToplevel[?], discovery: SchemaShape.DiscoveryState[F])(
      fa: G[Unit]
  )(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    useEdge(Edge.InputType(it.name)) {
      S.get.flatMap { s =>
        s.seenInputs.get(it.name) match {
          case Some(i) if (i eq it) => G.unit
          case Some(_)              => raise(CyclicDivergingTypeReference(it.name))
          case None =>
            discovery.inputs
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

  def allUnique[F[_], G[_]](f: String => Error, xs: List[String])(implicit
      G: Applicative[G],
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    xs
      .groupBy(identity)
      .toList
      .collect { case (name, xs) if xs.size > 1 => name }
      .traverse_(name => raise(f(name)))

  def validateTypeName[F[_], G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    QueryParser.name.parseAll(name) match {
      case Left(_)  => raise(InvalidTypeName(name))
      case Right(_) => G.unit
    }

  def validateFieldName[F[_], G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    QueryParser.name.parseAll(name) match {
      case Left(_)  => raise(InvalidFieldName(name))
      case Right(_) => G.unit
    }

  def validateInput[F[_], G[_]: Monad](input: In[?], discovery: SchemaShape.DiscoveryState[F])(implicit
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] = {
    input match {
      case InArr(of, _) => validateInput[F, G](of, discovery)
      case InOpt(of)    => validateInput[F, G](of, discovery)
      case t @ Input(name, fields, _) =>
        useInputEdge(t, discovery) {
          validateTypeName[F, G](name) *> validateArg[F, G](fields, discovery)
        }
      case Enum(name, _, _)      => validateTypeName[F, G](name)
      case Scalar(name, _, _, _) => validateTypeName[F, G](name)
    }
  }

  def validateArg[F[_], G[_]](arg: Arg[?], discovery: SchemaShape.DiscoveryState[F])(implicit
      G: Monad[G],
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    allUnique[F, G](DuplicateArg.apply, arg.entries.toList.map(_.name)) >> {
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
              case Left(errs) =>
                errs.traverse_ { err =>
                  val suf = err.position.position.collect { case PreparedQuery.PrepEdge.ASTEdge(x) => x }
                  raise(InvalidInput(err), suf)
                }
              case Right(_) => G.unit
            }
          }

      checkArgsF >>
        arg.entries.traverse_ { entry =>
          useEdge(Edge.Arg(entry.name)) {
            validateFieldName[F, G](entry.name) >> validateInput[F, G](entry.input.value, discovery)
          }
        }
    }

  def validateFields[F[_], G[_]: Monad](fields: NonEmptyList[(String, AbstractField[F, ?, ?])], discovery: SchemaShape.DiscoveryState[F])(
      implicit S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    allUnique[F, G](DuplicateField.apply, fields.toList.map { case (name, _) => name }) >>
      fields.traverse_ { case (name, field) =>
        useEdge(Edge.Field(name)) {
          validateFieldName[F, G](name) >>
            validateArg[F, G](field.arg, discovery) >>
            validateOutput[F, G](field.output.value, discovery)
        }
      }

  def validateToplevel[F[_], G[_]](tl: OutToplevel[F, ?], discovery: SchemaShape.DiscoveryState[F])(implicit
      G: Monad[G],
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] = {
    useOutputEdge[F, G](tl, discovery) {
      validateTypeName[F, G](tl.name) >> {
        def checkOl(ol: ObjectLike[F, ?]): G[Unit] = {
          val uniqF = allUnique[F, G](DuplicateInterfaceInstance.apply, ol.implementsMap.values.toList.map(_.value.name))
          val fieldsF = validateFields[F, G](ol.abstractFieldsNel, discovery)

          val implements = ol.implementsMap.values.toList

          // there must be no transitive interface implementations
          val transitiveInterfaceF = {
            val explicit = ol.implementsMap.keySet
            implements.traverse_ { e =>
              val transitive = e.value.implementsMap.keySet
              val ys = transitive -- explicit
              if (ys.nonEmpty) raise[F, G](TransitiveInterfacesNotImplemented(tl.name, ys.toList tupleLeft e.value.name))
              else G.unit
            }
          }
          // fields must be supersets of the interface fields
          // TODO
          implements.traverse_ { i =>
            i.value.fields.traverse_ { case (k, v) =>
              ol.abstractFieldMap.get(k) match {
                case None =>
                  raise[F, G](Error.MissingInterfaceFields(tl.name, i.value.name, k, PreparedQuery.friendlyName(v.output.value)))
                case Some(f) =>
                  val actualStr = PreparedQuery.friendlyName(v.output.value)
                  val expectedStr = PreparedQuery.friendlyName(f.output.value)
                  val verifyFieldTypeF =
                    if (actualStr != expectedStr)
                      raise[F, G](Error.WrongInterfaceFieldType(tl.name, i.value.name, k, expectedStr, actualStr))
                    else G.unit

                  val actualArg = f.arg
                  val expectedArg = v.arg

                  verifyFieldTypeF
              }
            }
          }

          uniqF >> fieldsF >> transitiveInterfaceF
        }

        tl match {
          case Union(_, types, _) =>
            val ols = types.toList.map(_.tpe)

            allUnique[F, G](DuplicateUnionInstance.apply, ols.map(_.value.name)) >>
              ols.traverse_(x => validateOutput[F, G](x.value, discovery))
          // TODO on both (interface extension)
          case t @ Type(_, _, _, _)      => checkOl(t)
          case i @ Interface(_, _, _, _) => checkOl(i)
          case Enum(_, _, _)             => G.unit
          case Scalar(_, _, _, _)        => G.unit
        }
      }
    }
  }

  def validateOutput[F[_], G[_]: Monad](tl: Out[F, ?], discovery: SchemaShape.DiscoveryState[F])(implicit
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    tl match {
      case x: OutToplevel[F, ?] => validateToplevel[F, G](x, discovery)
      case OutArr(of, _, _)     => validateOutput[F, G](of.asInstanceOf[Out[F, Any]], discovery)
      case OutOpt(of, _)        => validateOutput[F, G](of.asInstanceOf[Out[F, Any]], discovery)
    }
}
