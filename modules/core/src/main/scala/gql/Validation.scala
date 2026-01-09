/*
 * Copyright 2023 Valdemar Grange
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package gql

import cats.data._
import cats._
import cats.implicits._
import cats.mtl._
import gql.ast._
import gql.parser.GraphqlParser
import gql.preparation.PositionalError
import gql.preparation._
import gql.preparation.FieldMerging

object Validation {
  sealed trait Error {
    def message: String
  }
  object Error {
    final case class DuplicateTypenameInInputAndOutput(typename: String) extends Error {
      def message: String =
        s"Typename `$typename` appears both as input and output type. You must rename one of them such as `Input$typename`."
    }
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
      def message: String =
        s"""|Duplicate arg with different structure `$conflict`.
            |Duplicate args are allowed but they must be equal (==).
            |Otherwise two args of same name and type may cause ambiguity if for instance two args have different (potentially mutually exclusive) parsers, or different descriptions.
            |Define your arg as `val` or `lazy val` to get rid of this error.""".stripMargin
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
    final case class InvalidInput(pe: PositionalError[Unit]) extends Error {
      def message: String = s"Invalid argument input: ${pe.message}"
    }
    final case class InputNoArgs(name: String) extends Error {
      def message: String = s"Input `$name` has no arguments."
    }
    final case class MissingInterfaceFields(
        typename: String,
        interfaceName: String,
        fieldName: String,
        fieldType: String
    ) extends Error {
      def message: String =
        s"Type `$typename` does not implement all of the fields defined in interface `$interfaceName`, missing field '$fieldName' of type `$fieldType`."
    }
    final case class CyclicInterfaceImplementation(typename: String) extends Error {
      def message: String = s"`$typename` is an interface implementation of itself."
    }
    final case class TransitiveInterfacesNotImplemented(typename: String, interfaces: List[(String, String)]) extends Error {
      def message: String =
        s"`$typename` does not implement all interfaces: ${interfaces
            .map { case (through, name) => s"`$name` through `$through`" }
            .mkString(" and ")}."
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
    final case class ArgumentNotDefinedInInterface(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        argName: String
    ) extends Error {
      def message: String =
        s"Argument '$argName' was defined in field '$fieldName' in type `$typename` but was not defined in interface `$sourceInterface`."
    }
    final case class MissingInterfaceFieldArgument(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        argName: String
    ) extends Error {
      def message: String =
        s"Argument '$argName' was defined in field '$fieldName' in interface `$sourceInterface` but was not defined in type `$typename`."
    }
    final case class InterfaceImplementationWrongArgType(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        argName: String,
        expected: String,
        actual: String
    ) extends Error {
      def message: String =
        s"Argument '$argName' in field '$fieldName' in type `$typename` is of type `$actual` but expected `$expected` from interface `$sourceInterface`."
    }
    final case class InterfaceDoesNotDefineDefaultArg(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        argName: String
    ) extends Error {
      def message: String =
        s"The argument '$argName' in field '$fieldName' in type `$typename` has a default value, but the interface `$sourceInterface` does not define a default value."
    }
    final case class InterfaceImplementationMissingDefaultArg(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        argName: String
    ) extends Error {
      def message: String =
        s"The argument '$argName' in field '$fieldName' in type `$typename` does not have a default value, but the interface `$sourceInterface` defines a default value."
    }
    final case class InterfaceImplementationDefaultArgDoesNotMatch(
        typename: String,
        sourceInterface: String,
        fieldName: String,
        argName: String,
        msg: String
    ) extends Error {
      def message: String =
        s"The default value of the argument '$argName' in field '$fieldName' in type `$typename` is not equal to the one defined in `$sourceInterface`. The error found was: $msg"
    }
  }

  final case class Problem(
      error: Error,
      path: Cursor
  ) {
    override def toString() =
      s"${error.message} at ${path.formatted}"
  }

  final case class ValidationState[F[_]](
      problems: Chain[Problem],
      currentPath: Cursor,
      seenOutputs: Map[String, OutToplevel[F, ?]],
      seenInputs: Map[String, InToplevel[?]]
  ) {
    def addPath(arc: GraphArc): ValidationState[F] =
      copy(currentPath = currentPath add arc)
  }
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
        .runS(ValidationState(Chain.empty, Cursor.empty, Map.empty, Map.empty))
        .value
        .problems
        .toList
        .distinct
    }
  }

  def raise[F[_], G[_]](err: Error, suffix: Cursor = Cursor.empty)(implicit
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    S.modify(s => s.copy(problems = s.problems :+ Problem(err, s.currentPath |+| suffix)))

  def useEdge[F[_], G[_], A](edge: GraphArc)(
      fa: G[A]
  )(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[A] =
    S.get.flatMap { s =>
      S.set(s.copy(currentPath = s.currentPath add edge)) *>
        fa <*
        S.modify(_.copy(currentPath = s.currentPath))
    }

  def getDiscovery[F[_]](name: String, discovery: SchemaShape.DiscoveryState[F]) =
    (discovery.inputs.get(name), discovery.outputs.get(name))

  def useOutputEdge[F[_], G[_]](sel: Selectable[F, ?], discovery: SchemaShape.DiscoveryState[F])(
      fa: G[Unit]
  )(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    useEdge(GraphArc.Field(sel.name)) {
      S.inspect(_.seenOutputs.get(sel.name)).flatMap {
        case Some(o) if o eq sel => G.unit
        case Some(_)             => raise(CyclicDivergingTypeReference(sel.name))
        case None =>
          val checkF = getDiscovery(sel.name, discovery) match {
            case (Some(_), _)                   => raise(DuplicateTypenameInInputAndOutput(sel.name))
            case (None, Some(o)) if !(o eq sel) => raise(DivergingTypeReference(sel.name))
            case _                              => G.unit
          }

          checkF >>
            S.modify(s => s.copy(seenOutputs = s.seenOutputs + (sel.name -> sel))) *>
            fa <*
            S.modify(s => s.copy(seenOutputs = s.seenOutputs - sel.name))
      }
    }

  def useInputEdge[F[_], G[_]](it: InToplevel[?], discovery: SchemaShape.DiscoveryState[F])(
      fa: G[Unit]
  )(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    useEdge(GraphArc.Field(it.name)) {
      S.inspect(_.seenInputs.get(it.name)).flatMap {
        case Some(i) if i eq it => G.unit
        case Some(_)            => raise(CyclicDivergingTypeReference(it.name))
        case None =>
          val checkF = getDiscovery(it.name, discovery) match {
            case (_, Some(_))                  => raise(DuplicateTypenameInInputAndOutput(it.name))
            case (Some(i), None) if !(i eq it) => raise(DivergingTypeReference(it.name))
            case _                             => G.unit
          }

          checkF >>
            S.modify(s => s.copy(seenInputs = s.seenInputs + (it.name -> it))) *>
            fa <*
            S.modify(s => s.copy(seenInputs = s.seenInputs - it.name))
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
    GraphqlParser.name.parseAll(name) match {
      case Left(_)  => raise(InvalidTypeName(name))
      case Right(_) => G.unit
    }

  def validateFieldName[F[_], G[_]](name: String)(implicit G: Monad[G], S: Stateful[G, ValidationState[F]]): G[Unit] =
    GraphqlParser.name.parseAll(name) match {
      case Left(_)  => raise(InvalidFieldName(name))
      case Right(_) => G.unit
    }

  def validateInput[F[_], G[_]](input: In[?], discovery: SchemaShape.DiscoveryState[F])(implicit
      G: Monad[G],
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    input match {
      case InArr(of, _) => validateInput[F, G](of, discovery)
      case InOpt(of)    => validateInput[F, G](of, discovery)
      case t @ Input(name, fields, _) =>
        useInputEdge(t, discovery) {
          val validateNonEmptyF =
            if (fields.entries.isEmpty) raise(InputNoArgs(name))
            else G.unit

          validateNonEmptyF *>
            validateTypeName[F, G](name) *>
            validateArg[F, G](fields, discovery)
        }
      case Enum(name, _, _)      => validateTypeName[F, G](name)
      case Scalar(name, _, _, _) => validateTypeName[F, G](name)
    }

  def validateArg[F[_], G[_]](arg: Arg[?], discovery: SchemaShape.DiscoveryState[F])(implicit
      G: Monad[G],
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    allUnique[F, G](
      DuplicateArg.apply,
      // We abuse scala universal equals here
      arg.entries.toList.distinct.map(_.name)
    ) >> {

      // A trick;
      // We check the arg like we would in a user-supplied query
      // Except, we use default as the "input" such that it is verified against the arg
      val checkArgsF =
        arg.entries.toChain
          .mapFilter(x => x.defaultValue.tupleLeft(x))
          .traverse_[G, Unit] { case (a: ArgValue[a], pv) =>
            (new ArgParsing[Unit](Map.empty))
              .decodeIn[a](a.input.value, pv.map(List(_)), ambigiousEnum = false)
              .runToCompletion(Map.empty) match {
              case Left(errs) =>
                errs.traverse_ { err =>
                  val suf = err.position
                  raise(InvalidInput(err), suf)
                }
              case Right(_) => G.unit
            }
          }

      checkArgsF >>
        arg.entries.traverse_ { entry =>
          useEdge(GraphArc.Field(entry.name)) {
            validateFieldName[F, G](entry.name) >> validateInput[F, G](entry.input.value, discovery)
          }
        }
    }

  def validateFields[F[_], G[_]: Monad](fields: NonEmptyList[(String, AbstractField[F, ?])], discovery: SchemaShape.DiscoveryState[F])(
      implicit S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    allUnique[F, G](DuplicateField.apply, fields.toList.map { case (name, _) => name }) >>
      fields.traverse_ { case (name, field) =>
        useEdge(GraphArc.Field(name)) {
          validateFieldName[F, G](name) >>
            field.arg.traverse_(validateArg[F, G](_, discovery)) >>
            validateOutput[F, G](field.output.value, discovery)
        }
      }

  def validateToplevel[F[_], G[_]](sel: Selectable[F, ?], discovery: SchemaShape.DiscoveryState[F])(implicit
      G: Monad[G],
      S: Stateful[G, ValidationState[F]]
  ): G[Unit] =
    useOutputEdge[F, G](sel, discovery) {
      validateTypeName[F, G](sel.name) >> {
        def checkOl(ol: ObjectLike[F, ?]): G[Unit] = {
          val uniqF: G[Unit] =
            allUnique[F, G](DuplicateInterfaceInstance.apply, ol.implementsMap.values.toList.map(_.value.name))
          val fieldsF = validateFields[F, G](ol.abstractFieldsNel, discovery)

          val implements = ol.implementsMap.values.toList

          // there must be no transitive interface implementations
          val transitiveInterfaceF = {
            val explicit = ol.implementsMap.keySet
            implements.traverse_ { e =>
              val transitive = e.value.implementsMap.keySet
              val ys = transitive -- explicit
              if (ys.nonEmpty)
                raise[F, G](TransitiveInterfacesNotImplemented(sel.name, ys.toList tupleLeft e.value.name))
              else G.unit
            }
          }
          // fields must be supersets of the interface fields
          val fieldSubtypeConstraintsF = implements.traverse_ { i =>
            i.value.fields.traverse_ { case (k, v) =>
              ol.abstractFieldMap.get(k) match {
                case None =>
                  raise[F, G](
                    Error.MissingInterfaceFields(
                      sel.name,
                      i.value.name,
                      k,
                      ModifierStack
                        .fromOut(v.output.value)
                        .show(_.name)
                    )
                  )
                case Some(f) =>
                  val actualStr = ModifierStack
                    .fromOut(v.output.value)
                    .show(_.name)
                  val expectedStr = ModifierStack
                    .fromOut(f.output.value)
                    .show(_.name)
                  val verifyFieldTypeF =
                    if (actualStr != expectedStr)
                      raise[F, G](Error.WrongInterfaceFieldType(sel.name, i.value.name, k, expectedStr, actualStr))
                    else G.unit

                  val actualArg: Chain[ArgValue[?]] = Chain.fromOption(f.arg).flatMap(_.entries.toChain)
                  val expectedArg: Chain[ArgValue[?]] = Chain.fromOption(v.asAbstract.arg).flatMap(_.entries.toChain)
                  val comb = actualArg.map(x => x.name -> x) align expectedArg.map(x => x.name -> x)
                  val verifyArgsF = comb.traverse_ {
                    // Only actual; shouldn't occur
                    case Ior.Left((argName, _)) =>
                      raise[F, G](Error.ArgumentNotDefinedInInterface(ol.name, i.value.name, k, argName))
                    // Only expected; impl type should implement argument
                    case Ior.Right((argName, _)) =>
                      raise[F, G](Error.MissingInterfaceFieldArgument(ol.name, i.value.name, k, argName))
                    case Ior.Both((argName, l), (_, r)) =>
                      val lName = ModifierStack
                        .fromIn(l.input.value)
                        .show(_.name)
                      val rName = ModifierStack
                        .fromIn(r.input.value)
                        .show(_.name)
                      val verifyTypesF =
                        if (lName != rName)
                          raise[F, G](
                            Error.InterfaceImplementationWrongArgType(ol.name, i.value.name, k, argName, rName, lName)
                          )
                        else G.unit

                      val defaultMatchF = (l.defaultValue, r.defaultValue) match {
                        case (None, None) => G.unit
                        case (Some(_), None) =>
                          raise[F, G](Error.InterfaceDoesNotDefineDefaultArg(ol.name, i.value.name, k, argName))
                        case (None, Some(_)) =>
                          raise[F, G](Error.InterfaceImplementationMissingDefaultArg(ol.name, i.value.name, k, argName))
                        case (Some(ld), Some(rd)) =>
                          (new FieldMerging[Unit])
                            .compareValues(ld, rd, None)
                            .run
                            .swap
                            .toOption
                            .traverse_(_.traverse_ { pe =>
                              val suf = pe.position
                              raise[F, G](
                                Error.InterfaceImplementationDefaultArgDoesNotMatch(
                                  ol.name,
                                  i.value.name,
                                  k,
                                  argName,
                                  pe.message
                                ),
                                suf
                              )
                            })
                      }

                      verifyTypesF >> defaultMatchF
                  }

                  verifyFieldTypeF >> verifyArgsF
              }
            }
          }

          uniqF >> fieldsF >> transitiveInterfaceF >> fieldSubtypeConstraintsF
        }

        sel match {
          case Union(_, types, _) =>
            val ols = types.toList.map(_.tpe)

            allUnique[F, G](DuplicateUnionInstance.apply, ols.map(_.value.name)) >>
              ols.traverse_(x => validateOutput[F, G](x.value, discovery))
          // TODO on both (interface extension)
          case t: Type[F, ?]      => checkOl(t)
          case i: Interface[F, ?] => checkOl(i)
        }
      }
    }

  def validateOutput[F[_], G[_]](tl: Out[F, ?], discovery: SchemaShape.DiscoveryState[F])(implicit
      S: Stateful[G, ValidationState[F]],
      G: Monad[G]
  ): G[Unit] =
    tl match {
      case Enum(_, _, _)       => G.unit
      case Scalar(_, _, _, _)  => G.unit
      case s: Selectable[F, ?] => validateToplevel[F, G](s, discovery)
      case OutArr(of, _, _)    => validateOutput[F, G](of, discovery)
      case o: OutOpt[?, ?, ?]  => validateOutput[F, G](o.of, discovery)
    }
}
