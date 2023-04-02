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
package gql.client

import gql.parser.TypeSystemAst._
import cats._
import cats.implicits._
import cats.data._
import gql.ast._
import gql.parser.{Value => V}
import gql._
import gql.resolver.Resolver
import fs2.Pure

object QueryValidation {
  def astToSchema(ast: List[LowLevelAstNode]): EitherNec[String, SchemaShape[fs2.Pure, ?, ?, ?]] = {
    val outs = ast.collect { case Left(x) => x }
    val ins = ast.collect { case Right(x) => x }

    val outM = outs
      .collect { case t: Type[fs2.Pure, ?] => t }
      .map(x => x.name -> x)
      .toMap

    outM
      .get("Query")
      .toRightNec("Could not find a Query type of type object")
      .map { q =>
        (outM.get("Mutation"), outM.get("Subscription")) match {
          case (m: Option[Type[fs2.Pure, m]], s: Option[Type[fs2.Pure, s]]) => SchemaShape(q, m, s, outs, ins)
        }
      }
  }

  /*
   * Stubs the whole ast
   * Does 2 passes of the top-level types
   * 1. Verify that all types only reference other types that are defined in the ast
   * 2. Construct an ast, where references are call by name and generated ad-hoc:
   * ```
   * lazy val output: Map[String, Either[InToplevel[?], OutToplevel[fs2.Pure, ?]]] = {
   *   ...
   *   Eval.always(output(fieldTypename).toOption.get)
   * }
   * ```
   */
  type LowLevelAstNode = Either[OutToplevel[fs2.Pure, ?], InToplevel[?]]
  def liftAst(ast: Map[String, TypeDefinition]): EitherNec[String, List[LowLevelAstNode]] = {
    sealed trait Partition { def x: TypeDefinition }

    sealed trait InputPartition extends Partition
    case class InputType(x: TypeDefinition.InputObjectTypeDefinition) extends InputPartition

    sealed trait OutputPartition extends Partition
    case class ObjectType(x: TypeDefinition.ObjectTypeDefinition) extends OutputPartition
    case class InterfaceType(x: TypeDefinition.InterfaceTypeDefinition) extends OutputPartition
    case class UnionType(x: TypeDefinition.UnionTypeDefinition) extends OutputPartition

    case class EnumType(x: TypeDefinition.EnumTypeDefinition) extends OutputPartition with InputPartition
    case class ScalarType(x: TypeDefinition.ScalarTypeDefinition) extends OutputPartition with InputPartition

    def partition(td: TypeDefinition): Partition = td match {
      case x: TypeDefinition.InputObjectTypeDefinition => InputType(x)
      case x: TypeDefinition.ObjectTypeDefinition      => ObjectType(x)
      case x: TypeDefinition.InterfaceTypeDefinition   => InterfaceType(x)
      case x: TypeDefinition.UnionTypeDefinition       => UnionType(x)
      case x: TypeDefinition.EnumTypeDefinition        => EnumType(x)
      case x: TypeDefinition.ScalarTypeDefinition      => ScalarType(x)
    }

    def convertInterface(i: InterfaceType): EitherNec[String, Interface[fs2.Pure, ?]] = {
      val fields = i.x.fieldDefinitions.parTraverse(convertFieldDef)
      val impls = i.x.interfaces.parTraverse(implementation[Unit])
      (fields, impls)
        .parMapN((f, is) =>
          Interface[fs2.Pure, Unit](
            i.x.name,
            f.map { case (k, v) => k -> v.asAbstract },
            is.map(_.implementation)
          )
        )
    }

    def convertObject(o: ObjectType): EitherNec[String, Type[fs2.Pure, ?]] = {
      val i = o.x
      val fields = i.fieldDefinitions.parTraverse(convertFieldDef)
      val impls = i.interfaces.parTraverse(implementation[Unit])
      (fields, impls).parMapN(Type[fs2.Pure, Unit](i.name, _, _))
    }

    def implementation[A](s: String): EitherNec[String, Implementation[Pure, A, ?]] =
      partitionType(s).flatMap {
        case i: InterfaceType =>
          Right(Implementation(Eval.always(convertInterface(i).toOption.get))(_ => Option.empty[A]))
        case _ => s"Expected interface, got object `${s}`".leftNec
      }

    def variant[A](s: String) =
      partitionType(s).flatMap {
        case i: ObjectType =>
          Right(Variant(Eval.always(convertObject(i).toOption.get))((_: Unit) => None))
        case _ => s"Expected object type, got something else `${s}`".leftNec
      }

    def convertScalar(s: ScalarType): Scalar[Unit] =
      Scalar(s.x.name, _ => V.NullValue(), _ => Right(()))

    def convertEnum(e: EnumType): Enum[Unit] =
      Enum(e.x.name, e.x.values.map(x => x.name -> EnumValue(())))

    def convertOutputPart(p: OutputPartition): EitherNec[String, OutToplevel[fs2.Pure, ?]] =
      p match {
        case ObjectType(i)    => convertObject(ObjectType(i))
        case InterfaceType(i) => convertInterface(InterfaceType(i))
        case UnionType(x) =>
          val variants = x.types.parTraverse(variant[Unit])
          variants.map(Union[fs2.Pure, Unit](x.name, _))
        case x: EnumType   => Right(convertEnum(x))
        case x: ScalarType => Right(convertScalar(x))
      }

    def convertFieldDef(fd: FieldDefinition): EitherNec[String, (String, Field[fs2.Pure, Unit, ?])] = {
      val o = fd.argumentsDefinition.toNel.traverse(liftArgs)

      val inner = resolveOutputType(fd.tpe)

      (o, inner).parMapN { case (a, i: Eval[Out[fs2.Pure, a]]) =>
        val fail = Resolver.id[fs2.Pure, Unit].as(Ior.left[String, a]("Not implemented")).rethrow
        Field(a.fold(fail)(Resolver.argument[fs2.Pure, Unit, Unit](_) andThen fail), i)
      } tupleLeft fd.name
    }

    def liftArg(a: InputValueDefinition): EitherNec[String, ArgValue[?]] = {
      val t = resolveInputType(a.tpe)
      t.map { case i: Eval[In[a]] => ArgValue(a.name, i, a.defaultValue, None) }
    }

    def liftArgs(xs: NonEmptyList[InputValueDefinition]): EitherNec[String, Arg[Unit]] =
      xs.traverse(liftArg).map(_.nonEmptyTraverse_(x => Arg.make(x)))

    def convertInputPart(p: InputPartition): EitherNec[String, InToplevel[?]] = p match {
      case InputType(i)  => liftArgs(i.inputFields).map(Input(i.name, _))
      case x: EnumType   => Right(convertEnum(x))
      case x: ScalarType => Right(convertScalar(x))
    }

    def partitionType(name: String): EitherNec[String, Partition] =
      ast
        .get(name)
        .toRight(NonEmptyChain.one(s"Type `${name}` not found"))
        .map(partition)

    def resolveStackedType(t: gql.parser.Type): EitherNec[String, (String, Either[Eval[Out[fs2.Pure, ?]], Eval[In[?]]])] = {
      val t2 = ModifierStack.fromType(t).invert
      partitionType(t2.inner)
        .map {
          case o: OutputPartition =>
            Left(Eval.always(foldOutputStack(t2.set(convertOutputPart(o).toOption.get))))
          case i: InputPartition =>
            Right(Eval.always(InverseModifierStack.toIn(t2.set(convertInputPart(i).toOption.get))))
        } tupleLeft t2.inner
    }

    def resolveOutputType(t: gql.parser.Type): EitherNec[String, Eval[Out[fs2.Pure, ?]]] =
      resolveStackedType(t)
        .flatMap {
          case (name, Right(_)) => s"Expected output type, got input type `${name}`".leftNec
          case (_, Left(o))     => Right(o)
        }

    def resolveInputType(t: gql.parser.Type): EitherNec[String, Eval[In[?]]] =
      resolveStackedType(t)
        .flatMap {
          case (name, Left(_)) => s"Expected input type, got output type `${name}`".leftNec
          case (_, Right(i))   => Right(i)
        }

    def foldOutputStack(ms: InverseModifierStack[OutToplevel[fs2.Pure, ?]]): Out[fs2.Pure, ?] =
      ms.modifiers match {
        case InverseModifier.List :: xs =>
          foldOutputStack(InverseModifierStack(xs, ms.inner)) match {
            case e: Out[fs2.Pure, a] => OutArr[fs2.Pure, a, Unit, a](e, _ => Seq.empty[a], Resolver.id[fs2.Pure, a])
          }
        case InverseModifier.Optional :: xs =>
          foldOutputStack(InverseModifierStack(xs, ms.inner)) match {
            case e: Out[fs2.Pure, a] => OutOpt[fs2.Pure, a, a](e, Resolver.id[fs2.Pure, a])
          }
        case Nil => ms.inner
      }

    ast.values.toList.map(partition).parTraverse {
      case o: OutputPartition => convertOutputPart(o).map(_.asLeft)
      case i: InputPartition  => convertInputPart(i).map(_.asRight)
    }
  }
}
