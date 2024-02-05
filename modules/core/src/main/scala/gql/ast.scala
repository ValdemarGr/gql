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

import cats.implicits._
import io.circe._
import cats._
import cats.data._
import gql.resolver._
import java.util.UUID
import gql.parser.{Value => V, Const}
import scala.collection.immutable._

/** A tree-like structure representing a GraphQL schema, akin to most schema builders.
  */
object ast extends AstImplicits.Implicits {
  sealed trait Out[+F[_], A]

  sealed trait In[A]

  sealed trait Toplevel[+F[_], +A] {
    def name: String
    def description: Option[String]
  }

  sealed trait OutToplevel[+F[_], A] extends Out[F, A] with Toplevel[F, A]

  sealed trait InToplevel[A] extends In[A] with Toplevel[fs2.Pure, A]

  sealed trait Selectable[+F[_], A] extends OutToplevel[F, A] {
    def anyFields: List[(String, AnyField[F, ?, ?])]

    lazy val abstractFields: List[(String, AbstractField[F, ?])] =
      anyFields.map { case (k, v) => (k, v.asAbstract) }

    lazy val abstractFieldMap: Map[String, AbstractField[F, ?]] =
      abstractFields.toMap
  }

  sealed trait ObjectLike[+F[_], A] extends Selectable[F, A] {
    def implementsMap: Map[String, Eval[Interface[F, ?]]]

    def abstractFieldsNel: NonEmptyList[(String, AbstractField[F, ?])]
  }

  trait ImplementationAttribute[+F[_]]
  final case class Implementation[+F[_], A, B](
      implementation: Eval[Interface[F, B]],
      attributes: List[ImplementationAttribute[F]] = Nil
  )(val specify: B => Ior[String, Option[A]])

  trait TypeAttribute[+F[_], A]
  final case class Type[+F[_], A](
      name: String,
      fields: NonEmptyList[(String, Field[F, A, ?])],
      implementations: List[Implementation[F, A, ?]],
      description: Option[String] = None,
      attributes: List[TypeAttribute[F, A]] = Nil
  ) extends ObjectLike[F, A] {
    def document(description: String): Type[F, A] = copy(description = Some(description))

    def addAttributes[F2[x] >: F[x]](attrs: TypeAttribute[F2, A]*): Type[F2, A] =
      copy[F2, A](attributes = attributes ++ attrs.toList)

    lazy val fieldsList: List[(String, Field[F, A, ?])] = fields.toList

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    lazy val concreteFields = fieldsList

    lazy val concreteFieldsMap = fieldMap

    lazy val implementsMap = implementations.map(i => i.implementation.value.name -> i.implementation).toMap

    lazy val abstractFieldsNel = fields.map { case (k, v) => k -> v.asAbstract }

    override lazy val anyFields = fields.toList
  }

  final case class Input[A](
      name: String,
      fields: Arg[A],
      description: Option[String] = None
  ) extends InToplevel[A] {
    def document(description: String): Input[A] = copy(description = Some(description))
  }

  trait VariantAttribute[+F[_]]
  final case class Variant[+F[_], A, B](
      tpe: Eval[Type[F, B]],
      attributes: List[VariantAttribute[F]] = Nil
  )(val specify: A => Ior[String, Option[B]]) {
    def contramap[C](g: C => A): Variant[F, C, B] =
      Variant[F, C, B](tpe, attributes)(c => specify(g(c)))
  }

  final case class Union[+F[_], A](
      name: String,
      types: NonEmptyList[Variant[F, A, ?]],
      description: Option[String] = None
  ) extends Selectable[F, A] {
    def document(description: String): Union[F, A] = copy(description = Some(description))

    def contramap[B](f: B => A): Union[F, B] =
      Union(name, types.map(_.contramap(f)), description)

    lazy val instanceMap = types.map(i => i.tpe.value.name -> i).toList.toMap

    lazy val fieldMap = Map.empty

    lazy val fieldsList: List[(String, Field[F, A, ?])] = Nil

    val anyFields = Nil
  }

  final case class Interface[+F[_], A](
      name: String,
      fields: NonEmptyList[(String, AnyField[F, A, ?])],
      implementations: List[Eval[Interface[F, ?]]],
      description: Option[String] = None
  ) extends ObjectLike[F, A] {
    def document(description: String): Interface[F, A] = copy(description = Some(description))

    lazy val abstractFieldsNel = fields.map { case (k, v) => k -> v.asAbstract }

    override lazy val anyFields = fields.toList

    lazy val implementsMap = implementations.map(i => i.value.name -> i).toMap
  }

  final case class Scalar[A](
      name: String,
      encoder: A => V[Const, Unit],
      decoder: V[Const, Unit] => Either[String, A],
      description: Option[String] = None
  ) extends OutToplevel[fs2.Pure, A]
      with InToplevel[A] {
    def document(description: String): Scalar[A] = copy(description = Some(description))

    def eimap[B](f: A => Either[String, B])(g: B => A): Scalar[B] =
      Scalar(name, encoder.compose(g), decoder.andThen(_.flatMap(f)), description)

    def rename(newName: String): Scalar[A] = copy(name = newName)
  }

  final case class EnumValue[A](
      value: A,
      description: Option[String] = None
  ) {
    def document(description: String): EnumValue[A] = copy(description = Some(description))
  }

  final case class Enum[A](
      name: String,
      mappings: NonEmptyList[(String, EnumValue[? <: A])],
      directives: List[SchemaDirective[Nothing, Position.Enum]] = Nil,
      description: Option[String] = None
  ) extends OutToplevel[fs2.Pure, A]
      with InToplevel[A] {
    def document(description: String): Enum[A] = copy(description = Some(description))

    lazy val kv = mappings.map { case (k, v) => k -> v.value }

    lazy val m = kv.toNem

    lazy val revm = kv.map(_.swap).toList.toMap
  }

  sealed trait AnyField[+F[_], -A, B] {
    def output: Eval[Out[F, B]]

    def asAbstract: AbstractField[F, B]
  }

  trait FieldAttribute[+F[_]]
  final case class Field[+F[_], -A, B](
      resolve: Resolver[F, A, B],
      output: Eval[Out[F, B]],
      description: Option[String] = None,
      attributes: List[FieldAttribute[F]] = Nil
  ) extends AnyField[F, A, B] {
    def document(description: String): Field[F, A, B] = copy(description = Some(description))

    lazy val asAbstract: AbstractField[F, B] =
      AbstractField(
        NonEmptyChain
          .fromChain {
            def collectFields[G[_]](step: Step[G, ?, ?]): Chain[Arg[Any]] =
              step match {
                case Step.Alg.Argument(a)   => Chain.one(a)
                case Step.Alg.First(s)      => collectFields(s)
                case Step.Alg.Choose(l, r)  => collectFields(l) ++ collectFields(r)
                case Step.Alg.Compose(l, r) => collectFields(l) ++ collectFields(r)
                case _                      => Chain.empty
              }
            collectFields(resolve.underlying)
          }
          .map(_.nonEmptySequence),
        output,
        description
      )

    def compose[F2[x] >: F[x], A2](r: Resolver[F2, A2, A]): Field[F2, A2, B] =
      copy[F2, A2, B](r.andThen(resolve))

    def contramap[F2[x] >: F[x], A2](f: A2 => A): Field[F2, A2, B] =
      compose[F2, A2](Resolver.lift[F2, A2](f))

    def addAttributes[F2[x] >: F[x]](attrs: FieldAttribute[F2]*): Field[F2, A, B] =
      copy(attributes = attributes ++ attrs.toList)
  }

  // Field, but without any implementation
  final case class AbstractField[+F[_], T](
      arg: Option[Arg[?]],
      output: Eval[Out[F, T]],
      description: Option[String] = None
  ) extends AnyField[F, Any, T] {
    def document(description: String): AbstractField[F, T] = copy(description = Some(description))

    def asAbstract = this
  }

  final case class OutOpt[+F[_], A, B](of: Out[F, B], resolver: Resolver[F, A, B]) extends Out[F, Option[A]]

  final case class OutArr[+F[_], A, C, B](of: Out[F, B], toSeq: C => Seq[A], resolver: Resolver[F, A, B]) extends Out[F, C] {
    def contramap[D](f: D => C): OutArr[F, A, D, B] = OutArr(of, f.andThen(toSeq), resolver)
  }

  final case class InOpt[A](of: In[A]) extends In[Option[A]]

  // This can be a bit hard to read
  // For every element in an input array [I1, I2, ...] decode with of such that we have [A1, A2, ...],
  // then map [A1, A2, ...] into C (which could be another datatype, for example a non-empty container)
  final case class InArr[A, C](of: In[A], fromSeq: Seq[A] => Either[String, C]) extends In[C] {
    def emap[B](f: C => Either[String, B]): InArr[A, B] =
      InArr(of, fromSeq.andThen(_.flatMap(f)))

    def map[B](f: C => B): InArr[A, B] =
      InArr(of, fromSeq.andThen(_.map(f)))
  }

  object Scalar {
    def fromCirce[A](name: String)(implicit enc: Encoder[A], dec: Decoder[A]): Scalar[A] = {
      import io.circe.syntax._
      Scalar(
        name,
        a => V.fromJson(enc(a)),
        value =>
          dec.decodeJson(value.asJson).leftMap { case df: io.circe.DecodingFailure =>
            val maybeAt = if (df.history.size > 1) s" at ${io.circe.CursorOp.opsToPath(df.history)}" else ""
            s"decoding failure for type `$name`$maybeAt with message ${df.message}"
          }
      )
    }

    implicit lazy val invariantForScalar: Invariant[Scalar] = new Invariant[Scalar] {
      override def imap[A, B](fa: Scalar[A])(f: A => B)(g: B => A): Scalar[B] =
        Scalar(fa.name, fa.encoder.compose(g), fa.decoder.andThen(_.map(f)), fa.description)
    }
  }

  final case class ID[A](value: A) extends AnyVal
  object ID extends IDLowPrio {
    implicit def idTpe[A](implicit s: Scalar[A]): Scalar[ID[A]] =
      s.imap(ID(_))(_.value)
        .rename("ID")
        .document(
          """|The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache.
             |The ID type appears in a JSON response as a String; however, it is not intended to be human-readable.
             |When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."""".stripMargin
        )

  }
  trait IDLowPrio {
    implicit def idIn[A](implicit s: Scalar[A]): In[ID[A]] = ID.idTpe[A]
  }

  
  // TypeSystemDirectiveLocation
  final case class SchemaDirective[+F[_], P[x] <: Position[F, x]](
    position: P[?],
    args: List[(String, V[Const, Unit])]
  )
}

object AstImplicits {
  import ast._

  trait Implicits extends LowPriorityImplicits {
    implicit lazy val stringScalar: Scalar[String] = Scalar
      .fromCirce[String]("String")
      .document("The `String` is a UTF-8 character sequence usually representing human-readable text.")

    implicit lazy val intScalar: Scalar[Int] = Scalar
      .fromCirce[Int]("Int")
      .document(
        "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
      )

    implicit lazy val longScalar: Scalar[Long] = Scalar
      .fromCirce[Long]("Long")
      .document(
        "The `Long` scalar type represents non-fractional signed whole numeric values. Long can represent values between -(2^63) and 2^63 - 1."
      )

    implicit lazy val floatScalar: Scalar[Float] = Scalar
      .fromCirce[Float]("Float")
      .document(
        "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      )

    implicit lazy val doubleScalar: Scalar[Double] = Scalar
      .fromCirce[Double]("Double")
      .document(
        "The `Double` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      )

    implicit lazy val bigIntScalar: Scalar[BigInt] = Scalar
      .fromCirce[BigInt]("BigInt")
      .document(
        "The `BigInt` scalar type represents non-fractional signed whole numeric values. BigInt can represent values of arbitrary size."
      )

    implicit lazy val bigDecimalScalar: Scalar[BigDecimal] = Scalar
      .fromCirce[BigDecimal]("BigDecimal")
      .document(
        "The `BigDecimal` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      )

    implicit lazy val booleanScalar: Scalar[Boolean] = Scalar
      .fromCirce[Boolean]("Boolean")
      .document("The `Boolean` scalar type represents `true` or `false`.")

    implicit lazy val uuidScalar: Scalar[UUID] = Scalar
      .fromCirce[UUID]("UUID")
      .document(
        "The `UUID` scalar type represents a UUID v4 as specified by [RFC 4122](https://tools.ietf.org/html/rfc4122)."
      )

    implicit def gqlInForOption[A](implicit tpe: In[A]): In[Option[A]] = InOpt(tpe)

    implicit def gqlOutForOption[F[_], A](implicit tpe: Out[F, A]): OutOpt[F, A, A] =
      OutOpt(tpe, Resolver.id)
  }

  trait LowPriorityImplicits {
    implicit def gqlInForSeq[A](implicit tpe: In[A]): In[Seq[A]] = InArr[A, Seq[A]](tpe, _.asRight)
    implicit def gqlInForList[A](implicit tpe: In[A]): In[List[A]] = InArr[A, List[A]](tpe, _.toList.asRight)
    implicit def gqlInForVector[A](implicit tpe: In[A]): In[Vector[A]] = InArr[A, Vector[A]](tpe, _.toVector.asRight)
    implicit def gqlInForSet[A](implicit tpe: In[A]): In[Set[A]] = InArr[A, Set[A]](tpe, _.toSet.asRight)
    implicit def gqlInForNonEmptyList[A](implicit tpe: In[A]): In[NonEmptyList[A]] =
      InArr[A, NonEmptyList[A]](tpe, _.toList.toNel.toRight("expected non-empty array, but array was empty"))
    implicit def gqlInForNonEmptyVector[A](implicit tpe: In[A]): In[NonEmptyVector[A]] =
      InArr[A, NonEmptyVector[A]](tpe, _.toVector.toNev.toRight("expected non-empty array, but array was empty"))
    implicit def gqlInForChain[A](implicit tpe: In[A]): In[Chain[A]] =
      InArr[A, Chain[A]](tpe, xs => Chain.fromSeq(xs).asRight)
    implicit def gqlInForNonEmptyChain[A](implicit tpe: In[A]): In[NonEmptyChain[A]] =
      InArr[A, NonEmptyChain[A]](tpe, xs => NonEmptyChain.fromSeq(xs).toRight("expected non-empty array, but array was empty"))

    implicit def gqlOutArrForTraversable[F[_], A, G[_]: Traverse](implicit tpe: Out[F, A]): OutArr[F, A, G[A], A] =
      OutArr(tpe, _.toList, Resolver.id)
  }
}
