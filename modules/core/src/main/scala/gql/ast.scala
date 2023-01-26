/*
 * Copyright 2022 Valdemar Grange
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

object ast extends AstImplicits.Implicits {
  sealed trait Out[F[_], A]

  sealed trait In[A]

  sealed trait Toplevel[+A] {
    def name: String
    def description: Option[String]
  }

  sealed trait OutToplevel[F[_], A] extends Out[F, A] with Toplevel[A]

  sealed trait InToplevel[A] extends In[A] with Toplevel[A]

  sealed trait Selectable[F[_], A] extends OutToplevel[F, A] {
    def abstractFields: List[(String, AbstractField[F, ?])]

    def abstractFieldMap: Map[String, AbstractField[F, ?]]
  }

  sealed trait ObjectLike[F[_], A] extends Selectable[F, A] {
    def implementsMap: Map[String, Eval[Interface[F, ?]]]

    def abstractFieldsNel: NonEmptyList[(String, AbstractField[F, ?])]
  }

  final case class Implementation[F[_], A, B](implementation: Eval[Interface[F, B]])(implicit
      val specify: B => Option[A]
  )

  final case class Type[F[_], A](
      name: String,
      fields: NonEmptyList[(String, Field[F, A, ?])],
      implementations: List[Implementation[F, A, ?]],
      description: Option[String] = None
  ) extends ObjectLike[F, A] {
    def document(description: String): Type[F, A] = copy(description = Some(description))

    lazy val fieldsList: List[(String, Field[F, A, ?])] = fields.toList

    lazy val fieldMap = fields.toNem.toSortedMap.toMap

    lazy val concreteFields = fieldsList

    lazy val concreteFieldsMap = fieldMap

    lazy val implementsMap = implementations.map(i => i.implementation.value.name -> i.implementation).toMap

    lazy val abstractFieldsNel = fields.map { case (k, v) => k -> v.asAbstract }

    lazy val abstractFields: List[(String, AbstractField[F, ?])] = abstractFieldsNel.toList

    lazy val abstractFieldMap: Map[String, AbstractField[F, ?]] = abstractFields.toMap
  }

  final case class Input[A](
      name: String,
      fields: Arg[A],
      description: Option[String] = None
  ) extends InToplevel[A] {
    def document(description: String): Input[A] = copy(description = Some(description))
  }

  final case class Variant[F[_], A, B](tpe: Eval[Type[F, B]])(implicit val specify: A => Option[B]) {
    def contramap[C](g: C => A): Variant[F, C, B] =
      Variant[F, C, B](tpe)(c => specify(g(c)))
  }

  final case class Union[F[_], A](
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

    lazy val abstractFields = Nil

    lazy val abstractFieldMap = Map.empty
  }

  final case class Interface[F[_], A](
      name: String,
      fields: NonEmptyList[(String, AbstractField[F, ?])],
      implementations: List[Eval[Interface[F, ?]]],
      description: Option[String] = None
  ) extends ObjectLike[F, A] {
    def document(description: String): Interface[F, A] = copy(description = Some(description))

    lazy val abstractFieldsNel = fields

    lazy val abstractFields = fields.toList

    lazy val abstractFieldMap = abstractFields.toList.toMap

    lazy val implementsMap = implementations.map(i => i.value.name -> i).toMap
  }

  final case class Scalar[F[_], A](
      name: String,
      encoder: A => Value,
      decoder: Value => Either[String, A],
      description: Option[String] = None
  ) extends OutToplevel[F, A]
      with InToplevel[A] {
    def document(description: String): Scalar[F, A] = copy(description = Some(description))

    def eimap[B](f: A => Either[String, B])(g: B => A): Scalar[F, B] =
      Scalar(name, encoder.compose(g), decoder.andThen(_.flatMap(f)), description)

    def rename(newName: String): Scalar[F, A] = copy(name = newName)
  }

  final case class EnumValue[A](
      value: A,
      description: Option[String] = None
  ) {
    def document(description: String): EnumValue[A] = copy(description = Some(description))
  }

  final case class Enum[F[_], A](
      name: String,
      mappings: NonEmptyList[(String, EnumValue[? <: A])],
      description: Option[String] = None
  ) extends OutToplevel[F, A]
      with InToplevel[A] {

    def document(description: String): Enum[F, A] = copy(description = Some(description))

    lazy val kv = mappings.map { case (k, v) => k -> v.value }

    lazy val m = kv.toNem

    lazy val revm = kv.map(_.swap).toList.toMap
  }

  final case class Field[F[_], -I, T](
      resolve: Resolver[F, I, T],
      output: Eval[Out[F, T]],
      description: Option[String] = None
  ) {
    def document(description: String): Field[F, I, T] = copy(description = Some(description))

    def asAbstract: AbstractField[F, T] = AbstractField(null, output, description)

    def contramap[I2](f: I2 => I): Field[F, I2, T] =
      Field(resolve.contramap(f), output, description)
  }

  final case class AbstractField[F[_], T](
      arg: Arg[?],
      output: Eval[Out[F, T]],
      description: Option[String] = None
  ) {
    def document(description: String): AbstractField[F, T] = copy(description = Some(description))
  }

  final case class OutOpt[F[_], A, B](of: Out[F, B], resolver: Resolver[F, A, B]) extends Out[F, Option[A]]

  final case class OutArr[F[_], A, C, B](of: Out[F, B], toSeq: C => Seq[A], resolver: Resolver[F, A, B]) extends Out[F, C] {
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
    def fromCirce[F[_], A](name: String)(implicit enc: Encoder[A], dec: Decoder[A]): Scalar[F, A] =
      Scalar(
        name,
        a => Value.fromJson(enc(a)),
        value =>
          dec.decodeJson(value.asJson).leftMap { case df: io.circe.DecodingFailure =>
            val maybeAt = if (df.history.size > 1) s" at ${io.circe.CursorOp.opsToPath(df.history)}" else ""
            s"decoding failure for type $name$maybeAt with message ${df.message}"
          }
      )

    implicit def invariantForScalar[F[_]]: Invariant[Scalar[F, *]] = new Invariant[Scalar[F, *]] {
      override def imap[A, B](fa: Scalar[F, A])(f: A => B)(g: B => A): Scalar[F, B] =
        Scalar(fa.name, fa.encoder.compose(g), fa.decoder.andThen(_.map(f)), fa.description)
    }
  }

  final case class ID[A](value: A) extends AnyVal
  object ID extends IDLowPrio {
    implicit def idTpe[F[_], A](implicit s: Scalar[F, A]): Scalar[F, ID[A]] =
      s.imap(ID(_))(_.value)
        .rename("ID")
        .document(
          """|The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache.
             |The ID type appears in a JSON response as a String; however, it is not intended to be human-readable.
             |When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."""".stripMargin
        )

  }
  trait IDLowPrio {
    implicit def idIn[F[_], A](implicit s: Scalar[F, A]): In[ID[A]] = ID.idTpe[F, A]
  }
}

object AstImplicits {
  import ast._

  trait Implicits extends LowPriorityImplicits {
    implicit def stringScalar[F[_]]: Scalar[F, String] = Scalar
      .fromCirce[F, String]("String")
      .document("The `String` is a UTF-8 character sequence usually representing human-readable text.")

    implicit def intScalar[F[_]]: Scalar[F, Int] = Scalar
      .fromCirce[F, Int]("Int")
      .document(
        "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
      )

    implicit def longScalar[F[_]]: Scalar[F, Long] = Scalar
      .fromCirce[F, Long]("Long")
      .document(
        "The `Long` scalar type represents non-fractional signed whole numeric values. Long can represent values between -(2^63) and 2^63 - 1."
      )

    implicit def floatScalar[F[_]]: Scalar[F, Float] = Scalar
      .fromCirce[F, Float]("Float")
      .document(
        "The `Float` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      )

    implicit def doubleScalar[F[_]]: Scalar[F, Double] = Scalar
      .fromCirce[F, Double]("Double")
      .document(
        "The `Double` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      )

    implicit def bigIntScalar[F[_]]: Scalar[F, BigInt] = Scalar
      .fromCirce[F, BigInt]("BigInt")
      .document(
        "The `BigInt` scalar type represents non-fractional signed whole numeric values. BigInt can represent values of arbitrary size."
      )

    implicit def bigDecimalScalar[F[_]]: Scalar[F, BigDecimal] = Scalar
      .fromCirce[F, BigDecimal]("BigDecimal")
      .document(
        "The `BigDecimal` scalar type represents signed double-precision fractional values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
      )

    implicit def booleanScalar[F[_]]: Scalar[F, Boolean] = Scalar
      .fromCirce[F, Boolean]("Boolean")
      .document("The `Boolean` scalar type represents `true` or `false`.")

    implicit def uuidScalar[F[_]]: Scalar[F, UUID] = Scalar
      .fromCirce[F, UUID]("UUID")
      .document(
        "The `UUID` scalar type represents a UUID v4 as specified by [RFC 4122](https://tools.ietf.org/html/rfc4122)."
      )

    implicit def gqlInForOption[A](implicit tpe: In[A]): In[Option[A]] = InOpt(tpe)

    implicit def gqlOutForOption[F[_], A](implicit tpe: Out[F, A]): OutOpt[F, A, A] =
      OutOpt(tpe, Resolver.pure[F, A, A](identity))
  }

  trait LowPriorityImplicits {
    implicit def gqlInForSeq[A](implicit tpe: In[A]): In[Seq[A]] = InArr[A, Seq[A]](tpe, _.asRight)
    implicit def gqlInForList[A](implicit tpe: In[A]): In[List[A]] = InArr[A, List[A]](tpe, _.toList.asRight)
    implicit def gqlInForVector[A](implicit tpe: In[A]): In[Vector[A]] = InArr[A, Vector[A]](tpe, _.toVector.asRight)
    implicit def gqlInForSet[A](implicit tpe: In[A]): In[Set[A]] = InArr[A, Set[A]](tpe, _.toSet.asRight)
    implicit def gqlInForNonEmptyList[A](implicit tpe: In[A]): In[NonEmptyList[A]] =
      InArr[A, NonEmptyList[A]](tpe, _.toList.toNel.toRight("empty array"))
    implicit def gqlInForNonEmptyVector[A](implicit tpe: In[A]): In[NonEmptyVector[A]] =
      InArr[A, NonEmptyVector[A]](tpe, _.toVector.toNev.toRight("empty array"))
    implicit def gqlInForChain[A](implicit tpe: In[A]): In[Chain[A]] =
      InArr[A, Chain[A]](tpe, xs => Chain.fromSeq(xs).asRight)
    implicit def gqlInForNonEmptyChain[A](implicit tpe: In[A]): In[NonEmptyChain[A]] =
      InArr[A, NonEmptyChain[A]](tpe, xs => NonEmptyChain.fromSeq(xs).toRight("empty array"))

    implicit def gqlOutArrForSeqLike[F[_], A, G[x] <: Seq[x]](implicit tpe: Out[F, A]): OutArr[F, A, G[A], A] =
      OutArr(tpe, _.toList, Resolver.pure[F, A, A](identity))
    implicit def gqlOutArrForNel[F[_], A](implicit tpe: Out[F, A]): OutArr[F, A, NonEmptyList[A], A] =
      OutArr(tpe, _.toList, Resolver.pure[F, A, A](identity))
    implicit def gqlOutArrForNev[F[_], A](implicit tpe: Out[F, A]): OutArr[F, A, NonEmptyVector[A], A] =
      OutArr(tpe, _.toList, Resolver.pure[F, A, A](identity))
    implicit def gqlOutArrForNec[F[_], A](implicit tpe: Out[F, A]): OutArr[F, A, NonEmptyChain[A], A] =
      OutArr(tpe, _.toList, Resolver.pure[F, A, A](identity))
    implicit def gqlOutArrForChain[F[_], A](implicit tpe: Out[F, A]): OutArr[F, A, Chain[A], A] =
      OutArr(tpe, _.toList, Resolver.pure[F, A, A](identity))
  }
}
