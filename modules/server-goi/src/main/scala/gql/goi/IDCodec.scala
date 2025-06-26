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
package gql.goi

import org.typelevel.twiddles.TwiddleSyntax
import cats.implicits._
import cats._
import cats.data._

trait IDCodec[A] { self =>
  def codecs: NonEmptyChain[String]

  def encode(a: A): NonEmptyChain[String]

  def decode(s: Array[String]): ValidatedNec[String, A]

  def opt: IDCodec[Option[A]] =
    IDCodec(
      self.codecs,
      s =>
        if (s.forall(_ == "null")) None.validNec
        else self.decode(s).map(Some(_)),
      {
        case Some(a) => self.encode(a)
        case None    => NonEmptyChain.one("null")
      }
    )

  def eimap[B](f: A => Either[String, B])(g: B => A): IDCodec[B] =
    IDCodec(
      self.codecs,
      s => self.decode(s).andThen(x => f(x).toValidatedNec),
      b => self.encode(g(b))
    )

  def product[B](that: IDCodec[B]): IDCodec[(A, B)] =
    IDCodec(
      self.codecs ++ that.codecs,
      s => {
        val (s1, s2) = s.splitAt(self.codecs.length.toInt)
        (self.decode(s1), that.decode(s2)).tupled
      },
      { a =>
        val (a1, b1) = a
        self.encode(a1) ++ that.encode(b1)
      }
    )
}

object IDCodec extends TwiddleSyntax[IDCodec] {
  def apply[A](
      codecs: NonEmptyChain[String],
      decode: Array[String] => ValidatedNec[String, A],
      encode: A => NonEmptyChain[String]
  ): IDCodec[A] = {
    val decode0 = decode(_)
    val encode0 = encode(_)
    val codecs0 = codecs
    new IDCodec[A] {
      override def codecs: NonEmptyChain[String] = codecs0
      override def encode(a: A): NonEmptyChain[String] = encode0(a)
      override def decode(s: Array[String]): ValidatedNec[String, A] = decode0(s)
    }
  }

  def make[A](decode: String => Either[String, A], encode: A => String, name: String): IDCodec[A] =
    IDCodec(
      NonEmptyChain.one(name),
      {
        case Array(s1) => decode(s1).toValidatedNec
        case s => s"Invalid input for codec $name, expected exactly one input but got ${s.size}: ${s.toSeq.mkString_(":")}".invalidNec
      },
      a => NonEmptyChain.one(encode(a))
    )

  implicit lazy val invariantSemigroupalForIdCodec: InvariantSemigroupal[IDCodec] = new InvariantSemigroupal[IDCodec] {
    override def imap[A, B](fa: IDCodec[A])(f: A => B)(g: B => A): IDCodec[B] =
      new IDCodec[B] {
        override def codecs: NonEmptyChain[String] = fa.codecs

        override def encode(a: B): NonEmptyChain[String] = fa.encode(g(a))

        override def decode(s: Array[String]): ValidatedNec[String, B] = fa.decode(s).map(f)
      }

    override def product[A, B](fa: IDCodec[A], fb: IDCodec[B]): IDCodec[(A, B)] = fa product fb
  }

  implicit val stringInstance: IDCodec[String] = IDCodec.make[String](Right(_), identity, "string")

  implicit val intInstance: IDCodec[Int] =
    IDCodec.make[Int](s => Either.catchNonFatal(s.toInt).leftMap(_ => s"Cannot parse '$s' as Int"), _.toString, "int")

  implicit val longInstance: IDCodec[Long] =
    IDCodec.make[Long](s => Either.catchNonFatal(s.toLong).leftMap(_ => s"Cannot parse '$s' as Long"), _.toString, "long")

  implicit val floatInstance: IDCodec[Float] =
    IDCodec
      .make[Float](s => Either.catchNonFatal(s.toFloat).leftMap(_ => s"Cannot parse '$s' as Float"), _.toString, "float")

  implicit val doubleInstance: IDCodec[Double] =
    IDCodec
      .make[Double](s => Either.catchNonFatal(s.toDouble).leftMap(_ => s"Cannot parse '$s' as Double"), _.toString, "double")

  implicit val booleanInstance: IDCodec[Boolean] =
    IDCodec.make[Boolean](s => Either.catchNonFatal(s.toBoolean).leftMap(_ => s"Cannot parse '$s' as Boolean"), _.toString, "boolean")

  implicit val uuidInstance: IDCodec[java.util.UUID] = IDCodec.make[java.util.UUID](
    s => Either.catchNonFatal(java.util.UUID.fromString(s)).leftMap(_ => s"Cannot parse '$s' as UUID"),
    _.toString,
    "uuid"
  )

  implicit val dateInstance: IDCodec[java.time.LocalDate] =
    IDCodec.make[java.time.LocalDate](
      s => Either.catchNonFatal(java.time.LocalDate.parse(s)).leftMap(_ => s"Cannot parse '$s' as LocalDate"),
      _.toString,
      "date"
    )
}
