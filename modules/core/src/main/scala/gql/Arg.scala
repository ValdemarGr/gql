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

import cats.data._
import cats._
import cats.implicits._
import gql.ast._

trait Arg[A] {
  def entries: Chain[ArgValue[?]]

  def decode: Map[String, ?] => Either[String, A]

  def emap[B](f: A => Either[String, B]): Arg[B]
}

object Arg {
  def make[A](name: String, default: Option[Value], description: Option[String])(implicit input: => In[A]): NonEmptyArg[A] =
    NonEmptyArg.one[A](ArgValue(name, Eval.later(input), default, description))

  implicit lazy val applicativeInstanceForArg: Applicative[Arg] = new Applicative[Arg] {
    override def pure[A](x: A): Arg[A] = PureArg(x.asRight)

    override def ap[A, B](ff: Arg[A => B])(fa: Arg[A]): Arg[B] =
      (ff, fa) match {
        case (NonEmptyArg(entries1, decode1), NonEmptyArg(entries2, decode2)) =>
          NonEmptyArg(entries1 ++ entries2, m => (decode1(m), decode2(m)).mapN { case (f, a) => f(a) })
        case (NonEmptyArg(entries, decode), PureArg(value)) =>
          NonEmptyArg(entries, m => (decode(m), value).mapN { case (f, a) => f(a) })
        case (PureArg(value), NonEmptyArg(entries, decode)) =>
          NonEmptyArg(entries, m => (value, decode(m)).mapN { case (f, a) => f(a) })
        case (PureArg(f), PureArg(value)) => PureArg((f, value).mapN { case (f, a) => f(a) })
      }
  }
}

final case class ArgValue[A](
    name: String,
    input: Eval[In[A]],
    defaultValue: Option[Value],
    description: Option[String]
) {
  def document(description: String) = copy(description = Some(description))

  def default(value: Value) = copy(defaultValue = Some(value))
}

final case class NonEmptyArg[A](
    nec: NonEmptyChain[ArgValue[?]],
    decode: Map[String, ?] => Either[String, A]
) extends Arg[A] {

  def entries = nec.toChain

  override def emap[B](f: A => Either[String, B]): NonEmptyArg[B] =
    NonEmptyArg(nec, decode.andThen(_.flatMap(f)))
}
object NonEmptyArg {
  def one[A](av: ArgValue[A]): NonEmptyArg[A] =
    NonEmptyArg[A](NonEmptyChain.one(av), _(av.name).asInstanceOf[A].asRight)

  implicit lazy val applicativeInstanceForNonEmptyArg: Apply[NonEmptyArg] = new Apply[NonEmptyArg] {
    override def map[A, B](fa: NonEmptyArg[A])(f: A => B): NonEmptyArg[B] =
      NonEmptyArg(fa.nec, fa.decode.andThen(_.map(f)))

    override def ap[A, B](ff: NonEmptyArg[A => B])(fa: NonEmptyArg[A]): NonEmptyArg[B] =
      NonEmptyArg(ff.nec ++ fa.nec, m => (ff.decode(m), fa.decode(m)).mapN { case (f, a) => f(a) })
  }
}

final case class PureArg[A](value: Either[String, A]) extends Arg[A] {
  def entries = Chain.empty
  def decode = _ => value
  override def emap[B](f: A => Either[String, B]): Arg[B] = PureArg(value.flatMap(f))
}
