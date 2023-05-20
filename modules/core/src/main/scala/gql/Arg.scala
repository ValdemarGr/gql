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
import gql.ast._
import gql.parser.{Value => V, Const, AnyValue}
import gql.std.FreeApply

final case class ArgValue[A](
    name: String,
    input: Eval[In[A]],
    defaultValue: Option[V[Const]],
    description: Option[String]
) {
  def document(description: String) = copy(description = Some(description))

  def default(value: V[Const]) = copy(defaultValue = Some(value))
}

final case class ArgParam[A](
    defaulted: Boolean,
    value: A
)

final case class DecodedArgValue[A, B](
    av: ArgValue[A],
    decode: ArgParam[A] => Either[String, B]
)

final case class Arg[+A](impl: FreeApply[Arg.Impl, ValidatedNec[String, A]]) {
  def emap[B](f: A => Either[String, B]): Arg[B] =
    Arg(impl.map(_.andThen(f(_).toValidatedNec)))

  def entries: NonEmptyChain[ArgValue[?]] = impl.enumerate.map(_.av)
}

object Arg {
  type Impl[A] = DecodedArgValue[?, A]
  def makeFrom[A, B](av: ArgValue[A])(f: ArgParam[A] => Either[String, B]): Arg[B] = {
    val fa: Impl[B] = DecodedArgValue[A, B](av, f)
    val lifted: FreeApply[Impl, B] = FreeApply.lift(fa)
    val mapped: FreeApply[Impl, ValidatedNec[String, B]] = lifted.map(_.validNec[String])
    Arg(mapped)
  }

  def make[A](av: ArgValue[A]): Arg[A] =
    makeFrom(av)(_.value.asRight)

  implicit val applyForArg: Apply[Arg] = new Apply[Arg] {
    override def map[A, B](fa: Arg[A])(f: A => B): Arg[B] =
      Arg(fa.impl.map(_.map(f)))

    override def ap[A, B](ff: Arg[A => B])(fa: Arg[A]): Arg[B] =
      Arg((fa.impl, ff.impl).mapN(_ ap _))
  }
}
