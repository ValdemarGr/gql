/*
 * Copyright 2024 Valdemar Grange
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

import cats._
import cats.data._
import cats.implicits._
import gql.ast._
import gql.parser.Const
import gql.parser.{Value => V}
import gql.std.FreeApply
import gql.resolver.Resolver

/** A GraphQL argument declaration with an optional default value. The argument references an input type, which is suspended in Eval for
  * recursion.
  */
final case class ArgValue[A](
    name: String,
    input: Eval[In[A]],
    defaultValue: Option[V[Const, Unit]],
    description: Option[String]
) {
  def document(description: String) = copy(description = Some(description))

  def default(value: V[Const, Unit]) = copy(defaultValue = Some(value))
}

/** A GraphQL argument value suppled in respect to a declared [[ArgValue]], usually provided by a calling client.
  */
final case class ArgParam[A](
    defaulted: Boolean,
    value: A
)

/** A combination of an argument declaration [[ArgParam]] and a decoder function for [[ArgParam]]. This structure is more specialized than
  * just decoding with [[Arg]], since you will have access to field specific information such as if the argument was provided explicitly or
  * the default was used.
  *
  * The trivial implementation of an [[ArgDecoder]] given an [[ArgValue]]:
  * {{{
  *   val av: ArgValue[A] = ???
  *   ArgDecoder[A, A](av, _.value.asRight)
  * }}}
  */
final case class ArgDecoder[A, B](
    av: ArgValue[A],
    decode: ArgParam[A] => Either[String, B]
)

/** A small free algebra for combining arguments and decoding them. [[ArgDecoder]] can trivially be lifted into [[Arg]] with loss of
  * field-level information.
  *
  * Note that [[Arg]] must be non-empty, and therefore it does not form an [[cats.Applicative]], but instead it forms [[cats.Apply]].
  */
final case class Arg[+A](impl: FreeApply[Arg.Impl, ValidatedNec[String, A]]) {
  def emap[B](f: A => Either[String, B]): Arg[B] =
    Arg(impl.map(_.andThen(f(_).toValidatedNec)))

  def entries: NonEmptyChain[ArgValue[?]] = impl.enumerate.map(_.av)
}

object Arg {
  // We don't care where we came from, only that we can decode it.
  type Impl[A] = ArgDecoder[?, A]

  def makeFrom[A, B](av: ArgValue[A])(f: ArgParam[A] => Either[String, B]): Arg[B] = {
    val fa: Impl[B] = ArgDecoder[A, B](av, f)
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

sealed trait EmptyableArg[A] {
  def addArg[F[_], B]: Resolver[F, B, (A, B)]
}
object EmptyableArg {
  case object Empty extends EmptyableArg[Unit] {
    override def addArg[F[_], B]: Resolver[F, B, (Unit, B)] = Resolver.id[F, B].map(((), _))
  }
  final case class Lift[A](a: Arg[A]) extends EmptyableArg[A] {
    override def addArg[F[_], B]: Resolver[F, B, (A, B)] = Resolver.id[F, B].arg(a)
  }
}
