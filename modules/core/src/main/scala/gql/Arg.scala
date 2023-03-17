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

final case class ArgValue2[A](
    name: String,
    input: Eval[In[A]],
    defaultValue: Option[V[Const]],
    description: Option[String]
) {
  def document(description: String) = copy(description = Some(description))

  def default(value: V[Const]) = copy(defaultValue = Some(value))
}

final case class DecodedArgValue[A, B](
    av: ArgValue2[A],
    decode: ArgParam[A] => Either[String, B]
)

final case class Arg2[+A](impl: FreeApply[Arg2.Impl, ValidatedNec[String, A]]) {
  def emap[B](f: A => Either[String, B]): Arg2[B] =
    Arg2(impl.map(_.andThen(f(_).toValidatedNec)))
}

object Arg2 {
  type Impl[A] = DecodedArgValue[?, A]
  def makeFrom[A, B](av: ArgValue2[A])(f: ArgParam[A] => Either[String, B]): Arg2[B] = {
    val fa: Impl[B] = DecodedArgValue[A, B](av, f)
    val lifted: FreeApply[Impl, B] = FreeApply.lift(fa)
    val mapped: FreeApply[Impl, ValidatedNec[String, B]] = lifted.map(_.validNec[String])
    Arg2(mapped)
  }

  def make[A](av: ArgValue2[A]): Arg2[A] =
    makeFrom(av)(_.value.asRight)

  implicit val applyForArg: Apply[Arg2] = new Apply[Arg2] {
    override def map[A, B](fa: Arg2[A])(f: A => B): Arg2[B] =
      Arg2(fa.impl.map(_.map(f)))

    override def ap[A, B](ff: Arg2[A => B])(fa: Arg2[A]): Arg2[B] =
      Arg2((fa.impl, ff.impl).mapN(_ ap _))
  }

  type ContIn[A] = (In[A], V[AnyValue])
  def argCompiler[B](
      avail: Map[String, V[AnyValue]],
      inCompiler: ContIn ~> ValidatedNec[String, *]
  ): DecodedArgValue[B, *] ~> ValidatedNec[String, *] =
    new (DecodedArgValue[B, *] ~> ValidatedNec[String, *]) {
      override def apply[A](fa: DecodedArgValue[B, A]): ValidatedNec[String, A] = {
        def compileWith(x: V[AnyValue], default: Boolean) =
          inCompiler((fa.av.input.value, x)).andThen(a => fa.decode(ArgParam(default, a)).toValidatedNec)

        avail
          .get(fa.av.name)
          .map(compileWith(_, false))
          .orElse(fa.av.defaultValue.map(compileWith(_, true)))
          .getOrElse {
            fa.av.input.value match {
              case _: gql.ast.InOpt[a] => fa.decode(ArgParam(true, None)).toValidatedNec
              case _ =>
                s"Missing argument for '${fa.av.name}' and no default value was found.".invalidNec
            }
          }
      }
    }
}

final case class Arg[+A](
    entries: NonEmptyChain[ArgValue[?]],
    decode: Map[String, ArgParam[?]] => Either[String, A]
) {
  def emap[B](f: A => Either[String, B]): Arg[B] =
    Arg(entries, decode.andThen(_.flatMap(f)))
}

object Arg {
  def makeFrom[A, B](av: ArgValue[A])(f: ArgParam[A] => Either[String, B]): Arg[B] =
    Arg[B](NonEmptyChain.one(av), m => f(m(av.name).asInstanceOf[ArgParam[A]]))

  def make[A](av: ArgValue[A]): Arg[A] =
    makeFrom(av)(_.value.asRight)

  implicit lazy val applyForArg: Apply[Arg] = new Apply[Arg] {
    override def map[A, B](fa: Arg[A])(f: A => B): Arg[B] =
      Arg(fa.entries, fa.decode.andThen(_.map(f)))

    override def ap[A, B](ff: Arg[A => B])(fa: Arg[A]): Arg[B] =
      Arg(ff.entries ++ fa.entries, m => (ff.decode(m), fa.decode(m)).mapN((f, a) => f(a)))
  }
}

final case class ArgParam[A](
    defaulted: Boolean,
    value: A
)

final case class ArgValue[A](
    name: String,
    input: Eval[In[A]],
    defaultValue: Option[V[Const]],
    description: Option[String]
) {
  def document(description: String) = copy(description = Some(description))

  def default(value: V[Const]) = copy(defaultValue = Some(value))
}
