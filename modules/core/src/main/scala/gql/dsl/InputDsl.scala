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
package gql.dsl

import gql.resolver._
import gql.ast._
import gql.parser.{Value => V, Const}
import gql._
import cats._

trait InputDslFull {
  def arg[A](name: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), None, None))

  def arg[A](name: String, description: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), None, Some(description)))

  def arg[A](name: String, default: V[Const, Unit])(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), Some(default), None))

  def arg[A](name: String, default: V[Const, Unit], description: String)(implicit tpe: => In[A]): Arg[A] =
    Arg.make[A](ArgValue(name, Eval.later(tpe), Some(default), Some(description)))

  def argFull[A]: InputDsl.PartiallyAppliedArgFull[A] = new InputDsl.PartiallyAppliedArgFull[A]

  def arged[F[_], I, A](a: Arg[A]) = Resolver.argument[F, I, A](a)

  def value = InputDsl.ValueDsl

  def input[A](
      name: String,
      fields: Arg[A]
  ): Input[A] = Input(name, fields)
}

object InputDsl extends InputDslFull {
  object ValueDsl {
    def scalar[A](value: A)(implicit tpe: => Scalar[A]) =
      tpe.encoder(value)

    def fromEnum[A](value: A)(implicit tpe: => Enum[A]) =
      tpe.revm.get(value).map(enumValue)

    def enumValue(value: String) = V.EnumValue(value)

    def arr(xs: V[Const, Unit]*) = V.ListValue(xs.toList)

    def obj(xs: (String, V[Const, Unit])*) = V.ObjectValue(xs.toList)

    def nullValue = V.NullValue()
  }

  final class PartiallyAppliedArgFull[A](private val dummy: Boolean = false) extends AnyVal {
    def apply[B](name: String, default: Option[V[Const, Unit]], description: Option[String])(
        f: ArgParam[A] => Either[String, B]
    )(implicit tpe: => In[A]): Arg[B] =
      Arg.makeFrom[A, B](ArgValue(name, Eval.later(tpe), default, description))(f)
  }
}
