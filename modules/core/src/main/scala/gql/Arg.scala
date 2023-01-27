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

final case class Arg[+A](
    entries: Chain[ArgValue[?]],
    decode: Map[String, ArgParam[?]] => Either[String, A]
) {
  def emap[B](f: A => Either[String, B]): Arg[B] =
    Arg(entries, decode.andThen(_.flatMap(f)))
}

object Arg {
  def makeFrom[A, B](av: ArgValue[A])(f: ArgParam[A] => Either[String, B]): Arg[B] =
    Arg[B](Chain(av), m => f(m(av.name).asInstanceOf[ArgParam[A]]))

  def make[A](av: ArgValue[A]): Arg[A] =
    makeFrom(av)(_.value.asRight)

  implicit lazy val applicativeInstanceForArg: Applicative[Arg] = new Applicative[Arg] {
    override def pure[A](x: A): Arg[A] = Arg(Chain.empty, _ => x.asRight)

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
    defaultValue: Option[Value],
    description: Option[String]
) {
  def document(description: String) = copy(description = Some(description))

  def default(value: Value) = copy(defaultValue = Some(value))
}

object ArgValue {
  def make[A](name: String, default: Option[Value] = None, description: Option[String] = None)(implicit in: => In[A]): ArgValue[A] =
    ArgValue(name, Eval.later(in), default, description)
}
