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

import gql.ast._
import cats.data._
import cats._

trait InterfaceDsl[F[_]] {
  def interfaceNel[A](
      name: String,
      fields: AbstractFields[F]
  ): Interface[F, A] = InterfaceDsl.interfaceNel(name, fields)

  def interface[F[_], A](
      name: String,
      hd: (String, AbstractField[F, ?]),
      tl: (String, AbstractField[F, ?])*
  ): Interface[F, A] = InterfaceDsl.interface(name, hd, tl: _*)

  def interfaceFromNel[F[_], A](
      name: String,
      fields: Fields[F, A]
  ): Interface[F, A] = InterfaceDsl.interfaceFromNel(name, fields)

  def interfaceFrom[F[_], A](
      name: String,
      hd: (String, Field[F, A, ?]),
      tl: (String, Field[F, A, ?])*
  ): Interface[F, A] = InterfaceDsl.interfaceFrom(name, hd, tl: _*)

  implicit def interfaceDslInterfaceOps[F[_], A](tpe: Interface[F, A]): InterfaceDsl.InterfaceOps[F, A] =
    InterfaceDsl.interfaceDslFullInterfaceOps[F, A](tpe)
}

trait InterfaceDslFull {
  def interfaceNel[F[_], A](
      name: String,
      fields: AbstractFields[F]
  ): Interface[F, A] = Interface[F, A](name, fields, Nil)

  def interface[F[_], A](
      name: String,
      hd: (String, AbstractField[F, ?]),
      tl: (String, AbstractField[F, ?])*
  ): Interface[F, A] = interfaceNel[F, A](name, NonEmptyList(hd, tl.toList))

  def interfaceFromNel[F[_], A](
      name: String,
      fields: Fields[F, A]
  ): Interface[F, A] = interfaceNel[F, A](name, fields.map { case (k, v) => k -> v.asAbstract })

  def interfaceFrom[F[_], A](
      name: String,
      hd: (String, Field[F, A, ?]),
      tl: (String, Field[F, A, ?])*
  ): Interface[F, A] = interfaceFromNel[F, A](name, NonEmptyList(hd, tl.toList))

  implicit def interfaceDslFullInterfaceOps[F[_], A](tpe: Interface[F, A]): InterfaceDsl.InterfaceOps[F, A] =
    new InterfaceDsl.InterfaceOps[F, A](tpe)
}

object InterfaceDsl extends InterfaceDslFull {
  def apply[F[_]]: InterfaceDsl[F] = new InterfaceDsl[F] {}

  final class InterfaceOps[F[_], A](private val tpe: Interface[F, A]) extends AnyVal {
    def implements[B](implicit interface: => Interface[F, B]): Interface[F, A] =
      tpe.copy(implementations = Eval.later(interface) :: tpe.implementations)

    def addAbstractFields(xs: (String, AbstractField[F, ?])*): Interface[F, A] =
      tpe.copy(fields = tpe.fields concat xs.toList)

    def addAbstractFieldsNel(xs: NonEmptyList[(String, AbstractField[F, ?])]): Interface[F, A] =
      addAbstractFields(xs.toList: _*)

    def addFields(xs: (String, Field[F, A, ?])*): Interface[F, A] =
      addAbstractFields(xs.map { case (k, v) => k -> v.asAbstract }: _*)

    def addFieldsNel(xs: NonEmptyList[(String, Field[F, A, ?])]): Interface[F, A] =
      addFields(xs.toList: _*)
  }
}
