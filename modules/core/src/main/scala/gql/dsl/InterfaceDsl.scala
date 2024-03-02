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
import gql.dsl.aliases._

trait InterfaceDsl[F[_]] {
  def interfaceNel[A](
      name: String,
      fields: AnyFields[F, A]
  ): Interface[F, A] = InterfaceDsl.interfaceNel(name, fields)

  def interface[A](
      name: String,
      hd: (String, AnyField[F, A, ?]),
      tl: (String, AnyField[F, A, ?])*
  ): Interface[F, A] = InterfaceDsl.interface(name, hd, tl: _*)

  implicit def interfaceDslInterfaceOps[A](tpe: Interface[F, A]): InterfaceDsl.InterfaceOps[F, A] =
    InterfaceDsl.interfaceDslFullInterfaceOps[F, A](tpe)
}

trait InterfaceDslFull {
  def interfaceNel[F[_], A](
      name: String,
      fields: AnyFields[F, A]
  ): Interface[F, A] = Interface[F, A](name, fields, Nil)

  def interface[F[_], A](
      name: String,
      hd: (String, AnyField[F, A, ?]),
      tl: (String, AnyField[F, A, ?])*
  ): Interface[F, A] = interfaceNel[F, A](name, NonEmptyList(hd, tl.toList))

  implicit def interfaceDslFullInterfaceOps[F[_], A](tpe: Interface[F, A]): InterfaceDsl.InterfaceOps[F, A] =
    new InterfaceDsl.InterfaceOps[F, A](tpe)
}

object InterfaceDsl extends InterfaceDslFull {
  def apply[F[_]]: InterfaceDsl[F] = new InterfaceDsl[F] {}

  final class InterfaceOps[F[_], A](private val tpe: Interface[F, A]) extends AnyVal {
    def implements[B](implicit interface: => Interface[F, B]): Interface[F, A] =
      tpe.copy(implementations = Eval.always(interface) :: tpe.implementations)

    def subtypeImpl[B](implicit ev: A <:< B, interface: => Interface[F, B]): Interface[F, A] = {
      val existingConcretes = tpe.fields.collect { case (k, _: gql.ast.Field[F, A, ?]) => k }.toSet
      val existingAbstracts = tpe.fields.collect { case (k, _: gql.ast.AbstractField[F, ?]) => k }.toSet
      val news = interface.fields.collect {
        case (k, f: gql.ast.Field[F, B, x]) if !existingConcretes.contains(k) =>
          (k, f.contramap[F, A](ev(_)))
        case (k, f: gql.ast.AbstractField[F, x]) if !existingAbstracts.contains(k) && !existingConcretes.contains(k) =>
          (k, f)
      }
      implements[B](interface).addFields(news.toList: _*)
    }

    def addFields(xs: (String, AnyField[F, A, ?])*): Interface[F, A] =
      tpe.copy(fields = tpe.fields ++ xs.toList)

    def addFieldsNel(xs: NonEmptyList[(String, AnyField[F, A, ?])]): Interface[F, A] =
      addFields(xs.toList: _*)
  }
}
