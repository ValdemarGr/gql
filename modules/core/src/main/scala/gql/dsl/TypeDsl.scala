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
import cats.implicits._
import scala.reflect.ClassTag
import gql.dsl.aliases._

trait TypeDsl[F[_]] {
  def tpe[A](name: String, hd: (String, Field[F, A, ?]), tl: (String, Field[F, A, ?])*) =
    TypeDsl.tpe(name, hd, tl: _*)

  def tpeNel[A](name: String, entries: Fields[F, A]) =
    TypeDsl.tpeNel(name, entries)

  implicit def typeDslTypeOps[A](tpe: Type[F, A]): TypeDsl.TypeOps[F, A] =
    TypeDsl.typeDslFullTypeOps[F, A](tpe)
}

trait TypeDslFull {
  def tpe[F[_], A](name: String, hd: (String, Field[F, A, ?]), tl: (String, Field[F, A, ?])*) =
    Type[F, A](name, NonEmptyList(hd, tl.toList), Nil)

  def tpeNel[F[_], A](name: String, entries: Fields[F, A]) = Type[F, A](name, entries, Nil)

  implicit def typeDslFullTypeOps[F[_], A](tpe: Type[F, A]): TypeDsl.TypeOps[F, A] = new TypeDsl.TypeOps(tpe)
}

object TypeDsl extends TypeDslFull {
  def apply[F[_]]: TypeDsl[F] = new TypeDsl[F] {}

  final class TypeOps[F[_], A](private val tpe: Type[F, A]) extends AnyVal {
    def implements[B](pf: PartialFunction[B, A])(implicit interface: => Interface[F, B]): Type[F, A] =
      tpe.copy(implementations = Implementation(Eval.later(interface))(pf.lift.andThen(_.rightIor)) :: tpe.implementations)

    def subtypeOf[B](implicit ev: A <:< B, tag: ClassTag[A], interface: => Interface[F, B]): Type[F, A] =
      implements[B] { case a: A => a }(interface)

    def addFields(xs: (String, Field[F, A, ?])*): Type[F, A] =
      tpe.copy(fields = tpe.fields concat xs.toList)

    def addFieldsNel(xs: NonEmptyList[(String, Field[F, A, ?])]): Type[F, A] =
      addFields(xs.toList: _*)

    def subtypeImpl[B](implicit ev: A <:< B, tag: ClassTag[A], interface: => Interface[F, B]): Type[F, A] = {
      val existingConcretes = tpe.fields.map { case (k, _) => k }.toList.toSet
      val concretes = interface.fields.collect {
        case (k, f: gql.ast.Field[F, B, x]) if !existingConcretes.contains(k) =>
          (k, f.contramap[F, A](ev(_)))
      }
      subtypeOf[B](ev, tag, interface).addFields(concretes: _*)
    }
  }
}
