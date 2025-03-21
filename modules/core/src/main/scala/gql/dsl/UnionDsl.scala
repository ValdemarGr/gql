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

import cats.implicits._
import gql.ast._
import cats.data._
import cats._
import scala.reflect.ClassTag

trait UnionDsl[F[_]] {
  def union[A](name: String): UnionDsl.PartiallyAppliedUnion[F, A] =
    UnionDsl.union[F, A](name)

  implicit def unionDslUnionOps[A](u: Union[F, A]): UnionDsl.UnionOps[F, A] =
    UnionDsl.unionDslFullUnionOps[F, A](u)
}

trait UnionDslFull {
  def union[F[_], A](name: String): UnionDsl.PartiallyAppliedUnion[F, A] =
    new UnionDsl.PartiallyAppliedUnion[F, A](name)

  implicit def unionDslFullUnionOps[F[_], A](u: Union[F, A]): UnionDsl.UnionOps[F, A] =
    new UnionDsl.UnionOps[F, A](u)
}

object UnionDsl extends UnionDslFull {
  final class PartiallyAppliedUnion[F[_], A](private val name: String) {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      Union[F, A](name, NonEmptyList.of(Variant[F, A, B](Eval.always(innerTpe))(pf.lift.andThen(_.rightIor))), None)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }

  final case class UnionOps[F[_], A](private val u: Union[F, A]) extends AnyVal {
    def variant[B](pf: PartialFunction[A, B])(implicit innerTpe: => Type[F, B]): Union[F, A] =
      u.copy(types = Variant[F, A, B](Eval.always(innerTpe))(pf.lift.andThen(_.rightIor)) :: u.types)

    def subtype[B: ClassTag](implicit ev: B <:< A, innerTpe: => Type[F, B]): Union[F, A] =
      variant[B] { case a: B => a }(innerTpe)
  }
}
