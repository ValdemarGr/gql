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
package gql.std

import cats._

sealed abstract class FreeApply[F[_], +A] extends Product with Serializable {
  def foldMap[G[_], B >: A](fk: F ~> G)(implicit G: Apply[G]): G[B] = this match {
    case FreeApply.Lift(fa)    => G.map(fk(fa))(x => x)
    case FreeApply.Ap(ff, fa)  => G.map(G.ap(ff.foldMap(fk))(fa.foldMap(fk)))(x => x)
    case FreeApply.Fmap(fa, f) => G.map(fa.foldMap(fk))(f)
  }
}

object FreeApply {
  final case class Ap[F[_], A, B](ff: FreeApply[F, A => B], fa: FreeApply[F, A]) extends FreeApply[F, B]

  final case class Lift[F[_], A](fa: F[A]) extends FreeApply[F, A]

  // Allows O(1) fmap
  final case class Fmap[F[_], A, B](fa: FreeApply[F, A], f: A => B) extends FreeApply[F, B]

  def lift[F[_], A](fa: F[A]): FreeApply[F, A] = Lift(fa)

  implicit def freeApply[F[_]]: Apply[FreeApply[F, *]] = {
    type G[A] = FreeApply[F, A]
    new Apply[G] {
      override def map[A, B](fa: G[A])(f: A => B): G[B] = Fmap(fa, f)
      override def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] = Ap(ff, fa)
    }
  }
}
