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
import cats.implicits._
import cats.data._
import scala.annotation.nowarn

/**
  * FreeApply is a Free Applicative, but witout pure.
  * It has an Apply instance.
  */
sealed abstract class FreeApply[F[_], +A] extends Product with Serializable {
  def foldMap[G[_], B >: A](fk: F ~> G)(implicit G: Apply[G]): G[B] =
    FreeApply.foldMap[F, G, B](this)(fk)

  // Very cool, we can do non-empty algebras with semigroup with the cool Const Semigroup => Apply[Cost] typeclass impl
  def analyze[M: Semigroup](fk: F ~> λ[α => M]): M =
    foldMap[Const[M, *], A](new (F ~> Const[M, *]) {
      def apply[A](fa: F[A]): Const[M, A] = Const(fk(fa))
    }).getConst

  @nowarn
  def analyze_[M: Semigroup](fold: F[?] => M): M =
    foldMap[Const[M, *], A](new (F ~> Const[M, *]) {
      def apply[A](fa: F[A]): Const[M, A] = Const(fold(fa))
    }).getConst

  @nowarn
  def enumerate: NonEmptyChain[F[?]] =
    analyze_(fa => NonEmptyChain.one(fa))
}

object FreeApply {
  def foldMap[F[_], G[_], A](fa: FreeApply[F, A])(fk: F ~> G)(implicit G: Apply[G]): G[A] = {
    @nowarn
    def go[B](fa: FreeApply[F, B]): Eval[G[B]] = Eval.defer {
      fa match {
        case Lift(fa)            => Eval.now(G.map(fk(fa))(x => x))
        case fmap: Fmap[F, a, B] => go(fmap.fa).map(G.map(_)(fmap.f))
        case ap: Ap[F, a, B]     => (go(ap.ff), go(ap.fa)).mapN((gf, ga) => G.ap(gf)(ga))
      }
    }
    go(fa).value
  }

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
