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
package gql.std

import cats._
import cats.implicits._

// An applicative structure for effectful fixed points
case class LazyT[F[_], A, B](fb: F[Eval[A] => B]) {
  def mapF[G[_], C](f: F[Eval[A] => B] => G[Eval[A] => C]) =
    LazyT(f(fb))

  def runWithValue(f: B => A)(implicit F: Functor[F]): F[B] =
    runWithBoth(f).map { case (_, b) => b }

  def runWithBoth(f: B => A)(implicit F: Functor[F]): F[(A, B)] =
    fb
      .map { g =>
        lazy val t: (A, B) = {
          lazy val b = g(Eval.later(t._1))
          (f(b), b)
        }
        t
      }
}

object LazyT {
  def id[F[_], A](implicit F: Applicative[F]): LazyT[F, A, Eval[A]] =
    LazyT(F.pure(x => x))

  def liftF[F[_], A, B](fb: F[B])(implicit F: Functor[F]): LazyT[F, A, B] =
    LazyT(fb.map(a => (_: Eval[A]) => a))

  def lift[F[_], A, B](f: Eval[A] => B)(implicit F: Applicative[F]): LazyT[F, A, B] =
    LazyT(F.pure(f))

  def applicativeForApplicativeLazyT[F[_]: Applicative, A]: Applicative[LazyT[F, A, *]] =
    new Applicative[LazyT[F, A, *]] {
      override def ap[C, B](ff: LazyT[F, A, C => B])(fa: LazyT[F, A, C]): LazyT[F, A, B] =
        LazyT((ff.fb, fa.fb).mapN { (gf, ga) => (ea: Eval[A]) => gf(ea).apply(ga(ea)) })

      override def pure[C](x: C): LazyT[F, A, C] = ???
    }

  def applicativeForParallelLazyT[F[_], A](implicit P: Parallel[F]): Applicative[LazyT[F, A, *]] =
    new Applicative[LazyT[F, A, *]] {
      override def ap[C, B](ff: LazyT[F, A, C => B])(fa: LazyT[F, A, C]): LazyT[F, A, B] =
        LazyT {
          P.sequential {
            P.applicative.map2(P.parallel(ff.fb), P.parallel(fa.fb)) { (gf, ga) => (ea: Eval[A]) => gf(ea).apply(ga(ea)) }
          }
        }

      override def pure[C](x: C): LazyT[F, A, C] =
        LazyT(P.monad.pure((_: Eval[A]) => x))
    }
}
