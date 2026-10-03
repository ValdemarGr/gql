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

// An applicative structure for effectful fixed points
case class LazyT[F[_], A, B](fb: F[Either[B, Eval[A] => B]]) {
  def mapF[G[_], C](f: F[Either[B, Eval[A] => B]] => G[Either[C, Eval[A] => C]]) =
    LazyT(f(fb))

  def runWithValue(f: B => A)(implicit F: Functor[F]): F[B] =
    fb.map {
      case Left(b) => b
      case Right(g) =>
        lazy val b: B = g(Eval.later(f(b)))
        b
    }

  def runWithBoth(f: B => A)(implicit F: Functor[F]): F[(A, B)] =
    fb
      .map {
        case Left(b) => (f(b), b)
        case Right(g) =>
          lazy val t: (A, B) = {
            lazy val b = g(Eval.later(t._1))
            (f(b), b)
          }
          t
      }
}

object LazyT {
  def id[F[_], A](implicit F: Applicative[F]): LazyT[F, A, Eval[A]] =
    lift[F, A, Eval[A]](identity)

  def liftF[F[_], A, B](fb: F[B])(implicit F: Functor[F]): LazyT[F, A, B] =
    LazyT(fb.map(_.asLeft[Eval[A] => B]))

  def lift[F[_], A, B](f: Eval[A] => B)(implicit F: Applicative[F]): LazyT[F, A, B] =
    LazyT(F.pure(f.asRight[B]))

  def applicativeForApplicativeLazyT[F[_], A](implicit F: Applicative[F]): Applicative[LazyT[F, A, *]] =
    new Applicative[LazyT[F, A, *]] {
      override def ap[C, B](ff: LazyT[F, A, C => B])(fa: LazyT[F, A, C]): LazyT[F, A, B] =
        LazyT((ff.fb, fa.fb).mapN {
          case (Left(f), Left(a))   => f(a).asLeft[Eval[A] => B]
          case (Left(f), Right(a))  => ((ea: Eval[A]) => f(a(ea))).asRight[B]
          case (Right(f), Left(a))  => ((ea: Eval[A]) => f(ea)(a)).asRight[B]
          case (Right(f), Right(a)) => ((ea: Eval[A]) => f(ea)(a(ea))).asRight[B]
        })

      override def pure[C](x: C): LazyT[F, A, C] =
        LazyT(F.pure(x.asLeft[Eval[A] => C]))
    }

  def applicativeForParallelLazyT[F[_], A](implicit P: Parallel[F]): Applicative[LazyT[F, A, *]] =
    new Applicative[LazyT[F, A, *]] {
      val L = applicativeForApplicativeLazyT[P.F, A](P.applicative)

      override def ap[C, B](ff: LazyT[F, A, C => B])(fa: LazyT[F, A, C]): LazyT[F, A, B] =
        L.ap(ff.mapF[P.F, C => B](P.parallel(_)))(fa.mapF[P.F, C](P.parallel(_))).mapF[F, B](P.sequential(_))

      override def pure[C](x: C): LazyT[F, A, C] =
        LazyT(P.monad.pure(x.asLeft[Eval[A] => C]))
    }
}
