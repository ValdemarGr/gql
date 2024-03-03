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
package gql.server.interpreter

import cats._
import cats.implicits._

final case class IndexedData[F[_], +A](
    index: Int,
    node: EvalNode[F, A]
)

object IndexedData {
  implicit def traverseForIndexedData[F[_]]: Traverse[IndexedData[F, *]] = new Traverse[IndexedData[F, *]] {
    override def foldLeft[A, B](fa: IndexedData[F, A], b: B)(f: (B, A) => B): B =
      f(b, fa.node.value)
    override def foldRight[A, B](fa: IndexedData[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.node.value, lb)
    override def traverse[G[_]: Applicative, A, B](fa: IndexedData[F, A])(f: A => G[B]): G[IndexedData[F, B]] =
      f(fa.node.value).map(b => IndexedData(fa.index, fa.node.setValue(b)))
  }
}
