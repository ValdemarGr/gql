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
package gql.preparation

import gql.GraphArc
import cats.mtl.Local
import gql.Cursor

trait PathAlg[F[_]] {
  def ambientEdge[A](edge: GraphArc)(fa: F[A]): F[A]

  def ambientField[A](name: String)(fa: F[A]): F[A] =
    ambientEdge(GraphArc.Field(name))(fa)

  def ambientIndex[A](i: Int)(fa: F[A]): F[A] =
    ambientEdge(GraphArc.Index(i))(fa)
}

object PathAlg {
  def apply[F[_]](implicit ev: PathAlg[F]): PathAlg[F] = ev

  implicit def pathAlgForLocal[F[_]](implicit L: Local[F, Cursor]): PathAlg[F] = new PathAlg[F] {
    override def ambientEdge[A](edge: GraphArc)(fa: F[A]): F[A] = L.local(fa)(_.add(edge))
  }
}
