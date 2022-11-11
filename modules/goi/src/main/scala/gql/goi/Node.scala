/*
 * Copyright 2022 Valdemar Grange
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
package gql.goi

import gql.ast._
import gql.dsl._
import cats.effect._
import java.util.Base64
import java.nio.charset.StandardCharsets
import cats.implicits._

trait Node

final case class NodeId(id: String)

trait NodeInstance[F[_], A] {
  def id(a: A): NodeId
}

object Node {
  def fromTypename[F[_], A](typename: String, get: String => F[Option[A]], id: A => String)(implicit F: Sync[F]) = {
    def get0(id: String) =
      F.delay(new String(Base64.getDecoder.decode(id), StandardCharsets.UTF_8)).map(_.split(':').toList).map {
        case x :: xs if x == typename => Some(get(xs.mkString(":")))
        case _                        => None
      }
    def id0(a: A) =
      F.delay(new String(Base64.getEncoder.encode(s"$typename:${id(a)}".getBytes()), StandardCharsets.UTF_8))
    (get0 _, id0 _)
  }

  def fromType[F[_]: Sync, A <: Node](get: String => F[Option[A]], id: A => String)(tpe: Type[F, A]) = {
    val (get0, id0) = fromTypename(tpe.name, get, id)
    // tpe.imple
  }
}
