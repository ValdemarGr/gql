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
package gql.client.http4s

import gql.client.Query
import org.http4s.Request
import org.http4s.circe._
import io.circe.syntax._
import org.http4s.client.Client
import cats.effect._
import org.http4s.EntityDecoder

object syntax {
  implicit class Http4sCompiledQueryOps[A](private val c: Query.Compiled[A]) extends AnyVal {
    def http4sRequest[F[_]]: Request[F] =
      Request[F](method = org.http4s.Method.POST).withEntity(c.toJson.asJson)

    def fetch[F[_]: Concurrent](client: Client[F]): F[A] = {
      implicit val dec: io.circe.Decoder[A] = c.decoder
      implicit val ed: EntityDecoder[F, A] = org.http4s.circe.jsonOf[F, A]
      client.expect[A](c.http4sRequest[F])
    }

    def fetch[F[_]: Concurrent](implicit client: Client[F]): F[A] =
      fetch[F](client)
  }
}
