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
package gql.client.http4s

import gql.client.Query
import org.http4s.Request
import org.http4s.circe._
import org.http4s.client.Client
import cats.effect._
import org.http4s.EntityDecoder
import org.http4s._

object syntax {
  def gqlEntityDecoderInstance[F[_]: Concurrent, A](c: Query.Compiled[A]): EntityDecoder[F, A] = {
    implicit val dec: io.circe.Decoder[A] = c.decoder
    org.http4s.circe.jsonOf[F, A]
  }

  implicit def gqlEntityEncoderForQuery[A]: EntityEncoder.Pure[Query.Compiled[A]] = {
    org.http4s.circe.jsonEncoderOf[Query.Compiled[A]]
  }

  implicit class GqlHttp4sRequestOps[F[_]](private val req: Request[F]) extends AnyVal {
    def graphql[A](q: Query.Compiled[A], client: Client[F])(implicit F: Concurrent[F]): F[A] = {
      import io.circe.syntax._
      implicit val ed: EntityDecoder[F, A] = gqlEntityDecoderInstance(q)
      client.expect[A](req.withMethod(Method.POST).withEntity(q.asJson))
    }

    def graphql[A](q: Query.Compiled[A])(implicit F: Concurrent[F], client: Client[F]): F[A] =
      graphql[A](q, client)
  }
}
