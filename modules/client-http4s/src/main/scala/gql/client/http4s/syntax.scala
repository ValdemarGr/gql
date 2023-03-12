package gql.client.http4s

import gql.client.Query
import org.http4s.Request
import org.http4s.circe._
import io.circe.syntax._
import org.http4s.client.Client
import cats.effect._

object syntax {
    implicit class Http4sCompiledQueryOps[A](private val c: Query.Compiled[A]) extends AnyVal {
        def http4sRequest[F[_]]: Request[F] = 
            Request[F](method = org.http4s.Method.POST).withEntity(c.toJson.asJson)

        def fetch[F[_]: Concurrent](client: Client[F]): F[A] = {
            implicit val dec = c.decoder
            implicit val ed = org.http4s.circe.jsonOf[F, A]
            client.expect[A](c.http4sRequest[F])
        }
        
        def fetch[F[_]: Concurrent](implicit client: Client[F]): F[A] = 
            fetch[F](client)

        //def subscribe[F[_]](client: WSClient)
    }
}
