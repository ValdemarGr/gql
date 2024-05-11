---
title: Http4s
---
The http4s integration consists of two routes, one for synchronous requests and one for websocket requests.

The two routes have different signatures since ordinary Request/Response queries use headers to pass most information, but grahpql websocket protocols pass it in a payload.

Consider the following example of a synchronous http4s route that serves a graphql api:
```scala mdoc:reset
import cats.effect._
import cats.data._
import org.http4s._
import gql._
import gql.http4s._

def mySchema: Schema[IO, Unit, Unit, Unit] = ???

def http4sCompiler: HttpRoutes[IO] = Http4sRoutes.sync[IO](path = "graphql"){ content => 
  // The caller must choose when to parse the content, for instance after authorization
  content.parse.flatMap(qp => Http4sRoutes.toResponse(Compiler[IO].compileWith(mySchema, qp)))
}
```

## Implementing authorization
Something most applications need is authorization.
There is no build-in support for this, since authorization is usually very application specific.

Commonly, authorization is provided in the `Authorization` http header.
Consider the following authorization implementation, that also threads authorization credentials through the whole graph.
```scala mdoc
import org.http4s._
import org.http4s.headers._
import org.http4s.dsl.io._
import cats.data._
import cats._
import cats.effect._
import gql._
import gql.http4s._

final case class Creds(userId: String)

type AuthIO[A] = Kleisli[IO, Creds, A]

def schema: Schema[AuthIO, Unit, Unit, Unit] = ???

def authorize(token: String): IO[Option[Creds]] = ???

def authorizeApp(compiled: Compiler.Outcome[AuthIO], creds: Creds): Compiler.Outcome[IO] =
  compiled.map(_.mapK(Kleisli.applyK[IO, Creds](creds)))

def unauth(msg: String): IO[Response[IO]] = 
  IO.pure(Response[IO](Status.Unauthorized).withEntity(msg))

def routes = Http4sRoutes.sync[IO](path="graphql"){ content =>
  content.request.headers.get[Authorization] match {
    case None => unauth("missing authorization header")
    case Some(Authorization(Credentials.Token(AuthScheme.Bearer, token))) =>
      authorize(token).flatMap{
        case None => unauth("invalid token")
        case Some(creds) => 
          content.parse.flatMap{ qp =>
            Http4sRoutes.toResponse {
              authorizeApp(Compiler[AuthIO].compileWith(schema, qp), creds)
            }
          }
      }
    case _ => unauth("invalid authorization header, use a Bearer Token")
  }
}
```

## Websocket support
To implement streaming, we use websockets.
Most websocket protocols start off by sending a payload with a json object containing headers.
Subsequent messages act as subscription queries or terminations of subscriptions.

The structure of this header payload is not constrained; this is up to the application to consider.

However, the norm is to embed http headers in the payload:
```json
{
  "authorization": "Bearer aHR0cHM6Ly93d3cueW91dHViZS5jb20vd2F0Y2g/dj1kUXc0dzlXZ1hjUQ=="
}
```

The `ConnectionInit[F]` trait is used to handle the initial payload.
Returning `Left`, lets the application return an error message to the client, which also immideately closes the websocket connection.
One can embed errors such as `unauthorized` here.

:::info
The GraphQL over websocket protocol defines no way to communicate arbitary information without closing the connection.
:::

The `ConnectionInit[F]`'s `init` function must return a `Subscribe[F]` that is used for every query after the initial payload.

Here is a full example of the usage:
```scala mdoc:silent
import org.http4s.server.websocket.WebSocketBuilder
import io.circe._
import gql.graphqlws.GraphqlWSServer._

def makeSubscribe(creds: Creds): Subscribe[IO] = new Subscribe[IO] {
  def subscribe(id: String, params: QueryParameters): fs2.Stream[IO, Response[IO]] = 
    Resource.pure(authorizeApp(Compiler[AuthIO].compileWith(schema, params), creds))
}

val init = new ConnectionInit[IO] {
  def init(headers: Map[String, Json]): IO[Either[String, Subscribe[IO]]] = 
    headers.get("authorization") match {
      case None => IO(Left("missing authorization header"))
      case Some(a) => 
        a.asString match {
          case None => IO(Left("authorization token must be a string"))
          case Some(a) => authorize(a).map{
            case None => Left("invalid token")
            case Some(creds) => Right(makeSubscribe(creds))
          }
        }
    }
}

def wsb: WebSocketBuilder[IO] = ???

def wsRoutes: HttpRoutes[IO] = Http4sRoutes.ws[IO](init, wsb)
```