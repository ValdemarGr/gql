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

def http4sCompiler: HttpRoutes[IO] = Http4sRoutes.syncSimple[IO]{ qp => 
  IO(Right(Compiler[IO].compileWith(mySchema, qp)))
}
```

## Implementing authorization
Something most applications need is authorization.
There is no build-in support for this, since authorization is usually very application specific.
However, it is very simple to implement the authorization your application needs.

Commonly, authorization is provided in the `Authorization` http header.
Consider the following authorization implementation, that also threads authorization credentials through the whole graph.
```scala mdoc:reset
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

def routes = Http4sRoutes.syncFull[IO]{ headers =>
  def unauth(msg: String) = 
    IO.pure(Left(Response[IO](Status.Unauthorized).withEntity(msg)))

  headers.get[Authorization] match {
    case None => unauth("missing authorization header")
    case Some(Authorization(Credentials.Token(AuthScheme.Bearer, token))) =>
      authorize(token).flatMap{
        case None => unauth("invalid token")
        case Some(creds) => IO.pure { 
          Right { (qp: QueryParameters) =>
            IO.pure{
              Right {
                Compiler[AuthIO]
                  .compileWith(schema, qp)
                  .map(_.mapK(Kleisli.applyK[IO, Creds](creds)))
              }
            }
          }
        }
      }
    case _ => unauth("invalid authorization header, use a Bearer Token")
  }
}
```
There is some wrapping in `Either` and `IO`, since we don't want to parse or prepare the query before authorization has been performed.

## Websocket support
To implement streaming, we use websockets.
The websocket route is implemented via a curried function, like the http route, to facilitate pre-parse authorization.

Parameters are passed in graphqlws via a json object payload.
The structure of this json payload is not constrained; this is up to the application to consider.

However, the norm is to embed http headers in the payload:
```json
{
  "authorization": "Bearer aHR0cHM6Ly93d3cueW91dHViZS5jb20vd2F0Y2g/dj1kUXc0dzlXZ1hjUQ=="
}
```

Returning `Left` of the query handler function lets the application return an error message to the client, which also immideately closes the websocket.
One can embed errors such as `unauthorized` here.
The GraphQL over websocket protocol defines no way to communicate arbitary information without closing the connection.
