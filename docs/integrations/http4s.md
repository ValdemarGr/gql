---
title: Http4s
---
The http4s integration consists of two routes, one for synchronous requests and one for websocket requests.

The two routes have different signatures since ordinary Request/Response queries use headers to pass most information, but grahpql websocket protocols pass it in a payload.

For the construction of a synchronous Request/Response route an extension to the `Compiler` structure named `Http4sCompiler` is required.
The `Http4sCompiler` interface is a superset of the `Compiler` interface in that it provides headers, and allows returning http responses.

The default implementation for a `Http4sCompiler` simply decodes

We can convert a `Compiler` to a `Http4sCompiler` by using the `Http4sCompiler.fromCompiler` function.
```scala mdoc
import cats.effect._
import cats.data._
import gql._
import gql.http4s._

def myCompiler: Compiler[IO] = ???

def http4sCompiler: Http4sCompiler[IO] = Http4sCompiler.fromCompiler(myCompiler)
```
Like `Compiler`, if you want to compose your own compiler from the underlying function that makes up a `Http4sCompiler`, that is also possible, and in fact, it is encrouaged.

## Implementing authorization
Something most applications need is authorization.
There is no build-in support for this, since authorization is usually very application specific.
However, it is very simple to implement the authorization your application needs in `gql`.

Commonly, authorization is provided in the `Authorization` http header.
Consider the following authorization implementation, that also threads authorization credentials through the whole graph.
```scala mdoc
final case class Creds(userId: String)

type AuthIO[A] = Kleisli[IO, Creds, A]

def schema: Schema[AuthIO, Unit, Unit, Unit] = ???
```
