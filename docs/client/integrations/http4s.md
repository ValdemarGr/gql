---
title: Http4s
---
```scala mdoc
import org.http4s.client._
import org.http4s.{Query => _, _}
import org.http4s.implicits._
import gql.parser._
import gql.client._
import gql.client.dsl._
import gql.client.http4s.syntax._
import cats.effect._
import cats.effect.unsafe.implicits.global

def q: Query.Compiled[String] = Query.named(
    QueryAst.OperationType.Query, 
    "MyQuery",
    sel[String]("name")
).compile

def client: Client[IO] = Client{ _ => 
    Resource.pure(Response[IO](Status.Ok).withEntity("""{"data":{"name":"John"}}"""))
}

def result: IO[String] = Request[IO](uri=uri"https://example.com/graphql").graphql(q, client)

result.unsafeRunSync()
```