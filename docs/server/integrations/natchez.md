---
title: Natchez (tracing)
---
The natchez package provides functions to trace your query execution and planning.

The tracing functions include information such as the query plan and query in case of an invalid query.

The easiest way to add tracing to your app is by tracing the schema via `traceSchema` and incoming queries via `traceQuery`.
For instance, consider the following tracing implementation for a http server:
```scala mdoc
import natchez._
import gql._
import gql.natchez.NatchezTracer
import cats.effect.{Trace => _, _}
import gql.http4s.Http4sRoutes

implicit def trace: Trace[IO] = ???

def schema: Schema[IO, Unit, Unit, Unit] = ???

def tracedSchema = NatchezTracer.traceSchema(schema)

def traceAndRunHttpRequest(qp: QueryParameters) =
    NatchezTracer.traceQuery[IO](qp.query, qp.variables.getOrElse(Map.empty), qp.operationName)(
        Compiler[IO].compileWith(tracedSchema, qp)
    )

def routes = Http4sRoutes.syncSimple[IO](traceAndRunHttpRequest(_).map(Right(_)))
```