---
title: Error handling
---
There are different types of errors in gql.

* Schema validation errors, which should be caught in development.
These are for instance caused by duplicate field names or invalid typenames.
* Query preparation errors, which are errors caused by invalid queries.
* Execuion errors. These are errors that occur during query evaluation, caused by resolvers that fail.

## Execution
Error handling in gql can be performed in two ways, it can be returned explicitly or raised in `F`.
For instance, the `EffectResolver[F, I, A]` wraps the function `I => F[Ior[String, A]]`.

## Examples
Let's setup the scene:
```scala mdoc
import gql.ast._
import gql.dsl._
import gql._
import cats.implicits._
import cats.data._
import cats.effect._
import cats.effect.unsafe.implicits.global

def go(tpe: Type[IO, Unit], query: String) = 
  Schema.query(tpe).flatMap { sch =>
    sch.assemble(query, variables = Map.empty)
      .traverse { 
        case Executable.Query(run) => 
          run(()).map{x => println(x.errors);x.asGraphQL }
        case Executable.ValidationError(msg) =>
          println(msg)
          IO.pure(msg.asGraphQL)
      }
  }.unsafeRunSync()
  
def multifailSchema = 
  tpe[IO, Unit](
    "Query", 
    "field" -> fallible(arg[Int]("i", Some(10))){ 
      case (_, 0) => IO.pure(Ior.left("fail gracefully"))
      case (_, 1) => IO.raiseError(new Exception("fail hard"))
      case (_, i) => IO.pure(Ior.right(i))
    }
  )
  
go(multifailSchema, "query { field }")

go(multifailSchema, "query { field(i: 0) }")

go(multifailSchema, "query { field(i: 1) }")

go(multifailSchema, "query { nonExisting }")

def largerQuery = """
  query {
    field1
    field2(test: 42)
  }
  
  fragment test on Test {
    -value1
    value2 
  }
"""
go(multifailSchema, largerQuery).leftMap(_.prettyError.value)
```
Parser errors look nice in ANSI terminals:

![Terminal output](./error_image.png)
