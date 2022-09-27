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
```scala
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
// Chain()
// res0: Either[parser.package.ParseError, io.circe.JsonObject] = Right(
//   value = object[data -> {
//   "field" : 10
// }]
// )

go(multifailSchema, "query { field(i: 0) }")
// Chain(EffectResolution(CursorGroup(Cursor(Chain()),Cursor(Chain(Field(1,field))),1),Right(fail gracefully),()))
// res1: Either[parser.package.ParseError, io.circe.JsonObject] = Right(
//   value = object[errors -> [
//   [
//     {
//       "message" : "fail gracefully",
//       "path" : [
//         "field"
//       ]
//     }
//   ]
// ],data -> {
//   "field" : null
// }]
// )

go(multifailSchema, "query { field(i: 1) }")
// Chain(EffectResolution(CursorGroup(Cursor(Chain()),Cursor(Chain(Field(1,field))),1),Left(java.lang.Exception: fail hard),()))
// res2: Either[parser.package.ParseError, io.circe.JsonObject] = Right(
//   value = object[errors -> [
//   [
//     {
//       "message" : "internal error",
//       "path" : [
//         "field"
//       ]
//     }
//   ]
// ],data -> {
//   "field" : null
// }]
// )

go(multifailSchema, "query { nonExisting }")
// PositionalError(PrepCursor(List(nonExisting)),List(Caret(0,20,20)),unknown field name nonExisting)
// res3: Either[parser.package.ParseError, io.circe.JsonObject] = Right(
//   value = object[message -> "unknown field name nonExisting",locations -> [
//   {
//     "line" : 0,
//     "column" : 20
//   }
// ],path -> [
//   "nonExisting"
// ]]
// )

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
// res4: Either[String, io.circe.JsonObject] = Left(
//   value = """failed at offset 80 on line 7 with code 45
// one of "..."
// in char in range A to Z (code 65 to 90)
// in char in range _ to _ (code 95 to 95)
// in char in range a to z (code 97 to 122)
// in query:
// | 
// |   query {
// |     field1
// |     field2(test: 42)
// |   }
// |   
// |   fragment test on Test {
// |     -value1
// | >^^^^^^^ line:7 code:45
// |     value2 
// |   }
// | """
// )
```
Parser errors look nice in ANSI terminals:

![Terminal output](./error_image.png)
