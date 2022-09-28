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
  
def multifailSchema = 
  tpe[IO, Unit](
    "Query", 
    "field" -> fallible(arg[Int]("i", Some(10))){ 
      case (_, 0) => IO.pure(Ior.left("fail gracefully"))
      case (_, 1) => IO.raiseError(new Exception("fail hard"))
      case (_, i) => IO.pure(Ior.right(i))
    }
  )

def go(query: String, tpe: Type[IO, Unit] = multifailSchema) = 
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
  
go("query { field }")
// Chain()
// res0: Either[parser.package.ParseError, io.circe.JsonObject] = Right(
//   value = object[data -> {
//   "field" : 10
// }]
// )
```

A query can fail gracefully by returning `Ior.left`:
```scala
go("query { field(i: 0) }")
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
```

A query can fail hard by raising an exception:
```scala
go("query { field(i: 1) }")
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
```

A query can also fail before even evaluating the query:
```scala
go("query { nonExisting }")
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
```

And finally, it can fail if it isn't parsable:
```scala
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

go(largerQuery).leftMap(_.prettyError.value)
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
Parser errors also look nice in ANSI terminals:

![Terminal output](./error_image.png)

### Exception trick
If for whatever reason you wish to pass information through exceptions, that is also possible:
```scala
final case class MyException(msg: String, data: Int) extends Exception(msg)

val res = 
  Schema.query(
    tpe[IO, Unit](
      "Query",
      "field" -> eff(_ => IO.raiseError[String](MyException("fail hard", 42)))
    )
  ).flatMap { sch =>
    sch.assemble("query { field } ", variables = Map.empty)
      .traverse { case Executable.Query(run) => run(()) }
  }.unsafeRunSync()
// res: Either[parser.package.ParseError, QueryResult] = Right(
//   value = QueryResult(
//     errors = Singleton(
//       a = EffectResolution(
//         path = CursorGroup(
//           startPosition = Cursor(path = Chain()),
//           relativePath = Cursor(
//             path = Singleton(a = Field(id = 1, name = "field"))
//           ),
//           groupId = 1
//         ),
//         error = Left(value = MyException(msg = "fail hard", data = 42)),
//         input = ()
//       )
//     ),
//     data = object[field -> null]
//   )
// )
  
res.toOption.flatMap(_.errors.headOption).flatMap(_.exception) match {
  case Some(MyException(_, data)) => println(s"Got data: $data")
  case _ => println("No data")
}
// Got data: 42
```
