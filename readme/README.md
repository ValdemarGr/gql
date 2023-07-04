# gql <a href="https://typelevel.org/cats/"><img src="https://typelevel.org/cats/img/cats-badge.svg" height="40px" align="right" alt="Cats friendly" /></a>

gql is a functional server and client GraphQL implementation for Scala.

It enables succintly exposing and consuming GraphQL APIs in a purely functional way.

* **Statically typed.** Statically typed through and trough.
* **Purely functional.** Uses the simplest and most precise abstractions.
* **Extensible.** From lightweight utility to spec-compliant schema transformations.
* **And so much more!**

To learn more, check out the [docs.](https://valdemargr.github.io/gql/)

# Example
```scala mdoc:silent
import gql.dsl._
import gql.ast._
import cats.effect._
import cats.implicits._

case class Human(name: String, friends: List[String])

def getFriend(name: String): IO[Human] = ???

implicit val human: Type[IO, Human] = tpe[IO, Human](
  "Human",
  "name" -> lift(h => h.name),
  "friends" -> eff(_.friends.traverse(getFriend))
)
```

# Developing and using gql
It is the mission of gql to provide the best developer experience possible, which in practice often means providing decriptive and precise error messages.

gql provides descriptive messages in case of query errors.
```graphql
| query MyQuery {
|     test.,test
| >>>>>^^^^^^^ line:2, column:16, offset:41, character code code:46
| }
```
And also when the schema has been defined incorrectly.
```scala mdoc:passthrough
val c1 = gql.Cursor.empty.field("Query").field("myField")
val p1 = gql.Validation.Problem(gql.Validation.Error.ArgumentNotDefinedInInterface("MyType", "MyInterface", "myField", "myArg"), c1)
val c2 = gql.Cursor.empty.field("Query").field("0hey")
val p2 = gql.Validation.Problem(gql.Validation.Error.InvalidFieldName("0hey"), c2)
println(s"""```\n// $p1\n// $p2\n```""")
```