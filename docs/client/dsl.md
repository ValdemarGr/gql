---
title: Query DSL
---
gql provides a dsl for building graphql queries and response parsers.
When you compose your query with the dsl, you automatically compose both a query and a json decoder for the query response.

## Selections
The simplest combinator is `sel` which declares a field selection:
```scala mdoc
import gql.client._
import gql.client.dsl._
import cats.implicits._

sel[Option[String]]("name")
```
Most combinators in the dsl have multiple overloads to provide various features.
```scala mdoc:silent
sel[Option[String]]("name", alias="n")

sel[Option[String]]("name", arg("id", 42))
```

Every selection related structure forms an `Applicative` such that you can compose multiple selections together:
```scala mdoc:silent
val s1 = sel[Option[String]]("name")

val s2 = sel[Option[Int]]("age")

val s3: SelectionSet[(Option[String], Option[Int])] = (s1, s2).tupled

final case class PersonQuery(name: Option[String], age: Option[Int])

val pq: SelectionSet[PersonQuery] = (s1, s2).mapN(PersonQuery.apply)
```

Queries can also act as sub-selections (`SubQuery` in gql):
```scala mdoc:silent
sel[PersonQuery]("person") {
    pq
}
```

In the first examples the sub-query is captured implicitly.
We can also do this for custom types:
```scala mdoc:silent
implicit val pq2: SelectionSet[PersonQuery] = pq

sel[PersonQuery]("person")
```

## Fragments
Like in graphql we can define fragments to reuse selections:
```scala mdoc:silent
val frag = fragment[String]("MyFragment", on="Person") {
    sel[String]("name")
}

val fragmentSpreads = sel[(Option[String], Option[Int])]("person") {
    (
        frag,
        inlineFrag[Int]("Person") {
            sel[Int]("age")
        }
    ).tupled
}
```
Notice that both `fragment` and `inlineFrag` return an optional result.
This is because the spread may not match on the type (if the spread condition is a sub-type of the spread-on type).
This is not always the desired behavior, and as such, fragments can be required:
```scala mdoc:silent
frag.required: SelectionSet[String]
```
You can provide additional information, should the fragment turn out to actually be missing:
```scala mdoc:silent
frag.requiredFragment("MyFragment", on="Person")
```

:::info
Fragments should be preferred over re-using selections directly to reduce the rendered query size.
:::

## Variables

Variables are accumulated into a sort of writer monad, such that they can be declared ad-hoc:
```scala mdoc
variable[String]("name")
```
Variables can be combined with the `~` operator:
```scala mdoc
variable[String]("name") ~ variable[Int]("age")
```
Variables can also be declared as omittable, optionally with a default value:
```scala mdoc
omittableVariable[String]("name", value("John")) ~
    omittableVariable[Int]("age")
```

Variables can be "materialized" into a `VariableClosure` by introducing them to a query:
```scala mdoc:silent
// Given a variable of type String, we can construct a query that returns an Int
val queryWithVariable: VariableClosure[String, Int] = 
    variable[String]("name").introduce{ name: VariableName[String] =>
        sel[Int]("id", arg("name", name))
    }
```

`VariableClosure` can be combined via `~` and have their selections modified via `modify`:
```scala mdoc:silent
def subQuery1: VariableClosure[String, Int] = queryWithVariable

def subQuery2: VariableClosure[String, Int] = 
    variable[String]("name2").introduce{ name: VariableName[String] =>
        sel[Int]("id2", arg("name", name))
    }

def combined: VariableClosure[(String, String), Int] = 
 (subQuery1 ~ subQuery2).modify(_.map{ case (v1, v2) => v1 + v2 })

// VariableClosure also forms a profunctor so we can also use rmap
(subQuery1 ~ subQuery2).rmap{ case (v1, v2) => v1 + v2 }
```

## Execution
Once a query has been constructed, there are three ways to wrap it together.
`simple` if the query is parameter-less and name-less, `named` if your query is named and `parameterized` if it is both named and parameterized:
```scala mdoc
import gql.parser.QueryAst.OperationType
def simpleQuery = Query.simple(
    OperationType.Query,
    sel[Unit]("person") {
        (
            sel[Int]("id"),
            sel[Int]("age", arg("numbers", List(42)))
        ).tupled.void
    }
)

simpleQuery.compile.query

Query.named(
    OperationType.Mutation,
    "MyMutation",
    sel[String]("name")
).compile.query

def paramQuery = Query.parameterized(
    OperationType.Subscription,
    "MySubscription",
    combined
)

def compiledParamQuery = paramQuery.compile(("first", "second"))
compiledParamQuery.query

compiledParamQuery.variables
```