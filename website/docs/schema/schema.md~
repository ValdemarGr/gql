---
title: The schema
---
## SchemaShape
The `SchemaShape` consists of the roots that make up your gql schema; A query, mutation and subscription type.
The `SchemaShape` also contains extra types that should occur in the schema but are not neccesarilly discoverable through a walk of the ast.

The `SchemaShape` also has derived information embedded in it.
For instance, one can render the schema:
```scala mdoc
import cats.effect._
import cats.implicits._
import gql._
import gql.ast._
import gql.dsl._

def ss = SchemaShape[IO](
  tpe[IO, Unit](
    "Query",
    "4hello" -> pure(_ => "world")
  )
)

println(ss.render)
```

### Validation
Validation of the shape is also derived information:
```scala mdoc
println(ss.validate)
```
Running validation is completely optional, but is highly recommended.
Running queries against a unvalidated schema can have unforseen consequences.

For instance, here is a non-exhaustive list of things that can go wrong if not validated:
 - Unforseen runtime errors if two definitions of a type has diverging field definitions.
 - Names do not respect the graphql spec.
 - Missing interface field implementations.
 - Invalid default value structure.

Validation also reports other non-critical issues such as cases of ambiguity.

For instance, if a cyclic type is defined with `def`, validation cannot determine if the type is truely valid.
Solving this would require an infinite amount of time.
An exmaple follows:
```scala mdoc
final case class A()

def cyclicType(i: Int): Type[IO, A] = {
  if (i < 10000) tpe[IO, A](
    "A",
    "a" -> pure((_: A) => A())(cyclicType(i + 1))
  )
  else tpe[IO, A](
    "A",
    "a" -> pure(_ => "now I'm a string :)")
  )
}

implicit lazy val cyclic: Type[IO, A] = cyclicType(0)

def recursiveSchema = SchemaShape[IO](
  tpe[IO, Unit](
    "Query",
    "a" -> pure(_ => A())
  )
)

recursiveSchema.validate.toList.mkString("\n")
```
One can also choose to simply ignore some of the validation errors:
```scala mdoc
recursiveSchema.validate.filter{
  case SchemaShape.Problem(SchemaShape.ValidationError.CyclicOutputType("A"), _) => false
  case _ => true
}
```
:::info
Validation does not attempt structural equallity since this can have unforseen performance consequences.

For instance, if the whole graph was defined with `def`s, one could very easily accedentally construct a case of exponential running time.
:::

`gql` will use the specified implementation of a type when declaring relation to another type.

## Schema
A `Schema` is a collection of some components that are required to execute a query.
The `Schema` contains a `SchemaShape`, a `Statistics` instance, a query `Planner` implementation and state regarding `BatchResolver` implementations.
:::tip
Check out the [statistics section](../execution/statistics) for more information on the `Statistics` object.

Also, check out the [planning section](../execution/planning) for more information on how the default query planner works.

Finally, you can look in the [resolver section](./resolvers) for more information on `BatchResolver`s.
:::

The most powerful `Schema` constructor `stateful`, converts a `State[SchemaState[F], SchemaShape[F, Q, M, S]]` to a `Schema[F, Q, M, S]`.
