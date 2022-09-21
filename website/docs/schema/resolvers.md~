---
title: Resolvers
---
Resolvers are where the most interest should lie, since they act as the layer between input type and next continuation.
The raw resolver types are as expressive as possible to allow as many use cases as possible, which can cause a lot of noise in the daily use of gql.
Therefore the `dsl` should be enough to get started and this section should act as an introduction for the curious.

:::note
The error types have been omitted from the resolver types for brevity.
:::

## EffectResolver
The simplest resolver is the effect resolver `EffectResolver[F, I, A]` which takes `I` to `F[A]`.

## BatchResolver
The batch resolver `BatchResolver[F, I, A]` allows the interpreter to more effeciently fetch data.
The resolver captures a the following steps:
 - It takes `I` to `F[Set[K]]` for some `K`
 - Then merges the keys `Set[K]` from many different resolvers into a single `Set[K]`
 - Then it fetches the values using a user-defined function `Set[K] => F[Map[K, T]]` for some `T`
 - Finally it maps the values `Map[K, T]` to `F[A]`

The types `K` and `T` are existentially quantified; they are not visible to the user.
The base-case implementation for `BatchResolver` has `K = Set[I]` and `T = Map[I, A]`.

:::note
The resolver will automatically construct a GraphQL error if any of the keys are missing.
To avoid this, you must pad all missing keys.
For instance, you could map all values to `Some` and pad all missing values with `None`.
:::
 
The `BatchResolver` must also have an implementation of `Set[K] => F[Map[K, T]]`, which is constructed globally.
:::info
The `BatchResolver` cannot directly embed `Set[K] => F[Map[K, T]]`, since this would allow ambiguity.
What if two `BatchResolver`'s were to have their keys merged, what resolver's `Set[K] => F[Map[K, T]]` should be used?
:::

A `BatchResolver[F, K, T]` is constructed as follows:
```scala mdoc
import gql.resolver._
import cats.effect._

val brState = BatchResolver[IO, Int, Int](keys => IO.pure(keys.map(k => k -> (k * 2)).toMap))
```
A `State` monad is used to keep track of the batchers that have been created and unique id generation.
During schema construction, `State` can be composed using `Monad`ic operations.
The `Schema` companion object contains smart constructors that run the `State` monad.

`map` and `contramap` can be used to align the input and output types:
```scala mdoc
import gql._
import gql.dsl._
import cats._

def statefulSchema = brState.map { (br: BatchResolver[IO, Set[Int], Map[Int, Int]]) =>
  val adjusted: BatchResolver[IO, Int, Option[Int]] = br
    .contramap[Int](Set(_))
    .map { case (_, m) => m.values.headOption }

  SchemaShape[IO, Unit](
    tpe(
      "Query",
      "field" -> field(adjusted.contramap(_ => 42))
    )
  )
}
```

Which we can finally run:
```scala mdoc
import cats.effect.unsafe.implicits.global
def s = Schema.stateful(statefulSchema)
                                                                                        
def query = """
  query {
    field
  }
"""
                                                                                        
implicit def stats = Statistics[IO].unsafeRunSync()
                                                                                        
def parsed = gql.parser.parse(query).toOption.get
                                                                                        
def program = Execute.executor(parsed, s, Map.empty) match {
  case Execute.ExecutorOutcome.Query(run) => run(()).map { case (_, output) => output }
}
                                                                                        
program.unsafeRunSync()
```
