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

:::info
The resolver will automatically construct a GraphQL error if any of the keys are missing.
To avoid this, you must pad all missing keys.
For instance, you could map all values to `Some` and pad all missing values with `None`.
:::
 
The `BatchResolver` must also have an implementation of `Set[K] => F[Map[K, T]]`, which is constructed globally.
:::note
The `BatchResolver` cannot directly embed `Set[K] => F[Map[K, T]]`, since this would allow ambiguity.
What if two `BatchResolver`'s were to have their keys merged, what resolver's `Set[K] => F[Map[K, T]]` should be used?
:::

A `BatchResolver[F, K, T]` is constructed as follows:
```scala
import gql.resolver._
import cats.effect._

val brState = BatchResolver[IO, Int, Int](keys => IO.pure(keys.map(k => k -> (k * 2)).toMap))
// brState: cats.data.package.State[gql.SchemaState[IO], BatchResolver[IO, Set[Int], Map[Int, Int]]] = cats.data.IndexedStateT@7ca1259
```
A `State` monad is used to keep track of the batchers that have been created and unique id generation.
During schema construction, `State` can be composed using `Monad`ic operations.
The `Schema` companion object contains smart constructors that run the `State` monad.

`map` and `contramap` can be used to align the input and output types:
```scala
import gql._
import gql.dsl._
import cats._

def batchSchema = brState.map { (br: BatchResolver[IO, Set[Int], Map[Int, Int]]) =>
  val adjusted: BatchResolver[IO, Int, Option[Int]] = br
    .contramap[Int](Set(_))
    .map { case (_, m) => m.values.headOption }

  SchemaShape[IO, Unit, Unit, Unit](
    tpe[IO, Unit](
      "Query",
      "field" -> field(adjusted.contramap(_ => 42))
    ),
    None,
    None
  )
}
```
:::note
There are more formulations of batch resolvers that can be possible, but the chosen one has the least overhead for the developer.

One could let the developer declare batch resolvers in-line and explicitly name them.
This would impose the validation constraint that all batch resolvers with the same name must have the same function address, or else there would be ambiguity.
Reasoning with function addreses is not very intuitive, so this is not the preferred formulation.
:::

Which we can finally run:
```scala
import cats.effect.unsafe.implicits.global
import cats.implicits._

def query = """
  query {
    field
  }
"""

Schema.stateful(batchSchema).flatMap{ sch =>
  sch.assemble(query, variables = Map.empty)
    .traverse { case ExecutableQuery.Query(run) => run(()).map(_.asGraphQL) }
}.unsafeRunSync()
// res0: Either[parser.package.ParseError, io.circe.JsonObject] = Right(
//   value = object[errors -> [
// ],data -> {
//   "field" : 84
// }]
// )
```
:::tip
The `BatchResolver` de-duplicates keys since it uses `Set` and `Map`.
This means that even if no function exists that effeciently fetches your data, you can still use the `BatchResolver` to de-duplicates it.
:::
:::tip
The `BatchResolver` does not maintain ordering internally, but this doesn't mean that the output values cannot maintain order.
```scala
def br: BatchResolver[IO, Set[Int], Map[Int, String]] = ???

def orderedBr: BatchResolver[IO, List[Int], List[String]] =
  br.contramap[List[Int]](_.toSet).map{ case (i, m) => i.map(m.apply) }
```
:::

### Design patterns
Since `State` itself is a monad, we can compose them into `case class`es for more ergonomic implementation at scale.
```scala
import cats.implicits._

trait User
trait UserId

trait Company
trait CompanyId

final case class DomainBatchers[F[_]](
  userBatcher: BatchResolver[F, Set[UserId], Map[UserId, User]],
  companyBatcher: BatchResolver[F, Set[CompanyId], Map[CompanyId, Company]],
  doubleSumBatcher: BatchResolver[F, Int, Int]
)

(
  BatchResolver[IO, UserId, User](_ => ???),
  BatchResolver[IO, CompanyId, Company](_ => ???),
  BatchResolver[IO, Int, Int](is => IO.pure(is.map(i => i -> (i * 2)).toMap)).map(
    _
    .contramap[Int](Set(_))
    .map[Int]{ case (_, m) => m.values.toList.combineAll }
  )
).mapN(DomainBatchers.apply)
// res1: data.IndexedStateT[Eval, SchemaState[IO], SchemaState[IO], DomainBatchers[[A]IO[A]]] = cats.data.IndexedStateT@17b89407
```

## StreamResolver
The `StreamResolver` is a very powerful resolver type, that can perform many different tasks.
First and foremost a `StreamResolver` can update a sub-tree of the schema via some provided stream:
```scala
def streamSchema = 
  SchemaShape[IO, Unit, Unit, Unit](
    tpe(
      "Query",
      "stream" -> field(stream(_ => fs2.Stream(1).repeat.lift[IO].scan(0)(_ + _)))
    ),
    None,
    None
  )
```

:::caution
Streams must not be empty.
If stream terminates before at-least one element is emitted the subscription is terminated with an error.
:::
:::danger
It is **highly** reccomended that every stream has at least one **guarenteed** element.
The initial evaluation of a (sub-)tree will never complete if a stream never emits, even if future updates cause the stream to be redundant.
:::
:::note
Technically, a `StreamResolver` with one element can perform the same task as an `EffectResolver` by creating a single-element stream.
An `EffectResolver` has less resource overhead, since it is a single function compared to a `StreamResolver` that has some bookkeeping associated with it.
:::
:::info
When a `StreamResolver` is interpreted during a query or mutation operation, no "background fiber" is spawned such that only the head element is respected.
:::

# Deprecated
## SignalResolver
The `SignalResolver` is a special type of resolver that can update itself.
A `SignalResolver[F, I, R, A]` contains:
* A reference to a stream of `R` that can be subscribed to via `I` (think `I => Stream[R]`).
* A function to get the initial value of the signal `I => F[R]`.
* A resolver that takes `(I, R)` to `F[A]`.

:::note
The initial value and the stream could technically have seperate resolvers.
If this need arises, please open an issue.
:::
:::info
The `SignalResolver`'s property of updating, is only relevant in subscriptions.
In queries and mutations, the stream part of the resolver is ignored.
:::

A `StreamRef` is a reference to a stream that can be subscribed to, it is the implementation of `I => Stream[R]`.
A `StreamRef` is constructed by suppling a subscription function `I => Resource[F, fs2.Stream[F, O]]`:
```scala
import scala.concurrent.duration._

val sr = 
  StreamRef[IO, Int, Int](i => Resource.pure{
    fs2.Stream(1).covary[IO].repeat.scan(i)(_ + _).metered(1.second)
  })
```
TODO subscription


With a `StreamRef`, we can construct a `SignalResolver`:
```scala
def signalSchema = sr.map { s =>
  val adjusted = 
    s.contramap[(Unit, Int)]{ case (_, i) => i}

  SchemaShape[IO, Unit](
    tpe(
      "Query",
      "field" -> signal(arg[Int]("initial"), adjusted).pure(_ => 0).pure{ case (_, i) => i }
    )
  )
}
```

A `SignalResolver` is id'ed much like a `BatchResolver`, but with the twist that the input value to the `StreamRef` also acts as a key.
This means in the above example, any subscription to the same `i` will share the same stream, even latecomers.
:::note
If you need to specify both an input to your stream and a key as two seperate parameters then please open an issue.
:::

This also means that if multiple `SignalResolver`'s are subscribed to the same stream, then their resolvers will be evaluated in the same iteration.

When a `SignalResolver` updates, it forgets it's entire sub-tree and resolves it again.
That also means that a `SignalResolver` that occurs as a direct or transitive child of another `SignalResolver` which updates, will have its subscription terminated.
If the same child `SignalResolver` occurs again in the same sub-tree, it will be re-subscribed to.
New subscriptions are always performed before old ones are terminated, such that in the above case the stream will never be closed.

:::tip
The `Resource` in the `StreamRef` is opened before the initial value is fetched.
This allows the caller to have full control over the order of operations.
As such, complicated delivery schemes can be implemented such as at least once, at most once or even exactly once, if your data can implement it (versioned data).
:::

