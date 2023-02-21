---
title: Resolvers
---
Resolvers are at the core of gql; a resolver `Resolver[F, I, O]` takes an `I` and produces an `O` in effect `F`.
Resolvers are embedded in fields and act as continuations.
When gql executes a query it first constructs a tree of continueations from your schema and the supplied GraphQL query.

`Resolver`s act and compose like functions with combinators such as `andThen` and `compose`.

Lets start off with some imports:
```scala mdoc
import gql.dsl._
import gql.resolver._
import gql.ast._
import cats.effect._
import cats.implicits._
import cats.data._
```

## Resolvers
`Resolver` is a collection of high-level combinators that constructs a tree of `Step`.
:::note
If you are familiar with the relationship between `fs2.Stream` and `fs2.Pull`, then the relationship between `Resolver` and `Step` should be familiar.
:::
### Lift
The simplest `Resolver` type is `lift` which simply lifts a function `I => O` into `Resolver[F, I, O]`.
`lift`'s method form is `map`, which for any resolver `Resolver[F, I, O]` produces a new resolver `Resolver[F, I, O2]` given a function `O => O2`.
```scala mdoc:nest
val r = Resolver.lift[IO, Int](_.toLong)
r.map(_.toString())
```

### LiftF
Another simple resolver is `liftF` which lifts a function `I => F[O]` into `Resolver[F, I, O]`.
`liftF`'s method form is `evalMap`.
```scala mdoc:nest
val r = Resolver.liftF[IO, Int](i => IO(i.toLong))
r.evalMap(l => IO(l.toString()))
```
:::note
`liftF` is a combination of `lift` and then embedding the effect `F` into resolver.
The implementation of `liftF` is a composition of `Step.Alg.Lift` and `Step.Alg.EmbedEffect`.
Most resolvers are implemented or composed on by `lift`ing a function and composing an embedding.
:::

### Arguments
Arguments in gql are provided through resolvers.
A resolver `Resolver[F, I, A]` can be constructed from an argument `Arg[A]`, through either `argument` or `arg` in method form.
```scala mdoc:nest
val ageArg = arg[Int]("age")
val r = Resolver.argument[IO, Nothing, String](arg[String]("name"))
val r2 = r.arg(ageArg)
r2.map{ case (age, name) => s"$name is $age years old" }
```

`Arg` also has an applicative defined for it, so multi-argument resolution can be simplified to:
```scala mdoc:nest
val r = Resolver.argument[IO, Nothing, (String, Int)](
  (arg[String]("name"), arg[Int]("age")).tupled
)
r.map{ case (age, name) => s"$name is $age years old" }
```

### Meta
The `meta` resolver provides metadata regarding query execution, such as the position of query execution, field aliasing and the provided arguments.

### Errors
Well formed errors are returned in an `cats.data.Ior`.

:::info
An `Ior` is a non-exclusive `Either`.
:::

The `Ior` datatype's left side must be `String` and acts as an optional error that will be present in the query result.
gql can return an error and a result for the same path, given that `Ior` has both left and right side defined.

Errors are embedded into resolvers via `rethrow`.
The extension method `rethrow` is present on any resolver of type `Resolver[F, I, Ior[String, O]]`:
```scala mdoc:nest
val r = Resolver.lift[IO, Int](i => Ior.Both("I will be in the errors :)", i))
r.rethrow
```

### First
A `Resolver` also implements `first` (`Resolver[F, A, B] => Resolver[F, (A, C), (B, C)]`) which is very convinient since some `Resolver`s are constant functions (they throw away their arguments/`I`).
Since a `Resolver` does not form a `Monad`, `first` is necessary to implement non-trivial resolver compositions.
For instance, resolving an argument will ignore the input of the resolver, which is not always the desired semantics.

Lets assume we'd like to implement a resolver, that when given a name, can get a list of friends of the person with that name:
```scala mdoc
type PersonId = Int

case class Person(id: PersonId, name: String)

def getFriends(id: PersonId, limit: Int): IO[List[Person]] = ???

def getPerson(name: String): IO[Person] = ???

def getPersonResolver = Resolver.liftF[IO, String](getPerson)

def limitResolver = Resolver.argument[IO, Person, Int](arg[Int]("limit"))

getPersonResolver andThen 
  limitResolver.first[Person].contramap[Person](p => (p, p)) andThen
  Resolver.liftF{ case (limit, p) => getFriends(p.id, limit) }
```
The above example might be a bit tough to follow, but there methods available to make such formulations less gruesome.
```scala mdoc:nest
val limitArg = arg[Int]("limit")
getPersonResolver
  .arg(limitArg)
  .evalMap{ case (limit, p) => getFriends(p.id, limit) }
```

### Batch
Like most other GraphQL implementations, gql also supports batching.

Unlike most other GraphQL implementations, gql's batching implementation features a global query planner that lets gql delay field execution until it can be paired with another field.

Batch declaration and usage occurs as follows:
* Declare a function `Set[K] => F[Map[K, V]]`.
* Give this function to gql and get back a `Resolver[F, Set[K], Map[K, V]]` in a `State` monad (for unique id generation).
* Use this new resolver where you want batching.

And now put into practice:
```scala mdoc
case class Person(id: Int, name: String)

def getPeopleFromDB(ids: Set[Int]): IO[List[Person]] = ???

Resolver.batch[IO, Int, Person]{ keys => 
  getPeopleFromDB(keys).map(_.map(x => x.id -> x).toMap)
}
```

Whenever gql sees this resolver in any composition, it will look for similar resolvers during query planning.

Note, however, that you should only declare each batch resolver variant **once**, that is, you should build your schema in `State`. 
gql consideres different batch instantiations incompatible regardless of any type information.

State has `Monad` (and transitively `Applicative`) defined for it, so it composes well.
Here is an example of multiple batchers:
```scala mdoc
def b1 = Resolver.batch[IO, Int, Person](_ => ???)
def b2 = Resolver.batch[IO, Int, String](_ => ???)

(b1, b2).tupled
```
:::tip
Even if your field doesn't benefit from batching, batching can still do duplicate key elimination.
:::

#### Batch resolver syntax
When a resolver in a very specific form `Resolver[F, Set[K], Map[K, V]]`, then the gql dsl provides some helper methods.
For instance, it is very likely that a batcher is embedded in a singular context (`K => V`).
Here is a showcase of some of the helper methods:
```scala mdoc
def pb: Resolver[IO, Set[Int], Map[Int, Person]] = 
  // Stub implementation
  Resolver.lift(_ => Map.empty)

// None if a key is missing
pb.optionals[List]

// Emits all values
pb.values[List]

// Every key must have an associated value
// or else raise an error via a custom show-like typeclass
implicit lazy val showMissingPersonId =
  ShowMissingKeys.showForKey[Int]("not all people could be found")
pb.force[List]

// Maybe there is one value for one key?
pb.optional

// Same as optional
pb.optionals[cats.Id]

// There is always one value for one key
pb.forceOne

// Same as force but for non-empty containers
pb.forceNE[NonEmptyList]
```

:::tip
For larger programs, consider declaring all your batchers up-front and putting them into some type of collection:
```scala mdoc
case class MyBatchers(
  personBatcher: Resolver[IO, Set[Int], Map[Int, Person]],
  intStringBatcher: Resolver[IO, Set[Int], Map[Int, String]]
)

(b1, b2).mapN(MyBatchers.apply)
```
For most batchers it is likely that you eventually want to pre-compose them in various ways, for instance requsting args, which this pattern promotes.
:::

:::tip
Sometimes you have multiple groups of fields in the same object where each group have different performance overheads.

Say you had a `Person` object in your database.
This `Person` object also exists in a remote api.
This remote api can tell you, the friends of a `Person` given the object's id and name.

```scala mdoc:nest:silent
case class PersonId(id: Int)

def pureFields = fields[IO, PersonId](
  "id" -> lift(_.id)
)

case class PersonDB(
  id: PersonId, 
  name: String, 
  remoteApiId: String
)

// SELECT id, name, remote_api_id FROM person WHERE id in (...)
def dbBatchResolver: Resolver[IO, PersonId, PersonDB] = ???

def dbFields = fields[IO, PersonDB](
  "name" -> lift(_.name),
  "apiId" -> lift(_.remoteApiId)
)

case class PersonRemoteAPI(
  id: PersonId, 
  friends: List[PersonId]
)

// Given a PersonDB we can call the api (via a batched GET or something)
def personBatchResolver: Resolver[IO, PersonDB, PersonRemoteAPI] = ???

def remoteApiFields = fields[IO, PersonRemoteAPI](
  "friends" -> lift(_.friends)
)

// Given a PersonDB object we have the following fields
def dbFields2: Fields[IO, PersonDB] = 
  remoteApiFields.compose(personBatchResolver) ::: dbFields

// Given a PersonId we have every field
// If "friends" is selected, gql will first run `dbBatchResolver` and then `personBatchResolver`
def allFields = dbFields2.compose(dbBatchResolver) ::: pureFields

implicit def person: Type[IO, PersonId] = tpeNel[IO, PersonId](
  "Person",
  allFields
)
```

The general pattern for this decomposition revolves around figuring out what the most basic description of your object is.
In this example, every fields can (eventually through various side-effects) be resolved from just `PersonId`.

Notice that even if we had many more fields, the composition overhead remains constant.
:::

#### Batchers from elsewhere
Most batching implementations have compatible signatures and can be adapted into a gql batcher.

For instance, converting `fetch` to gql:
```scala mdoc:nest
import fetch._

case class PersonId(value: Int)
case class Person(id: PersonId, data: String)

object People extends Data[PersonId, Person] {
  def name = "People"

  def source: DataSource[IO, PersonId, Person] = ???
}

Resolver
  .batch[IO, PersonId, Person](_.toList.toNel.traverse(People.source.batch).map(_.getOrElse(Map.empty)))
```

### Choice
Resolvers also implement `Choice` via `(Resolver[F, A, C], Resolver[F, B, D]) => Resolver[F, Either[A, B], Either[C, D]]`.
On the surface, this combinator may have limited uses, but with a bit of composition we can perform tasks such as caching.

For instance, a combinator derived from `Choice` is `skippable: Resolver[F, I, O] => Resolver[F, Either[I, O], O]`, which acts as a variant of "caching".
If the right side is present we skip the underlying resolver (`Resolver[F, I, O]`) altogether.

More refined variants of this combinator also exist:
```scala mdoc:silent:nest
case class PersonId(id: Int)

case class Person(id: PersonId, data: String)

def getPersonForId: Resolver[IO, PersonId, Person] = ???

type CachedPerson = Either[PersonId, Person]
def cachedPerson = tpe[IO, CachedPerson](
  "Person",
  "id" -> lift(_.map(_.id).merge.id),
  "data" -> build[IO, CachedPerson](_.skipThat(getPersonForId).map(_.data))
)
```

We can also use some of the `compose` tricks from the [batch resolver syntax section](#batch-resolver-syntax) if we have a lot of fields that depend on `Person`. 

:::note
The query planner treats the choice branches as parallel, such that for two instances of a choice, resolvers in the two branches may be batched together.
:::

### Stream
The stream resolver embeds an `fs2.Stream` and provides the ability to emit a stream of results for a graphql subscription.

#### Stream semantics
* When one or more streams emit, the interpreter will re-evaluate the query from the position that emitted.
That is, only the sub-tree that changed will be re-interpreted.
* If two streams emit and one occurs as a child of the other, the child will be ignored since it will be replaced.
* By default, the interpreter will only respect the most-recent emitted data.

This means that by default, gql assumes that your stream should behave like a signal, not sequentially.
However, gql can also sequential semantics.

For instance a schema designed like the following, emits incremental updates regarding the price for some symbol:
```graphql
type PriceChange {
  difference: Float!
}

type Subscription {
  priceChanges(symbolId: ID!): PriceChange!
}
```

And here is a schema that represents an api that emits updates regarding the current price of a symbol:
```graphql
type SymbolState {
  price: Float!
}

type Subscription {
  price(symbolId: ID!): SymbolState!
}
```

Consider the following example where two different evaluation semantics are displayed:
```scala mdoc:silent:nest
case class PriceCahnge(difference: Float)
def priceChanges(symbolId: String): fs2.Stream[IO, PriceChange] = ???

case class SymbolState(price: Float)
def price(symbolId: String): fs2.Stream[IO, SymbolState] = ???

def priceChangesResolver = build[IO, String](_.sequentialStreamMap(priceChanges))

def priceResolver = build[IO, String](_.streamMap(price))
```

If your stream is sequential, gql will only pull elements when they are needed.

The interpreter performs a global re-interpretation of your schema, when one or more streams emit.
That is, the interpreter cycles through the following two phases:
* Interpret for the current values.
* Await new values.

## Steps
A `Step` is the low-level algebra for a resolver, that describes a single step of evaluation for a query.
The variants of `Step` are clearly listed in the source code. All variants of step provide orthogonal properties.

:::warning
this is not up to date
:::
Resolvers are the edges that connect fields and types.
Resolvers can be composed to build simple or complex edge strucures.

## Additional syntax for resolvers TODO

## PureResolver
The simplest resolver is the `PureResolver[F, I, A]`, which simply contains a function `I => A`.
Execution statistics for a `PureResolver` are not tracked.

## EffectResolver
The `EffectResolver[F, I, A]` is a resolver that contains a function `I => F[A]` where `F` is some effect type.

## FallibleResolver
Extending the `EffectResolver` with a possibility of failure leads us to the `FallibleResolver[F, I, A]` with the structure `I => F[Ior[String, A]]`.

:::note
The `EffectResolver` can be implemented via the `FallibleResolver`, but requires a `Functor` instance for `F`.

Having no typeclass constraints of `F` allows us to construct fields with only one implicit parameter; the type of the field.
This in turn allows passing the type of the field explicitly instead of caputuring it as an implicit parameter.
```scala 
import gql.dsl._
import gql.ast._
import cats.effect._

eff[IO, Unit, String](_ => IO("hey"))(stringScalar)
```
:::

## BatchResolver
The batch resolver `BatchResolver[F, I, A]` allows the interpreter to more effeciently fetch data.
The resolver captures a the following steps:
 - It takes `I` to `Set[K]` for some `K`
 - Then merges the keys `Set[K]` from many different resolvers into a single `Set[K]`
 - Then it fetches the values using a user-defined function `Set[K] => F[Map[K, T]]` for some `T`
 - Finally it maps the values `Map[K, T]` to `A`

The types `K` and `T` are existentially quantified inside of the `BatchResolver`.
When constructing a `BatchResolver[F, I, A]` for `K` and `V` then the `I = Set[K]` and `A = Map[K, V]`.
 
The `BatchResolver` must be constructed globally, that is, must not be constructed ad-hoc.
:::note
The `BatchResolver` cannot directly embed `Set[K] => F[Map[K, T]]`, since this would allow ambiguity.
What if two `BatchResolver`'s were to have their keys merged, what resolver's `Set[K] => F[Map[K, T]]` should be used?
:::

A `BatchResolver[F, K, T]` is constructed as follows:
```scala 
import gql.resolver._
import cats.effect._

val brState = BatchResolver[IO, Int, Int](keys => IO.pure(keys.map(k => k -> (k * 2)).toMap))
```
A `State` monad is used to keep track of the batchers that have been created and unique id generation.
During schema construction, `State` can be composed using `Monad`ic operations.
The `Schema` companion object contains smart constructors that run the `State` monad.

`mapBoth` and several `map` variants that exists for all `Resolver`s, can be used to align the input and output types:
```scala 
import gql._
import gql.dsl._
import gql.ast._
import cats._
import cats.implicits._

def batchSchema = brState.map { (br: BatchResolver[IO, Set[Int], Map[Int, Int]]) =>
  val adjusted: Resolver[IO, Int, Option[Int]] = br
    .contramap[Int](Set(_))
    .map(_.values.headOption)

  SchemaShape.make[IO](
    tpe[IO, Unit](
      "Query",
      "field" -> field(adjusted.contramap(_ => 42))
    )
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

Schema.stateful(batchSchema)
  .map(Compiler[IO].compile(_, query))
  .flatMap{ case Right(Application.Query(run)) => run.map(_.asGraphQL) }
  .unsafeRunSync()
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
  br.contramap[List[Int]](_.toSet).mapBoth{ case (i, m) => i.map(m.apply) }
```
:::
:::tip
For more information on how the batch resolver works, check out the [planning section](./../execution/planning.md).
:::
:::tip
The `BatchResolver` can also be used to fetch multiple fields of different value efficiently and lazily.

Say you had a document store that had may fields, but you only wanted to fetch a few of them.
You also don't want the interpreter to construct a new request for each field.
```scala 
type DocId = String

final case class DocumentQuery(id: DocId, field: String)
sealed trait DocumentValue

// Do some groupBy id to collect all requested fields for a DocId
def documentResolver: BatchResolver[IO, Set[DocumentQuery], Map[DocumentQuery, DocumentValue]] = ???

lazy val adjusted = documentResolver.contramap[DocumentQuery](Set(_)).mapBoth{ case (q, m) => m(q) }

implicit lazy val documentValue: Out[IO, DocumentValue] = ???

def document = tpe[IO, DocId](
  "Document",
  "someField" -> field(adjusted.contramap(DocumentQuery(_, "someField"))),
  "otherField" -> field(adjusted.contramap(DocumentQuery(_, "otherField"))),
  "anotherField" -> field(adjusted.contramap(DocumentQuery(_, "anotherField")))
)
```
:::

### Example of a database batcher
Most applications interact with a database one way or another.
Usually databases have a way to fetch multiple rows at once, and it is usually more efficient to do so.

Let's define our database:
```scala 
trait DatabaseConnection[F[_]] {
  def get(ids: Set[Int]): F[Map[Int, String]]
}

object DatabaseConnection {
  def apply[F[_]](implicit F: Applicative[F]) = new DatabaseConnection[F] {
    def get(ids: Set[Int]): F[Map[Int, String]] = {
      println(s"executing query for ids $ids")
      F.pure(ids.map(i => i -> s"row $i").toMap)
    }
  }
}
```
Now we can define our schema:
```scala 
final case class Nested(key: Int)

def databaseRoot[F[_]](implicit F: Monad[F], db: DatabaseConnection[F]) =
  BatchResolver[F, Int, String](keys => db.get(keys)).map { br =>
    val single = 
      br.contramap[Int](Set(_))
        .fallibleMap(x => F.pure(x.values.headOption.toRightIor("not found")))

    implicit lazy val nestedType = tpe[F, Nested](
      "Nested",
      "nestedValue" -> field(single.contramap[Nested](_.key))
    )
    
    SchemaShape.make[F](
      tpe[F, Unit](
        "Query",
        "getFirstField" -> field(arg[Int]("x"))(single.contramap{ case (_, x) => x}),
        "getSecondField" -> field(arg[Int]("y"))(single.contramap{ case (_, x) => x}),
        "nested" -> pure(_ => Nested(42))
      )
    )
  }
```

And finally execute it:
```scala 
def databaseQuery = """
  query {
    getFirstField(x: 1)
    getSecondField(y: 2)
    nested {
      nestedValue
    }
  }
"""

implicit def db = DatabaseConnection[IO]

Schema.stateful(databaseRoot[IO])
  .map(Compiler[IO].compile(_, databaseQuery))
  .flatMap{ case Right(Application.Query(run)) => run.map(_.asGraphQL) }
  .unsafeRunSync()
```

Notice how the huristic query planner is able to figure out that waiting till `nested` is resolved and then batching is more efficient than batching the two toplevel fields first and then resolving `nested`.

### Design patterns
Since `State` itself is a monad, we can compose them into `case class`es for more ergonomic implementation at scale.
```scala 
import cats.implicits._

trait User
trait UserId

trait Company
trait CompanyId

final case class DomainBatchers[F[_]](
  userBatcher: Resolver[F, Set[UserId], Map[UserId, User]],
  companyBatcher: Resolver[F, Set[CompanyId], Map[CompanyId, Company]],
  doubleSumBatcher: Resolver[F, Int, Int]
)

(
  BatchResolver[IO, UserId, User](_ => ???),
  BatchResolver[IO, CompanyId, Company](_ => ???),
  BatchResolver[IO, Int, Int](is => IO.pure(is.map(i => i -> (i * 2)).toMap))
    .map(_.contramap[Int](Set(_)).map[Int](_.values.toList.combineAll))
).mapN(DomainBatchers.apply)
```

## StreamResolver
The `StreamResolver` is a very powerful resolver type, that can perform many different tasks.
First and foremost a `StreamResolver` can update a sub-tree of the schema via some provided stream, like signals in frp or observables.
```scala 
def streamSchema = 
  SchemaShape[IO, Unit, Unit, Unit](
    tpe[IO, Unit]("Query", "queryCannotBeEmpty" -> pure(_ => 42)),
    subscription = tpe[IO, Unit](
      "Subscription",
      "stream" -> field(stream(_ => fs2.Stream(1).repeat.lift[IO].scan(0)(_ + _)))
    ).some
  )
```

### Stream semantics
A `StreamResolver` that occurs as a child another `StreamResolver` has some interesting implications.

The first time the interpreter sees a `StreamResolver`, it will await the first element and subscribe the rest of the stream to a background queue.

:::info
When a `StreamResolver` is interpreted during a query or mutation operation, only the head element is respected.
:::
:::note
Technically, a `StreamResolver` with one element can perform the same task as an `EffectResolver` by creating a single-element stream.
An `EffectResolver` has less resource overhead, since it is a single function compared to a `StreamResolver` that has some bookkeeping associated with it.
:::

When the first element arrives, the interpreter will continue interpreting the rest of the query.
That is, the interpreter will be blocked until the first element arrives.

:::caution
Streams may not be empty.
If stream terminates before at-least one element is emitted the subscription is terminated with an error.
If you for whatever reason wish to do this and permantently block a graphql subscription, you can always use `fs2.Stream.never`.
:::
:::danger
It is **highly** reccomended that every stream has at least one **guarenteed** element.
The initial evaluation of a (sub-)tree will never complete if a stream never emits, even if future updates cause a stream to become redundant.
:::

Whenever the tail of the stream emits, the interpreter will re-evaluate the sub-tree that occurs at the `StreamResolver`.
Re-evaluation forgets everything regarding the previous children, which includes `StreamResolver`s that may occur as children.
Forgotten `StreamResolver`s are gracefully cancelled.

Streaming is implemented in a sort of global re-evaluation loop.
Having global re-evaluation allows much more stable result emission and better batching possibilities.

:::note
If the reader is faimilar with the frontend framework `React`, this works much in the same way.
`useEffect` is analogous to resources in `fs2.Stream`, updates are batched and the interpreter diffs previous and new trees and then effeciently applies whatever changes are necessary.
:::

An alternative formulation could involve letting every `StreamResolver` have it's own re-evaluation loop, but this can have unforeseen consequences.
For instance, the implementation of nested streams becomes ambiguous.
Does an inner stream cancel when the outer emits? Does this mean that a fast outer stream can end up causing an inner stream to never emit?

:::note
The interpreter only respects elements arriving in streams that are considered "active".
That is, if a node emits but is also about to be removed because a parent has emitted, the interpreter will ignore the child's emission.
:::

### Interesting use cases
Since stream can embed `Resource`s, some very interesting problems can be solved with `StreamResolver`s.

Say we had a very slow connection to some VPN server that we wanted to fetch data from, but only if data from the VPN had been selected.
```scala 
import cats.effect.implicits._
import scala.concurrent.duration._

final case class VpnData(
  content: String,
  hash: String,
  connectedUser: String,
  serverId: String
)

type Username = String

trait VpnConnection[F[_]] {
  def getName: F[String]
  
  def getCreatedAge: F[Int]
  
  def getDataUpdates: fs2.Stream[F, VpnData]
}

object VpnConnection {
  // Connection aquisition is very slow
  def apply[F[_]](userId: Username, serverId: String)(implicit F: Async[F]): Resource[F, VpnConnection[F]] = {
    Resource.eval(F.monotonic).flatMap{ before =>
      Resource.makeFull[F, VpnConnection[F]]{ poll =>
        poll{
          F.delay(println(s"Connecting to VPN for $userId ...")) >>
          F.sleep(500.millis) >> 
          F.delay(println(s"Connected to VPN for $userId!"))
        }.onCancel(F.delay(println(s"Connection for $userId cancelled while connecting!"))).as{
          new VpnConnection[F] {
            def getName = F.delay("super_secret_file")
            
            def getCreatedAge = F.delay(42)
            
            def getDataUpdates = 
              fs2.Stream(1)
                .repeat
                .scan(0)(_ + _)
                .lift[F]
                .metered(50.millis)
                .map{ x => println(s"emitting for user $userId");x}
                .map(i => VpnData(s"content $i", s"hash of $i", userId, serverId))
          }
        }
      }(_ => F.monotonic.map{ after => 
        println(s"Disconnecting from VPN after ${(after - before).toMillis}ms for $userId ...")
      })
    }
  }
}
```

We could embed the VPN connection in a stream and pass it around to types that need it.
```scala 
final case class WithVpn[F[_], A](
  vpn: VpnConnection[F],
  value: A
)

final case class VpnMetadata(subscriptionTimestamp: String)

def currentTimestamp[F[_]](implicit F: Applicative[F]): F[String] = F.pure("now!")

implicit def vpnMetadata[F[_]: Applicative] = tpe[F, WithVpn[F, VpnMetadata]](
  "VpnMetadata",
  "name" -> eff(_.vpn.getName),
  "createdAge" -> eff(_.vpn.getCreatedAge),
  "subscriptionTimestamp" -> pure(_.value.subscriptionTimestamp),
)

implicit def vpnData[F[_]: Applicative] = tpe[F, VpnData](
  "VpnData",
  "content" -> pure(_.content),
  "hash" -> pure(_.hash),
  "connectedUser" -> pure(_.connectedUser),
  "serverId" -> pure(_.serverId)
)

implicit def vpn[F[_]: Applicative] = tpe[F, VpnConnection[F]](
  "Vpn",
  "metadata" -> eff(conn => currentTimestamp[F].map(ts => WithVpn(conn, VpnMetadata(ts)))),
  "data" -> field(stream(_.getDataUpdates))
)

def root[F[_]: Async] = 
  tpe[F, Username](
    "Subscription",
    "vpn" -> field(arg[String]("serverId"))(stream{ case (userId, serverId) => 
      fs2.Stream.resource(VpnConnection[F](userId, serverId))
    }),
    "me" -> pure(identity)
  )
```

We can now try querying the VPN connection through a GraphQL query:
```scala 
import gql.ast._

def subscriptionQuery = """
subscription {
  vpn(serverId: "secret_server") {
    metadata {
      name
      createdAge
      subscriptionTimestamp
    }
    data {
      content
      hash
      connectedUser
      serverId
    }
  }
}
"""

def runVPNSubscription(q: String, n: Int, subscription: Type[IO, Username] = root[IO]) = 
  Schema
    .simple(SchemaShape[IO, Unit, Unit, Username](
      tpe[IO, Unit]("Query", "queryCannotBeEmpty" -> pure(_ => 42)),
      subscription = subscription.some)
    )
    .map(Compiler[IO].compile(_, q, subscriptionInput = IO.pure("john_doe")))
    .flatMap{ case Right(Application.Subscription(stream)) => 
      stream.take(n).map(_.asGraphQL).compile.toList 
    }
  
runVPNSubscription(subscriptionQuery, 3).unsafeRunSync()
```
We can also check the performance difference of a queries that open a VPN connection versus and ones that don't:
```scala 
def bench(fa: IO[_]) = 
  for {
    before <- IO.monotonic
    _ <- fa.timed
    after <- IO.monotonic
  } yield s"duration was ${(after - before).toMillis}ms"
  
bench(runVPNSubscription(subscriptionQuery, 10)).unsafeRunSync()

bench(runVPNSubscription(subscriptionQuery, 3)).unsafeRunSync()

bench(runVPNSubscription(subscriptionQuery, 1)).unsafeRunSync()

def fastQuery = """
  subscription {
    me
  }
"""

bench(runVPNSubscription(fastQuery, 1)).unsafeRunSync()
```

Say that the VPN connection was based on credentials that needed to be refreshed every 600 milliseconds.
This is also possible:
```scala 
def accessToken[F[_]: Async](username: Username): fs2.Stream[F, Username] =
  fs2.Stream(username)
    .lift[F]
    .repeat
    .metered(600.millis)
    .zipWithIndex
    .map{ case (un, i) => s"token-$un-$i"}
    .map{x => println(s"a new token was issued: $x");x}

def root2[F[_]: Async] = 
  tpe[F, Username](
    "Subscription",
    "vpn" -> field(arg[String]("serverId"))(stream{ case (userId, serverId) => 
      accessToken[F](userId).flatMap{ token =>
        fs2.Stream.resource(VpnConnection[F](token, serverId))
      }
    })
  )
  
runVPNSubscription(subscriptionQuery, 13, root2[IO]).unsafeRunSync().takeRight(3)
```

## Resolver composition
Resolvers can also be composed via the `CompositionResolver`.
This means that the output of one resolver can be used as the input of another resolver.
This also means that `Stream`, `Batch` and `Effect` resolvers can be combined in any order.

For instance, one can efficiently fetch some list of ids, then subscribe to the data of each id and for all changed ids, efficiently fetch the changed data.
This can be achieved by composing `Batch`, `Stream` and then `Batch` again.
