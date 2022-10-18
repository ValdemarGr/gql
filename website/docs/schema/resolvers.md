---
title: Resolvers
---
Resolvers are where the most interest should be placed, since they act as the layer between input type and next continuation;
Resolvers are effectively the edges in the graph.

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
import gql.ast._
import cats._
import cats.implicits._

def batchSchema = brState.map { (br: BatchResolver[IO, Set[Int], Map[Int, Int]]) =>
  val adjusted: BatchResolver[IO, Int, Option[Int]] = br
    .contramap[Int](Set(_))
    .map { case (_, m) => m.values.headOption }

  SchemaShape[IO, Unit, Unit, Unit](
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
```scala mdoc
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
```scala mdoc
def br: BatchResolver[IO, Set[Int], Map[Int, String]] = ???

def orderedBr: BatchResolver[IO, List[Int], List[String]] =
  br.contramap[List[Int]](_.toSet).map{ case (i, m) => i.map(m.apply) }
```
:::
:::tip
For more information on how the batch resolver works, check out the [planning section](./../execution/planning.md).
:::
:::tip
The `BatchResolver` can also be used to fetch multiple fields of different value efficiently and lazily.

Say you had a document store that had may fields, but you only wanted to fetch a few of them.
You also don't want the interpreter to construct a new request for each field.
```scala mdoc:silent
type DocId = String

final case class DocumentQuery(id: DocId, field: String)
sealed trait DocumentValue

// Do some groupBy id to collect all requested fields for a DocId
def documentResolver: BatchResolver[IO, Set[DocumentQuery], Map[DocumentQuery, DocumentValue]] = ???

lazy val adjusted = documentResolver.contramap[DocumentQuery](Set(_)).map{ case (q, m) => m(q) }

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
```scala mdoc
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
```scala mdoc
final case class Nested(key: Int)

def databaseRoot[F[_]: Monad](implicit db: DatabaseConnection[F]) =
  BatchResolver[F, Int, String](keys => db.get(keys)).map { br =>
    val single = br.contramap[Int](Set(_)).map { case (_, m) => m.values.head }

    implicit lazy val nestedType = tpe[F, Nested](
      "Nested",
      "nestedValue" -> field(single.contramap[Nested](_.key))
    )
    
    SchemaShape[F, Unit, Unit, Unit](
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
```scala mdoc
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
```scala mdoc
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
```

## StreamResolver
The `StreamResolver` is a very powerful resolver type, that can perform many different tasks.
First and foremost a `StreamResolver` can update a sub-tree of the schema via some provided stream, like signals in frp or observables.
```scala mdoc
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
If you for whatever reason whish to do this and permantently block a graphql subscription, you can always use `fs2.Stream.never`.
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
That is, if a node emits but is also about to be removed because a parent has emitted, the interpreter will ignore the emission.
:::

### Interesting use cases
Since stream can embed `Resource`s, some very interesting problems can be solved with `StreamResolver`s.

Say we had a very slow connection to some VPN server that we wanted to fetch data from, but only if data from the VPN had been selected.
```scala mdoc
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
```scala mdoc
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
```scala mdoc
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
We can alsa check the performance difference of a queries that open a VPN connection, and ones that don't:
```scala mdoc
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

Say that the VPN connection was based on a OAuth token that needed to be refreshed every 600 milliseconds.
This is also possible:
```scala mdoc
def oauthAccessToken[F[_]: Async](username: Username): fs2.Stream[F, Username] =
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
      oauthAccessToken[F](userId).flatMap{ token =>
        fs2.Stream.resource(VpnConnection[F](token, serverId))
      }
    })
  )
  
runVPNSubscription(subscriptionQuery, 13, root2[IO]).unsafeRunSync().takeRight(3)
```

## Resolver composition
Resolvers can also be composed via the `andThen` method that exists on all resolvers.
This means that the output of one resolver can be used as the input of another resolver.
This also means that `Stream`, `Batch` and `Effect` resolvers can be combined in any order.

For instance, one can efficiently fetch some list of ids, then subscribe to the data of each id and for all changed ids, efficiently fetch the changed data.
This can be achieved by composing `Batch`, `Stream` and then `Batch` again.
