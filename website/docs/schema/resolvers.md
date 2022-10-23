---
title: Resolvers
---
Resolvers are the edges that connect fields and types.
Resolvers can be composed to build simple or complex edge strucures.

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
// brState: cats.data.package.State[gql.SchemaState[IO], BatchResolver[IO, Set[Int], Map[Int, Int]]] = cats.data.IndexedStateT@11ca3da9
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

  SchemaShape[IO](
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
// res1: io.circe.JsonObject = object[data -> {
//   "field" : 84
// }]
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
    
    SchemaShape[F](
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
// executing query for ids Set(1, 2, 42)
// res2: io.circe.JsonObject = object[data -> {
//   "nested" : {
//     "nestedValue" : "row 42"
//   },
//   "getSecondField" : "row 2",
//   "getFirstField" : "row 1"
// }]
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
// res3: data.IndexedStateT[Eval, SchemaState[IO], SchemaState[IO], DomainBatchers[[A]IO[A]]] = cats.data.IndexedStateT@44730ea2
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
// Connecting to VPN for john_doe ...
// Connected to VPN for john_doe!
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// Disconnecting from VPN after 683ms for john_doe ...
// res4: List[io.circe.JsonObject] = List(
//   object[data -> {
//   "vpn" : {
//     "data" : {
//       "serverId" : "secret_server",
//       "connectedUser" : "john_doe",
//       "hash" : "hash of 0",
//       "content" : "content 0"
//     },
//     "metadata" : {
//       "subscriptionTimestamp" : "now!",
//       "createdAge" : 42,
//       "name" : "super_secret_file"
//     }
//   }
// }],
//   object[data -> {
//   "vpn" : {
//     "data" : {
//       "serverId" : "secret_server",
//       "connectedUser" : "john_doe",
//       "hash" : "hash of 1",
//       "content" : "content 1"
//     },
//     "metadata" : {
//       "subscriptionTimestamp" : "now!",
//       "createdAge" : 42,
//       "name" : "super_secret_file"
//     }
//   }
// }],
//   object[data -> {
//   "vpn" : {
//     "data" : {
//       "serverId" : "secret_server",
//       "connectedUser" : "john_doe",
//       "hash" : "hash of 2",
//       "content" : "content 2"
//     },
//     "metadata" : {
//       "subscriptionTimestamp" : "now!",
//       "createdAge" : 42,
//       "name" : "super_secret_file"
//     }
//   }
// }]
// )
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
// Connecting to VPN for john_doe ...
// Connected to VPN for john_doe!
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// Disconnecting from VPN after 1010ms for john_doe ...
// res5: String = "duration was 1025ms"

bench(runVPNSubscription(subscriptionQuery, 3)).unsafeRunSync()
// Connecting to VPN for john_doe ...
// Connected to VPN for john_doe!
// emitting for user john_doe
// emitting for user john_doe
// emitting for user john_doe
// Disconnecting from VPN after 668ms for john_doe ...
// res6: String = "duration was 680ms"

bench(runVPNSubscription(subscriptionQuery, 1)).unsafeRunSync()
// Connecting to VPN for john_doe ...
// Connected to VPN for john_doe!
// emitting for user john_doe
// Disconnecting from VPN after 566ms for john_doe ...
// res7: String = "duration was 578ms"

def fastQuery = """
  subscription {
    me
  }
"""

bench(runVPNSubscription(fastQuery, 1)).unsafeRunSync()
// res8: String = "duration was 4ms"
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
// a new token was issued: token-john_doe-0
// Connecting to VPN for token-john_doe-0 ...
// Connected to VPN for token-john_doe-0!
// emitting for user token-john_doe-0
// a new token was issued: token-john_doe-1
// Connecting to VPN for token-john_doe-1 ...
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// emitting for user token-john_doe-0
// Connected to VPN for token-john_doe-1!
// emitting for user token-john_doe-0
// emitting for user token-john_doe-1
// emitting for user token-john_doe-0
// Disconnecting from VPN after 1164ms for token-john_doe-0 ...
// a new token was issued: token-john_doe-2
// Connecting to VPN for token-john_doe-2 ...
// emitting for user token-john_doe-1
// Connection for token-john_doe-2 cancelled while connecting!
// Disconnecting from VPN after 613ms for token-john_doe-1 ...
// res9: List[io.circe.JsonObject] = List(
//   object[data -> {
//   "vpn" : {
//     "data" : {
//       "serverId" : "secret_server",
//       "connectedUser" : "token-john_doe-0",
//       "hash" : "hash of 10",
//       "content" : "content 10"
//     },
//     "metadata" : {
//       "subscriptionTimestamp" : "now!",
//       "createdAge" : 42,
//       "name" : "super_secret_file"
//     }
//   }
// }],
//   object[data -> {
//   "vpn" : {
//     "data" : {
//       "serverId" : "secret_server",
//       "connectedUser" : "token-john_doe-1",
//       "hash" : "hash of 0",
//       "content" : "content 0"
//     },
//     "metadata" : {
//       "subscriptionTimestamp" : "now!",
//       "createdAge" : 42,
//       "name" : "super_secret_file"
//     }
//   }
// }],
//   object[data -> {
//   "vpn" : {
//     "data" : {
//       "serverId" : "secret_server",
//       "connectedUser" : "token-john_doe-1",
//       "hash" : "hash of 1",
//       "content" : "content 1"
//     },
//     "metadata" : {
//       "subscriptionTimestamp" : "now!",
//       "createdAge" : 42,
//       "name" : "super_secret_file"
//     }
//   }
// }]
// )
```

## Resolver composition
Resolvers can also be composed via the `CompositionResolver`.
This means that the output of one resolver can be used as the input of another resolver.
This also means that `Stream`, `Batch` and `Effect` resolvers can be combined in any order.

For instance, one can efficiently fetch some list of ids, then subscribe to the data of each id and for all changed ids, efficiently fetch the changed data.
This can be achieved by composing `Batch`, `Stream` and then `Batch` again.
