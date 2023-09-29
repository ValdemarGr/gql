---
title: Relational
---
:::caution
This integration is fairly new and sofisticated so it can be subject to change.
:::
gql also comes with an optional integration for relational databases.

The relational integration is library agnostic and is based on query fragments that can be composed into a full query.

The relational module ships with two implementations, one for `skunk` and another for `doobie`.
They can be found in the [modules](../../overview/modules) section.
:::tip
Integrating a new library requires very little code.
The skunk integration only spans 18 lines of code.
:::

## Skunk example
For this example we will use `skunk`.
We will start off with some imports.
```scala mdoc
import skunk._
import skunk.codec.all._
import skunk.implicits._
import gql.ast._
import gql.dsl.all._
import gql.relational._
import gql.relational.skunk.dsl._
import cats._
import cats.data._
import cats.arrow._
import cats.effect._
import cats.implicits._
```

Before we start declaring fragments, we need to define our domain.
```scala mdoc
final case class Home(name: String, address: String)
// many homes belong to many people
final case class Person(name: String, age: Int)
// a pet has one owner
final case class Pet(name: String, age: Int, owner: Int)
```

The realtional module also ships with a dsl that makes declaration use conscise.
We will start off just declaring the home table.
```scala mdoc:silent
case class HomeTable(
  // When a table is queried it must have an alias
  alias: String
) extends SkunkTable {
  // Note that we use only skunk tools to declare the contents of this structure

  // We can declare how this table is referenced in sql (or some other query language)
  def table = void"home"

  // The SkunkTable trait gives some convinience methods for declaring columns
  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (addressCol, address) = sel("address", text)

  // The projection that uniquely identifies a row in the table
  def tableKey = id
}
// We get some methods if show how given an alias we can get a table
val homeTable = skunkTable(HomeTable)
```

We will also need to declare the other two tables, this time with less comments.
```scala mdoc:silent
case class PersonTable(alias: String) extends SkunkTable {
  def table = void"person"

  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)

  def tableKey = id
}
val personTable = skunkTable(PersonTable)

case class PetTable(alias: String) extends SkunkTable {
  def table = void"pet"

  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)
  val (ownerCol, owner) = sel("owner", int4)

  def tableKey = id
}
val petTable = skunkTable(PetTable)
```

Since `Home` and `Person` have a many to many relationship, we will have to go through another table table to get the relationship.
```scala mdoc:silent
case class HomePersonTable(alias: String) extends SkunkTable {
  def table = void"home_person"

  val (homeCol, home) = sel("home_id", int4)
  val (personCol, person) = sel("person_id", int4)

  def tableKey = (home, person).tupled
}
val homePersonTable = skunkTable(HomePersonTable)
```

Now we can start declaring our graphql schema.
```scala mdoc:silent
implicit lazy val pet: Type[IO, QueryResult[PetTable]] = 
  tpe[IO, QueryResult[PetTable]](
    "PetTable",
    "name" -> query(_.name), // query is a method that compiles to a projection in the query language (sql)
    "age" -> query(_.age)
  )

implicit lazy val person: Type[IO, QueryResult[PersonTable]] = 
  tpe[IO, QueryResult[PersonTable]](
    "PersonTable",
    "name" -> query(_.name),
    "age" -> query(_.age),
    "pets" -> cont{ person => // cont is a continuation that will create a new table from the current one
      // The join method takes a type parameter that declares the multiplicity of the join
      // If no type parameter is given, the join is assumed to be one to one
      petTable.join[List]{ pet =>
        // Given an instance of the pet table, we can declare a join predicate
        sql"${pet.ownerCol} = ${person.idCol}"
      }
    }
  )

implicit lazy val home: Type[IO, QueryResult[HomeTable]] = 
  tpe[IO, QueryResult[HomeTable]](
    "HomeTable",
    "name" -> query(_.name),
    "address" -> query(_.address),
    "caption" -> query(h => (h.name, h.address).mapN(_ + " at " + _)), // projections form an applicative
    "people" -> cont{ home =>
      // Tables can be flatmapped together
      for {
        hp <- homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}")
        p <- personTable.join(p => sql"${hp.personCol} = ${p.idCol}")
      } yield p
    }
  )
```
Now we are done declaring our schema.

Before querying it we will need our database up and running.
```scala mdoc:silent
import cats.effect.unsafe.implicits.global
import natchez.noop._ // needed for skunk connection
implicit val trace: natchez.Trace[IO] = NoopTrace[IO]()

def connection = Session.single[IO](
  host = "127.0.0.1",
  port = 5432,
  user = "postgres",
  database = "postgres"
)
```


<details>
  <summary>We will also need to create our tables and insert some data.</summary>

```scala mdoc
connection.use{ ses =>
  val queries = List(
    sql"drop table if exists pet",
    sql"drop table if exists home_person",
    sql"drop table if exists person",
    sql"drop table if exists home",
    sql"""create table home_person (
      home_id int not null,
      person_id int not null
    )""",
    sql"""create table pet (
      id int4 primary key,
      name text not null,
      age int not null,
      owner int not null
    )""",
    sql"""create table person (
      id int4 primary key,
      name text not null,
      age int not null
    )""",
    sql"""create table home (
      id int4 primary key,
      name text not null,
      address text not null
    )""",
    sql"""insert into home (id, name, address) values (1, 'Doe Home', '123 Main St')""",
    sql"""insert into person (id, name, age) values (1, 'John Doe', 42)""",
    sql"""insert into person (id, name, age) values (2, 'Jane Doe', 40)""",
    sql"""insert into home_person (home_id, person_id) values (1, 1)""", 
    sql"""insert into home_person (home_id, person_id) values (1, 2)""",
    sql"""insert into pet (id, name, age, owner) values (1, 'Fluffy', 2, 1)""",
  )

  queries.traverse(x => ses.execute(x.command))
}.unsafeRunSync()
```

</details>

```scala mdoc

def schema = gql.Schema.query(
  tpe[IO, Unit](
    "Query",
    "homes" -> runFieldSingle(connection) { (_: Unit) => 
      homeTable.join[List](_ => sql"true")
    }
  )
)

def q = """
query {
  homes {
    name
    address
    caption
    people {
      name
      age
      pets {
        name
        age
      }
    }
  }
}
"""

import io.circe.syntax._
import gql.{Compiler, Application}
schema
  .map(Compiler[IO].compile(_, q))
  .flatMap { case Right(Application.Query(run)) => run.map(_.handleErrors{e => println(e.getMessage()); ""}.asJson.spaces2) }
  .unsafeRunSync()
```
And thats it!

Just for fun, we check out the generated sql.
```scala mdoc:nest
import gql.relational.skunk._
implicit def logQueries[F[_]: MonadCancelThrow]: SkunkIntegration.Queryable[F] = 
  new SkunkIntegration.Queryable[F] {
    def apply[A](
      query: AppliedFragment,
      decoder: Decoder[A], 
      connection: SkunkIntegration.Connection[F]
    ): F[List[A]] = {
      println(query.fragment.sql)
      SkunkIntegration.skunkQueryable[F].apply(query, decoder, connection)
    }
}

def schema = gql.Schema.query(
  tpe[IO, Unit](
    "Query",
    "homes" -> runFieldSingle(connection) { (_: Unit) => 
      homeTable.join[List](_ => sql"true")
    }
  )
)

schema
  .map(Compiler[IO].compile(_, q))
  .flatMap { case Right(Application.Query(run)) => run.void }
  .unsafeRunSync()
```

### Simplifying relationships
The join between `home` and `person` can be a bit daunting, since you have to keep track of multiplicity yourself.
Instead we can use the database to handle some of the multiplicity for us by generalizing the person table.
```scala mdoc:silent
case class SharedPersonTable(alias: String, table: AppliedFragment) extends SkunkTable {
  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)

  def tableKey = id
}

val sharedPersonTable = skunkTable(SharedPersonTable(_, void"person"))

val homePersonQuery = void"(select * from home_person inner join person on home_person.person_id = person.id)"
val sharedHomePersonTable = skunkTable(SharedPersonTable(_, homePersonQuery))

// And now using our subquery we can simplify the join.
implicit lazy val person: Type[IO, QueryResult[SharedPersonTable]] = ???

tpe[IO, QueryResult[HomeTable]](
  "HomeTable",
  "name" -> query(_.name),
  "address" -> query(_.address),
  "caption" -> query(h => (h.name, h.address).mapN(_ + " at " + _)), // projections form an applicative
  "people" -> cont{ h => 
    sharedHomePersonTable.join[List](hp => sql"${h.idCol} = ${hp.aliased(sql"home_id")}")
  }
)
```

## Runtime semantics
:::info
This section is a technical reference, and not necessary to use the library.
:::
Data emitted by SQL is not hierarchical, but instead flat; for it to map well to graphql, which is hierarchical some work must be performed.
Most use-cases are covered by simply invoking the `join` method with the proper multiplicity parameter.

When your AST is inspected to build a query, a recursive AST walk composes a big reassociation function that can translate flat query results into the proper hierarchical structure.
This composed function also tracks the visited columns and their decoders.

The query algebra has a special operation that lets the caller modify the state however they wish.
The dsl uses this state modification for various tasks, such as providing a convinient `join` method that both joins a table and performs the proper reassociation of results.
Consider the following example that joins a table more explicitly.
```scala mdoc
val q1 = for {
  ht <- homeTable.simpleJoin(_ => void"true")
  _ <- reassociate[List](ht.tableKey)
  // some other reassociation criteria
  _ <- reassociate[Option](select(int4, void"42"))
} yield ht

// we can perform reassociation before out join also
val q2 = reassociate[Option](select(text, void"'john doe'")).flatMap(_ => q1)

// we can also change the result structure after reassociation
q2.mapK[List](new (λ[X => Option[List[Option[X]]]] ~> List) {
  def apply[A](fa: Option[List[Option[A]]]): List[A] = fa.toList.flatten.flatMap(_.toList)
})
```

Accessing the lowlevel state also lets the user perform other tasks such as unique id (new alias) generation.
```scala mdoc
for {
  alias1 <- newAlias
  alias2 <- newAlias
} yield ()
```

## Implementing your own integration
The entire dsl and query compiler is available if you implement a couple of methods.

Here is the full skunk integration.
```scala mdoc
import _root_.{skunk => sk}
object MyIntegration extends QueryAlgebra {
  // What is a fragment
  type Frag = sk.AppliedFragment
  // How do we make a fragment for a string
  def stringToFrag(s: String): Frag = sql"#${s}".apply(Void)
  // Combine and create empty fragments
  implicit def appliedFragmentMonoid: Monoid[Frag] = sk.AppliedFragment.MonoidAppFragment
  // How do we decode and encode values
  type Encoder[A] = sk.Encoder[A]
  type Decoder[A] = sk.Decoder[A]
  // How can we combine decoders
  implicit def applicativeForDecoder: Applicative[Decoder] = Decoder.ApplicativeDecoder
  // How do we make an optional decoder
  def optDecoder[A](d: Decoder[A]): Decoder[Option[A]] = d.opt
  // What is needed to perform a query
  type Connection[F[_]] = Resource[F, Session[F]]
  // Given a connection, how do we use it
  implicit def skunkQueryable[F[_]: MonadCancelThrow]: Queryable[F] = new Queryable[F] {
    def apply[A](query: AppliedFragment, decoder: Decoder[A], connection: Connection[F]): F[List[A]] =
      connection.use(_.execute(query.fragment.query(decoder))(query.argument))
  }
}
```

The dsl can be instantiated for any query algebra.
```scala mdoc:nest
object myDsl extends QueryDsl(MyIntegration)
```
you can also add integration specific methods to your dsl.
```scala mdoc:nest
object myDsl extends QueryDsl(MyIntegration) {
  def someOperationSpecificToMyIntegration = ???
}
```

## Adding arguments
All field combinators allow arguments to be provided naturally, regardless of where the field is in the query.
```scala mdoc:silent
implicit lazy val pt: Type[IO, QueryResult[PersonTable]] = ???

tpe[IO, QueryResult[HomeTable]](
  "HomeTable",
  "people" -> cont(arg[List[Int]]("ids")) { (home, ids) =>
    for {
      hp <- homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}")
      p <- personTable.join(p => sql"${hp.personCol} = ${p.idCol} and ${p.idCol} in (${int4.list(ids)})".apply(ids))
    } yield p
  }
)
```

## Sum types
Sum types can naturally be declared also.

<details>
  <summary>Lets set up some tables for sum types.</summary>

```scala mdoc
connection.use{ ses =>
  val queries = List(
    sql"drop table if exists owner",
    sql"drop table if exists dog",
    sql"drop table if exists cat",
    sql"""create table owner (
      id int4 primary key
    )""",
    sql"""create table dog (
      id int4 primary key,
      owner_id int4 not null,
      name text not null,
      age int not null
    )""",
    sql"""create table cat (
      id int4 primary key,
      owner_id int4 not null,
      name text not null,
      age int not null
    )""",
    sql"""insert into owner (id) values (1)""",
    sql"""insert into owner (id) values (2)""",
    sql"""insert into dog (id, owner_id, name, age) values (1, 1, 'Dog', 42)""",
    sql"""insert into cat (id, owner_id, name, age) values (2, 2, 'Cat', 22)""",
  )

  queries.traverse(x => ses.execute(x.command))
}.unsafeRunSync()
```

</details>

And now we can run it.
```scala mdoc:nest
sealed trait Animal { 
  def name: String
}
case class Dog(owner: String, name: String, age: Int) extends Animal
case class Cat(owner: String, name: String, age: Int) extends Animal

trait OwnerTable extends SkunkTable {
  def table = void"owner"
  val (idCol, id) = sel("id", int4)
  def tableKey = id
}
case class OwnerTableUnion(alias: String) extends OwnerTable
case class OwnerTableInterface(alias: String) extends OwnerTable
val ownerTableUnion = skunkTable(OwnerTableUnion)
val ownerTableInterface = skunkTable(OwnerTableInterface)

case class DogTable(alias: String) extends SkunkTable {
  def table = void"dog"

  val (idCol, id) = sel("id", int4)
  val (ownerCol, owner) = sel("owner_id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)

  def tableKey = id
}
val dogTable = skunkTable(DogTable)

case class CatTable(alias: String) extends SkunkTable {
  def table = void"cat"

  val (idCol, id) = sel("id", int4)
  val (ownerCol, owner) = sel("owner_id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)

  def tableKey = id
}
val catTable = skunkTable(CatTable)

implicit lazy val animalInterface = interface[IO, QueryResult[OwnerTableInterface]](
  "AnimalInterface",
  "owner" -> abst[IO, String]
)

implicit lazy val cat = tpe[IO, QueryResult[CatTable]](
  "Cat",
  "owner" -> query(_.owner),
  "name" -> query(_.name),
  "age" -> query(_.age)
).contImplements[OwnerTableInterface]{ owner => 
  catTable.join[Option](cat => sql"${owner.idCol} = ${cat.ownerCol}")
}

implicit lazy val dog = tpe[IO, QueryResult[DogTable]](
  "Dog",
  "owner" -> query(_.owner),
  "name" -> query(_.name),
  "age" -> query(_.age)
).contImplements[OwnerTableInterface]{ owner => 
  dogTable.join[Option](dog => sql"${owner.idCol} = ${dog.ownerCol}")
}

// we use the builder to create a union type
implicit lazy val animal = relBuilder[IO, OwnerTableUnion] { b =>
  b
    .union("Animal")
    .contVariant(owner => dogTable.join[Option](dog => sql"${owner.idCol} = ${dog.ownerCol}"))
    .contVariant(owner => catTable.join[Option](cat => sql"${owner.idCol} = ${cat.ownerCol}"))
}

def schema = gql.Schema.query(
  tpe[IO, Unit](
    "Query",
    "animals" -> runFieldSingle(connection) { (_: Unit) =>
      ownerTableUnion.join[List](_ => sql"true")
    },
    "animalInterfaces" -> runFieldSingle(connection) { (_: Unit) =>
      ownerTableInterface.join[List](_ => sql"true")
    }
  )
)

def animalQuery = """
  query {
    animals {
      __typename
      ... on Dog {
        owner
        name
        age
      }
      ... on Cat {
        owner
        name
        age
      }
    }
    animalInterfaces {
      __typename
      ... on Dog {
        owner
        name
        age
      }
      ... on Cat {
        owner
        name
        age
      }
    }
  }
"""

schema
  .map(Compiler[IO].compile(_, animalQuery))
  .flatMap { case Right(Application.Query(run)) => run.map(_.handleErrors{e => println(e.getMessage()); ""}.asJson.spaces2) }
  .unsafeRunSync()
```


## Declaring complex subqueries
Sometimes your tables must have complex filtering, limiting, ordering and so on.
The most obvious way to declare such parameters is simply to use a subquery.
```scala mdoc:silent
case class ParameterizedPersonTable(alias: String, table: AppliedFragment) extends SkunkTable {
  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)
  
  def tableKey = id
}
def parameterizedPersonTable(
  limitOffset: Option[(Int, Int)],
  order: Option[AppliedFragment],
  filter: Option[AppliedFragment]
) = skunkTable{ alias => 
  val filt = filter.foldMap(f => sql"where ${f.fragment}".apply(f.argument))
  val ord = order.foldMap(f => sql"order by ${f.fragment}".apply(f.argument))
  val lim = 
    limitOffset.foldMap{ case (limit, offset) => sql"limit ${int4} offset ${int4}".apply((limit, offset))}
  ParameterizedPersonTable(
    alias,
    sql"""|(
          |  select *
          |  from person
          |  ${filt.fragment}
          |  ${ord.fragment}
          |  ${lim.fragment}
          |)""".stripMargin.apply((filt.argument, ord.argument, lim.argument))
  )
}
```
And now we can use our new table.
```scala mdoc:silent
implicit lazy val ppt: Type[IO, QueryResult[ParameterizedPersonTable]] = ???

val personQueryArgs = (
  arg[Option[Int]]("limit"),
  arg[Option[Int]]("offset"),
  arg[Option[Boolean]]("order"),
  arg[Option[Int]]("ageFilter")
).tupled
tpe[IO, QueryResult[HomeTable]](
  "HomeTable",
  "people" -> cont(personQueryArgs) { case (home, (lim, off, ord, af)) =>
    for {
      hp <- homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}")
      p <- parameterizedPersonTable(
        limitOffset = (lim, off).tupled,
        order = ord.map{
          case true => void"age desc"
          case false => void"age asc"
        },
        filter = af.map(age => sql"age > ${int4}".apply(age))
      ).join(p => sql"${hp.personCol} = ${p.idCol}")
    } yield p
  }
)
```

## Using relational without tables
There is no restriction on how you can implement a table, so you can choose your own strategy.
For instance say we just wanted to declare everything up-front and select fields ad-hoc.
```scala mdoc:silent
import gql.relational.skunk.SkunkIntegration.Query.Select

case class AdHocTable(
  alias: String, 
  table: AppliedFragment,
  tableKey: Select[?],
) extends SkunkTable

tpe[IO, QueryResult[HomeTable]](
  "HomeTable",
  "people" -> cont(arg[List[Int]]("ids")) { (home, ids) =>
    for {
      hp <- skunkTable(alias => 
          AdHocTable(
            alias, 
            sql"#${alias}.home_person".apply(Void), 
            select(
              int4 ~ int4,
              sql"#${alias}.home_id".apply(Void), 
              sql"#${alias}.person_id".apply(Void)
            )
          )
        ).join[List](hp => sql"${home.idCol} = ${hp.aliased(sql"home_id")}")
      p <- personTable.join(p => sql"${hp.aliased(sql".person_id")} = ${p.idCol} and ${p.idCol} in (${int4.list(ids)})".apply(ids))
    } yield p
  }
)
```
Since there is no dsl for this, constructing the query is a bit gruesome.
Consider if a dsl is possible for your formulation.

## Running transactions
Most usecases involve running all queries in a transaction, but none of the examples so far have introduces this.
The implementation of transactions depends on the database library, but many implementations share common properties.

If your database library supports opening transactions as a resource then the you can lazily open a transaction.
Here is an example using skunk.
```scala mdoc:silent:nest
trait SessionContext {
  def getSession: Resource[IO, Session[IO]]
}

object SessionContext {
  def fromIOLocal(iol: IOLocal[Option[Resource[IO, Session[IO]]]]) = new SessionContext {
    def getSession = Resource.eval(iol.get).flatMap{
      case None => Resource.eval(IO.raiseError(new Exception("No session in context")))
      case Some(sc) => sc
    }
  }
}

def myConnection: Resource[IO, Session[IO]] = Session.single[IO](
  host = "127.0.0.1",
  port = 5432,
  user = "postgres",
  database = "postgres"
)

// The outer resource manages the lifecycle of the connection
// The inner resource leases the connection, if the inner resource is not closed, the outer waits
def lazyConnection: Resource[IO, LazyResource[IO, Session[IO]]] = 
  gql.relational.LazyResource.fromResource(myConnection)

// We define our schema as requiring a connection
def myQuery(ctx: SessionContext): Type[IO, Unit] = {
  implicit lazy val homeTableTpe: Out[IO, QueryResult[HomeTable]] = ???
  tpe[IO, Unit](
    "Query",
    "homes" -> runFieldSingle(ctx.getSession) { (_: Unit) => 
      homeTable.join[List](_ => sql"true")
    }
  )
}

def runQuery: IO[String => Compiler.Outcome[IO]] = 
  gql.Statistics[IO].flatMap{ stats => 
    IOLocal[Option[Resource[IO, Session[IO]]]](None).map{ loc =>
      val sc = SessionContext.fromIOLocal(loc)

      val schema = gql.Schema.query(stats)(myQuery(sc))

      val setResource = lazyConnection.evalMap(x => loc.set(Some(x.get)))

      (query: String) => 
        Compiler[IO]
          .compile(schema, q)
          .map{
            case gql.Application.Query(fa) => gql.Application.Query(setResource.surround(fa))
            case gql.Application.Mutation(fa) => gql.Application.Mutation(setResource.surround(fa))
            // Subscription is a bit more complex since we would like to close the transaction on every event
            case gql.Application.Subscription(fa) => 
              gql.Application.Subscription{
                fs2.Stream.resource(lazyConnection).flatMap{ x =>
                  fs2.Stream.exec(loc.set(Some(x.get))) ++
                    fa.evalTap(_ => x.forceClose)
                }
              }
          }
    }
  }
```

<details>
  <summary>You can also use MTL for passing the transaction around</summary>

```scala mdoc:silent:nest
import cats.mtl._

def myConnection: Resource[IO, Session[IO]] = Session.single[IO](
  host = "127.0.0.1",
  port = 5432,
  user = "postgres",
  database = "postgres"
)

// The outer resource manages the lifecycle of the connection
// The inner resource leases the connection, if the inner resource is not closed, the outer waits
def lazyConnection: Resource[IO, LazyResource[IO, Session[IO]]] = 
  gql.relational.LazyResource.fromResource(myConnection)

val liftK = Kleisli.liftK[IO, Resource[IO, Session[IO]]]

type GetConn[F[_]] = Ask[F, Resource[F, Session[F]]]

def makeConn[F[_]](conn: GetConn[F]): Resource[F, Session[F]] = 
  Resource.eval(conn.ask[Resource[F, Session[F]]]).flatten

// We define our schema as requiring a connection
def myQuery[F[_]: Async](conn: GetConn[F]): Type[F, Unit] = {
  implicit lazy val homeTableTpe: Type[F, QueryResult[HomeTable]] = ???
  tpe[F, Unit](
    "Query",
    "homes" -> runFieldSingle(makeConn(conn)) { (_: Unit) => 
      homeTable.join[List](_ => sql"true")
    }
  )
}

implicit def functorForAsk[F[_]]: Functor[Ask[F, *]] = ???
def kleisliAsk[F[_]: Applicative, A] = Ask[Kleisli[F, A, *], A]

def runQuery: IO[String => Compiler.Outcome[IO]] = 
  gql.Statistics[IO].map{ stats => 
    type G[A] = Kleisli[IO, Resource[IO, Session[IO]], A]

    val liftK = Kleisli.liftK[IO, Resource[IO, Session[IO]]]

    val ask: Ask[G, Resource[G, Session[G]]] = 
      kleisliAsk[IO, Resource[IO, Session[IO]]].map(_.mapK(liftK).map(_.mapK(liftK)))

    val schema = gql.Schema.query(stats.mapK(liftK))(myQuery[G](ask))

    val oneshot = lazyConnection.map(_.get.flatTap(_.transaction))

    (query: String) => 
      Compiler[G]
        .compile(schema, q)
        .map{ 
          case gql.Application.Query(fa) => gql.Application.Query(oneshot.useKleisli(fa))
          case gql.Application.Mutation(fa) => gql.Application.Mutation(oneshot.useKleisli(fa))
          // Subscription is a bit more complex since we would like to close the transaction on every event
          case gql.Application.Subscription(fa) => 
            gql.Application.Subscription{
              fs2.Stream.resource(lazyConnection).flatMap{ lc =>
                fa
                  .translate(Kleisli.applyK[IO, Resource[IO, Session[IO]]](lc.get.flatTap(_.transaction)))
                  .evalTap(_ => lc.forceClose)
              }
            }
        }
  }
```

</details>


## Handling N+1
The relational module can handle N+1 queries and queries that can cause cartesian products.
To solve N+1, the user must use the `runField` method instead of the `runFieldSingle`.
The `runField` method takes a list of inputs `I` and produces `Query[G, (Select[I], B)]`, such that query results can be reassociated with the inputs.
```scala mdoc
def myBatchedHomeQuery(conn: Resource[IO, Session[IO]]) = {
  case class MyDatatype(homeId: Int)

  tpe[IO, MyDatatype](
    "MyDatatype",
    "home" -> runField[IO, List, MyDatatype, HomeTable](conn) { xs => 
      val lst = xs.toList.map(_.homeId)
      for {
        ht <- homeTable.join[List](ht => sql"${ht.idCol} in (${int4.list(lst)})".apply(lst))
      } yield (ht.id.fmap(MyDatatype), ht)
    }
  )
}
```

To solve the query multiplicity explosions you can use the `contBoundary` which works almost like `cont`, except the query will be split up into two queries.

The `contBoundary` function takes two interesting parameters.
The first parameter will be a projection of the current query, decoded into `B`.
The second parameter turns this `B` into another query, which will be the root of the new query.
```scala mdoc
def boundaryQuery(conn: Resource[IO, Session[IO]]) = {
  case class MyDatatype(homeId: Int)

  relBuilder[IO, HomeTable]{ rb =>
    rb.tpe(
      "HomeTable",
      "people" -> rb.contBoundary(conn){ home =>
        homePersonTable.join[List](hp => sql"${home.idCol} = ${hp.homeCol}").map(_.person)
      }{ (xs: NonEmptyList[Int]) =>
        val lst = xs.toList
        personTable.join(p => sql"${p.idCol} in (${int4.list(lst)})".apply(lst)).map(p => p.id -> p)
      }
    )
  }
}
```
:::info
The `contBoundary` is only available in when using the `relBuilder`, since type inference does not work very well.

Inference troubles with `runField` can also be alleviated by using the `relBuilder`.
:::

