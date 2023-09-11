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
import gql.dsl._
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

  // Set of columns that uniquely identify a row
  // The dsl will add the table alias to all column related methods
  def tableKeys = keys(void"id" -> int4)

  // The SkunkTable trait gives some convinience methods for declaring columns
  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (addressCol, address) = sel("address", text)
}
// We get some methods if show how given an alias we can get a table
val homeTable = skunkTable(HomeTable)
```

We will also need to declare the other two tables, this time with less comments.
```scala mdoc:silent
case class PersonTable(alias: String) extends SkunkTable {
  def table = void"person"

  def tableKeys = keys(void"id" -> int4)

  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)
}
val personTable = skunkTable(PersonTable)

case class PetTable(alias: String) extends SkunkTable {
  def table = void"pet"

  def tableKeys = keys(void"id" -> int4)

  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)
  val (ownerCol, owner) = sel("owner", int4)
}
val petTable = skunkTable(PetTable)
```

Since `Home` and `Person` have a many to many relationship, we will have to go through another table table to get the relationship.
```scala mdoc:silent
case class HomePersonTable(alias: String) extends SkunkTable {
  def table = void"home_person"

  def tableKeys = keys(
    void"home_id" -> int4,
    void"person_id" -> int4
  )

  val (homeCol, home) = sel("home_id", int4)
  val (personCol, person) = sel("person_id", int4)
}
val homePersonTable = skunkTable(HomePersonTable)
```

Now we can start declaring our graphql schema.
```scala mdoc:silent
implicit lazy val pet = tpe[IO, QueryResult[PetTable]](
  "PetTable",
  "name" -> query(_.name), // query is a method that compiles to a projection in the query language (sql)
  "age" -> query(_.age)
)

implicit lazy val person = tpe[IO, QueryResult[PersonTable]](
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

implicit lazy val home = tpe[IO, QueryResult[HomeTable]](
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
implicit val trace = NoopTrace[IO]()

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
  def tableKeys = keys(void"id" -> int4)

  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)
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

### Runtime semantics
SQL is a language that works on flat arrays of rows, but for it to map well to graphql some work must be performed.
Most use-cases should be covered by simply invoking the `join` method with the proper multiplicity parameter, so this section is more of a technical reference.

When you AST is inspected to build the query, the recursive AST walk also composes a big reassociation function that can translate a list of flat query results into the proper nested structure.
This composed function also tracks the visited columns and their decoders.

The query algebra has a special operation that lets the caller modify the state however they wish.
The dsl uses this state modification for various tasks, such as providing a convinient `join` method that both joins a table and performs the proper reassociation of results.
Consider the following example that joins a table more explicitly.
```scala mdoc
val q1 = for {
  ht <- homeTable.simpleJoin(_ => void"true")
  _ <- reassociate[List](ht.tableKeys._2, ht.tableKeys._1.toList: _*)
  // some other reassociation criteria
  _ <- reassociate[Option](int4, void"42")
} yield ht

// we can even perform them before the join
val q2 = reassociate[Option](text, void"'john doe'").flatMap(_ => q1)

// we can also change the result structure after reassociation
q2.mapK[List](new (Lambda[X => Option[List[Option[X]]]] ~> List) {
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

### Implementing your own integration
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

### Adding arguments
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

### Declaring complex subqueries
Sometimes your tables must have complex filtering, limiting, ordering and so on.
The most obvious way to declare such parameters is simply to use a subquery.
```scala mdoc:silent
case class ParameterizedPersonTable(alias: String, table: AppliedFragment) extends SkunkTable {
  def tableKeys = keys(void"id" -> int4)

  val (idCol, id) = sel("id", int4)
  val (nameCol, name) = sel("name", text)
  val (ageCol, age) = sel("age", int4)
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

### Using relational without tables
There is no restriction on how you can implement a table, so you can choose your own strategy.
For instance say we just wanted to declare everything up-front and select fields ad-hoc.
```scala mdoc:silent
case class AdHocTable(
  alias: String, 
  table: AppliedFragment,
  tableKeys: (Chain[AppliedFragment], Decoder[?]),
) extends SkunkTable

tpe[IO, QueryResult[HomeTable]](
  "HomeTable",
  "people" -> cont(arg[List[Int]]("ids")) { (home, ids) =>
    for {
      hp <- skunkTable(alias => 
          AdHocTable(alias, void"home_person", Chain(sql"#${alias}.home_id", sql"#${alias}.person_id").map(_.apply(Void)) -> int4 ~ int4)
        ).join[List](hp => sql"${home.idCol} = ${hp.aliased(sql"home_id")}")
      p <- personTable.join(p => sql"${hp.aliased(sql".person_id")} = ${p.idCol} and ${p.idCol} in (${int4.list(ids)})".apply(ids))
    } yield p
  }
)
```
Since there is no dsl for this, constructing the query is a bit gruesome.
Consider if a dsl is possible for your formulation.

### Running transactions
Lazy session
#### Subscriptions
evalMap reset

### Handling N+1