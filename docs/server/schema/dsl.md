---
title: The DSL
---
The DSL consists of a series of smart constructors for the ast nodes of gql.
The source code for the DSL is very easy to follow and as such, the best documentation is the source code itself :-).

Lets start off with some imports.
```scala mdoc
import cats.data._
import cats.effect._
import cats.implicits._
import gql.dsl._
import gql.ast._
import gql.resolver._
```

## Fields
The simplest form of field construction comes from the `build.from` smart constructor.
It simply lifts a resolver into a field, given that a gql output type exists for the resolver output.
```scala mdoc
def r: Resolver[IO, Int, String] = Resolver.lift(i => i.toString())

val f: Field[IO, Int, String] = build.from(r)
```

Sometimes type inference cannot find the proper type for a field:
```scala mdoc:fail
build.from(Resolver.liftF(i => IO(i.toString())))
```
The type parameters for `build` are partially applied, such that when type inference isn't enough, types can be supplied explicitly.
```scala mdoc:silent
build[IO, Int].from(Resolver.liftF(i => IO(i.toString())))

build.from(Resolver.liftF((i: Int) => IO(i.toString())))
```

For most non-trivial fields, there is an even more concise syntax.
Invoking the `apply` method of `build`, takes a higher order function that goes from the identity resolver to some output:
```scala mdoc:silent
build[IO, Int](_.map(i => i * 2).evalMap(i => IO(i))): Field[IO, Int, Int]
```

### Builders
Complex structures may require many special resolver compositions.
The dsl also introduces a somethink akin to a builder pattern.
The `build` function from the previous section, in fact, creates a builder that has many more options than just `from` and `apply`.
```scala mdoc:silent
val b: FieldBuilder[IO, Int] = build[IO, Int]
```
Often a builder is only relevant within a scope, thus one can end up having many unused builders in scope.
The `builder` makes such code more concise:
```scala mdoc:silent
builder[IO, Int]{ (fb: FieldBuilder[IO, Int]) =>
  fb
}
```
The builder dsl contains most of the field related constructors:
```scala mdoc:silent
builder[IO, Int]{ fb =>
  fb.tpe(
    "Query",
    "answer" -> lift(i => i * 0 + 42),
    "pong" -> fb(_.map(_ => "pong"))
  ): Type[IO, Int]
  
  fb.fields(
    "answer" -> fb.lift(i => i * 0 + 42),
    "ping" -> fb.from(Resolver.lift(_ => "pong"))
  )
}
```

### Value resolution
Wrapping every field in a `build` smart constructor and then defining the resolver seperately is a bit verbose.
There are smart constructors for two common variants of field resolvers that lift the resolver function directly to a `Field`.

We must decide if the field is pure or effectful:
:::note
The effect constructor is named `eff` to avoid collisions with cats-effect.
:::
```scala mdoc:silent
final case class Person(
  name: String
)

tpe[IO, Person](
  "Person",
  "name" -> lift(_.name),
  "nameEffect" -> eff(x => IO(x.name))
)
```

The `lift` and `eff` constructors can also also be supplied with arguments:
```scala mdoc:silent
def familyName = arg[String]("familyName")

tpe[IO, Person](
  "Person",
  "name" -> lift(familyName)(_ + _.name),
  "nameEffect" -> eff(familyName)((f, p) => IO(p.name + f))
)
```

## Unification instances
`Union`s and `Interface`s are abstract types that have implementations.

`Union` declares it's implementations up-front, like a `sealed trait`.
However, `Interface` implementations are declared on the types that implement the interface, like a `trait` or an `abstract class`.

Before continuing, lets setup the environment.
```scala mdoc:silent
trait Vehicle { 
  def name: String
}
final case class Car(name: String) extends Vehicle
final case class Boat(name: String) extends Vehicle
final case class Truck(name: String) extends Vehicle

```

For the `Union`, variants can be declared using the `variant` function, which takes a `PartialFunction` from the unifying type to the implementation.
```scala mdoc:silent
implicit def car: Type[IO, Car] = ???
implicit def boat: Type[IO, Boat] = ???
implicit def truck: Type[IO, Truck] = ???

union[IO, Vehicle]("Vehicle")
  .variant[Car] { case c: Car => c }
  .variant[Boat] { case b: Boat => b }
  .variant[Truck] { case t: Truck => t }
```
A shorthand function exists, if the type of the variant is a subtype of the unifying type.
```scala mdoc:silent
union[IO, Vehicle]("Vehicle")
  .subtype[Car] 
  .subtype[Boat] 
  .subtype[Truck] 
```

For an `Interface` the same dsl exists, but is placed on the types that can implement the interface (a `Type` or another `Interface`).
```scala mdoc:silent
implicit lazy val vehicle: Interface[IO, Vehicle] = interface[IO, Vehicle](
  "Vehicle",
  "name" -> abst[IO, String]
)

tpe[IO, Car]("Car", "name" -> lift(_.name))
  .implements[Vehicle]{ case c: Car => c }
  
tpe[IO, Boat]("Boat", "name" -> lift(_.name))
  .subtypeOf[Vehicle]
  
trait OtherVehicle extends Vehicle {
  def weight: Int
}

interface[IO, OtherVehicle](
  "OtherVehicle",
  "weight" -> abst[IO, Int],
  // Since OtherVehicle is a subtype of Vehicle
  // we can directly embed the Vehicle fields
  vehicle.abstractFields: _*
).implements[Vehicle]
```
### Interface pattern
It can be a bit cumbersome to implement an interface's fields every time it is extended.
An interface constructor has been provided that can convert field implementations into abstract fields:
```scala mdoc:silent
trait Pet {
  def name: String
  def age: Int
  def weight: Double
}

case class Dog(name: String, age: Int, weight: Double) extends Pet

val petFields = fields[IO, Pet](
  "name" -> lift(_.name),
  "age" -> lift(_.age),
  "weight" -> lift(_.weight)
)

implicit val pet = interfaceFromNel[IO, Pet](
  "Pet",
  petFields
)

implicit val dof = tpe[IO, Dog](
  "Dog",
  "bark" -> lift(_ => "woof!")
)
  .addFields(petFields.toList: _*)
  .subtypeOf[Pet]
``` 

## Input types
Review the [Input types](./input_types) section for more information.

## Other output structures
Examples of other structures can be in the [Output types](./output_types) section.

### Covariant effects
Output types in gql are covariant in `F`, such that output types written in different effects seamlessly weave together.
`fs2` provides a type that we can reuse for pure effects defined as `type Pure[A] <: Nothing`.
Consider the following:
```scala mdoc:silent
final case class Entity(
  name: String,
  age: Int
)

object Entity {
  implicit lazy val gqlType = tpe[fs2.Pure, Entity](
    "Entity",
    "name" -> lift(_.name),
    "age" -> lift(_.age)
  )
}

trait Example

tpe[IO, Example](
  "Example",
  "entity" -> lift(_ => Entity("John Doe", 42))
)
```
:::note
`Pure` will be avaiable with any effect type but `cats.Id` will not.
When working in `Pure` we have guarenteed that no there are no effects, since no value can inhabit `Nothing`.
A transformation from `cats.Id` requires walking through the ast and transforming `cats.Id` to `F`.
```scala mdoc:fail
tpe[fs2.Pure, Entity](
  "Entity",
  "name" -> lift(_.name),
  "age" -> eff(x => fs2.Pure(x.age))
)
```
:::