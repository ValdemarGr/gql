---
title: The DSL
---
:::warning
not up to date
:::
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
The simplest form of field construction comes from the `field.from` smart constructor.
It simply lifts a resolver into a field, given that a gql output type exists for the resolver output.
```scala mdoc
def r: Resolver[IO, Int, String] = Resolver.lift(i => i.toString())

val f: Field[IO, Int, String] = field.from(r)
```

Sometimes type inference cannot find the proper type for a field:
```scala mdoc:fail
field.from(Resolver.eval(i => IO(i.toString())))
```
The type parameters for `field` are partially applied, such that when type inference isn't enough, types can be supplied succinctly.
```scala mdoc:silent
field[Int].from(Resolver.eval(i => IO(i.toString())))

field.from(Resolver.eval((i: Int) => IO(i.toString())))
```

For most non-trivial fields, there is an even more concise syntax.
Just calling the `field` constructor, takes a higher order function that goes from the identity resolver to some output:
```scala mdoc:silent
field[Int](_.map(i => i * 2).evalMap(i => IO(i))): Field[IO, Int, Int]
```

### Builders
Complex structures may require many special resolver compositions.
The dsl also introduces a somethink akin to a builder pattern.
The easiet way to summon an instance is with the `builder` function.
```scala mdoc:silent
val b: FieldBuilder[IO, Int] = builder[IO, Int]
```
Often a builder is only relevant within a scope, thus one ends up having many unused builders in scope.
The `build` combinator solves with with a simple composition:
```scala mdoc:silent
build[IO, Int]{ (fb: FieldBuilder[IO, Int]) =>
  fb
}
```
The builder dsl contains most of the field related constructors:
```scala mdoc:silent
build[IO, Int]{ fb =>
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
Wrapping every field in a `field` smart constructor and then defining the resolver seperately is a bit verbose.
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

The `lift` and `eff` constructors can also 
```scala
def familyName = arg[String]("familyName")

tpe[IO, Person](
  "Person",
  "name" -> pure(familyName)(_.name + _),
  "nameEffect" -> eff(familyName)((p, f) => IO(p.name + f))
)
```

## Unification instances
`Union`s and `Interface`s require implementations of their type.

`Union` declares it's implementations on the `Union` structure.
However, `Interface` implementations are declared on the types that implement the interface.

Before continuing, lets setup the environment.
```scala
trait Vehicle { 
  def name: String
}
final case class Car(name: String) extends Vehicle
final case class Boat(name: String) extends Vehicle
final case class Truck(name: String) extends Vehicle

```

For the `Union`, variants can be declared using the `variant` function, which takes a `PartialFunction` from the unifying type to the implementation.
```scala
implicit def car: Type[IO, Car] = ???
implicit def boat: Type[IO, Boat] = ???
implicit def truck: Type[IO, Truck] = ???

union[IO, Vehicle]("Vehicle")
  .variant[Car] { case c: Car => c }
  .variant[Boat] { case b: Boat => b }
  .variant[Truck] { case t: Truck => t }
```
A shorthand function exists, if the type of the variant is a subtype of the unifying type.
```scala
union[IO, Vehicle]("Vehicle")
  .subtype[Car] 
  .subtype[Boat] 
  .subtype[Truck] 
```

For an `Interface` the same dsl exists, but is placed on the types that can implement the interface (a `Type` or another `Interface`).
```scala
implicit lazy val vehicle: Interface[IO, Vehicle] = interface[IO, Vehicle](
  "Vehicle",
  "name" -> pure(_.name)
)

tpe[IO, Car]("Car", "name" -> pure(_.name))
  .implements[Vehicle]{ case c: Car => c }
  
tpe[IO, Boat]("Boat", "name" -> pure(_.name))
  .subtypeOf[Vehicle]
  
trait OtherVehicle extends Vehicle {
  def weight: Int
}

interface[IO, OtherVehicle](
  "OtherVehicle",
  "weight" -> pure(_.weight),
  // Since OtherVehicle is a subtype of Vehicle
  // we can directly embed the Vehicle fields
  vehicle.fieldsList: _*
).subtypeOf[Vehicle]
```

## Input types
Review the [Input types](./input_types) section for more information.

## Other output structures
Examples of other structures can be in the [Output types](./output_types) section.

