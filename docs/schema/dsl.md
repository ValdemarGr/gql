---
title: The DSL
---
The DSL consists of a set of smart constructors for the ast nodes of gql.
The source code for the DSL is very easy to follow and as such, the best documentation is the source code itself :-).

## Fields
The simplest form of field construction comes from the `field` smart constructor.
It simply lifts a resolver (and optionally an argument) into a field.
```scala mdoc
import cats.data._
import cats.effect._
import cats.implicits._
import gql.dsl._
import gql.ast._
import gql.resolver._

def f = field(FallibleResolver[IO, String, String](s => IO.pure(s.rightIor)))

def intArg = arg[Int]("intArg")
field(intArg)(FallibleResolver[IO, (String, Int), String]{ case (s, i) => 
  IO.pure((s + i.toString()).rightIor)
})
```

### Value resolution
Wrapping every field in a `field` smart constructor and then defining the resolver seperately is a bit verbose.
There are smart constructors for three variants of field resolvers that lift the resolver function directly to a `Field`.

We must decide if the field is pure, an effect or a fallible effect:
:::note
The effect constructor is named `eff` to avoid collisions with cats-effect.
:::
```scala mdoc:silent
final case class Person(
  name: String
)

tpe[IO, Person](
  "Person",
  "name" -> pure(_.name),
  "nameEffect" -> eff(x => IO.delay(x.name)),
  "nameFallible" -> fallible { x => 
    IO.delay(Ior.both("some constructive error", x.name))
  }
)
```
Thereafter we must decide if any of the fields requires arguments:
```scala mdoc:silent
def familyName = arg[String]("familyName")

tpe[IO, Person](
  "Person",
  "name" -> pure(familyName)(_ + _),
  "nameEffect" -> eff(familyName) { case (p, fn) => IO.delay(p.name + fn) },
  "nameFallible" -> fallible(familyName) { case (p, fn) => 
    IO.delay(Ior.both("some constructive error for $fn", p.name)) 
  }
)
```

### Stream and Batch resolution
`StreamResolver`s can be constructed via the `stream` and `streamFallible` smart constructors.
Both smart constructors are overloaded with a variant that take an explicit "next" resolver and a variant that does not.

The `BatchResolver` can be lifted into a `Field` via the `field` smart constructor.

## Unification instances
`Union`s and `Interface`s require implementations of their type.

`Union` declares it's implementations on the `Union` structure.
However, `Interface` implementations are declared on the types that implement the interface.

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
implicit lazy val vehicle = interface[IO, Vehicle](
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
  vehicle.fieldsList: _*
).subtypeOf[Vehicle]
```
```

The implementations are called `Instance`s and consist of a `PartialFunction` that goes from the unifying type to an implementing type and a gql type for the implementing type.

The `instance` smart constructor partially applies the implementing type parameter required for an `Instance`, such that the scala compiler can be leveraged to infer the remaining type parameters:
```scala mdoc
trait Animal {
  def sound: String
}

implicit lazy val animal = interface[IO, Animal](
  "Animal",
  "sound" -> pure(_.sound)
)

case object Dog extends Animal {
  def sound = "woof"
}

tpe[IO, Dog.type](
  "Dog",
  "sound" -> pure(_.sound)
).subtypeOf[Animal]
```

## Input types
Review the [Input types](./input_types) section for more information.

## Other output structures
Examples of other structures can be in the [Output types](./output_types) section.

