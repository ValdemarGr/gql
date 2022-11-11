---
title: The DSL
---
:::warning
not up to date
:::
The DSL consists of a set of smart constructors for the ast nodes of gql.
The source code for the DSL is very easy to follow and as such, the best documentation is the source code itself :-).

## Fields
The simplest form of field construction comes from the `field` smart constructor.
It simply lifts a resolver (and optionally an argument) into a field.
```scala
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
// res0: Field[[A]IO[A], String, String, Int] = Field(
//   args = NonEmptyArg(
//     nec = Singleton(
//       a = ArgValue(
//         name = "intArg",
//         input = cats.Later@4294418d,
//         defaultValue = None,
//         description = None
//       )
//     ),
//     decode = gql.NonEmptyArg$$$Lambda$74305/0x000000010f0f0840@4e8eb801
//   ),
//   resolve = FallibleResolver(resolve = <function1>),
//   output = cats.Later@599f68ed,
//   description = None
// )
```

### Value resolution
Wrapping every field in a `field` smart constructor and then defining the resolver seperately is a bit verbose.
There are smart constructors for three variants of field resolvers that lift the resolver function directly to a `Field`.

We must decide if the field is pure, an effect or a fallible effect:
:::note
The effect constructor is named `eff` to avoid collisions with cats-effect.
:::
```scala
final case class Person(
  name: String
)

tpe[IO, Person](
  "Person",
  "name" -> pure(_.name),
  "nameEffect" -> eff(x => IO.delay(x.name)),
  "nameFallible" -> fallible { x => 
    IO(Ior.both("some constructive error", x.name))
  }
)
```

We can also include arguments in fields:
```scala
def familyName = arg[String]("familyName")

tpe[IO, Person](
  "Person",
  "name" -> pure(familyName)(_ + _),
  "nameEffect" -> eff(familyName) { case (p, fn) => IO.delay(p.name + fn) },
  "nameFallible" -> fallible(familyName) { case (p, fn) => 
    IO(Ior.both("some constructive error for $fn", p.name)) 
  }
)
```

### Stream and Batch resolution
`StreamResolver`s can be constructed via the `stream` and `streamFallible` smart constructors.
Both smart constructors are overloaded with a variant that take an explicit "next" resolver and a variant that does not.

The `BatchResolver` can be lifted into a `Field` via the `field` smart constructor.

## Resolver composition
`Resolver`s can be composed by using the `andThen` method.
There are also several `map` variants that combine `andThen` with different types of resolvers:
```scala
val r: Resolver[IO, Int, Int] = PureResolver[IO, Int, Int](x => x)

r.andThen(PureResolver(_ + 1))

r.map(_ + 1)

r.evalMap(x => IO(x + 1))

r.fallibleMap(x => IO(Ior.both("some constructive error", x + 1)))

r.streamMap(x => fs2.Stream.iterate(x)(_ + 1).map(_.rightIor))
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

