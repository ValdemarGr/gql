---
title: Output types
---
An output type `Out[F[_], A]` is an ast node that can take some `A` as input and produce a graphql value in `F`.
Output types act as continuations of their input types, such that a schema effectively is a tree of continuations.
The output types of gql are defined in `gql.ast` and are named after their respective GraphQL types.

Lets import the things we need: 
```scala mdoc
import gql.ast._
import gql.resolver._
import gql.dsl._
import gql._
import cats._
import cats.data._
import cats.implicits._
import fs2.Pure
```

## Scalar
`Scalar` types are composed of a name and a codec
The `Scalar` type can encode `A => Json` and decode `Json => Either[Error, A]`.
gql comes with a few predefined scalars, but you can also define your own.

For instance, the `ID` type is defined for any `Scalar` as follows:
```scala mdoc
final case class ID[A](value: A)
implicit def idTpe[A](implicit s: Scalar[A]): Scalar[ID[A]] =
  Scalar[ID[A]]("ID", x => s.encoder(x.value), v => s.decoder(v).map(ID(_)))
    .document(
      "The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache. The ID type appears in a JSON response as a String; however, it is not intended to be human-readable. When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."
    )
  
idTpe[String]
```

## Enum
`Enum` types, like `Scalar` types, are terminal types that consist of a name and non-empty bi-directional mapping from a scala type to a `String`:
```scala mdoc:silent
sealed trait Color
object Color {
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
}

enum[Color](
  "Color",
  enumInst("RED", Color.Red),
  enumInst("GREEN", Color.Green),
  enumInst("BLUE", Color.Blue)
)
```

`Enum` types have no constraints on the values they can encode or decode, so they can in fact, be dynamically typed:
```scala mdoc:silent
final case class UntypedEnum(s: String)

enum[UntypedEnum](
  "UntypedEnum",
  enumInst("FIRST", UntypedEnum("FIRST"))
)
```
:::caution
Encoding a value that has not been defined in the enum will result in a GraphQL error.
Therefore, it is recommended to enumerate the image of the enum; only use `sealed trait`s
:::

## Field
`Field` is a type that represents a field in a graphql `type` or `interface`.
A `Field[F, I, T, A]` has arguments `Arg[A]`, a continuation `Out[F, T]` and a resolver that takes `(I, A)` to `F[T]`.
Field also lazily captures `Out[F, T]`, to allow recursive types.
The `dsl` lazily captures `Out[F, T]` definitions in the implicit scope.
:::tip
Check out the [resolver section](./resolvers.md) for more info on how resolvers work.
:::

## Type (object)
`Type` is the gql equivalent of `type` in GraphQL parlance.
A `Type` consists of a name and a non-empty list of fields.
```scala mdoc:silent
final case class Domain(
  name: String,
  amount: Int
)

Type[Pure, Domain](
  "Domain",
  NonEmptyList.of(
    "name" -> Field[Pure, Domain, String, Unit](
      Applicative[Arg].unit,
      PureResolver{ case (i, _) => i.name },
      Eval.now(stringScalar)
    ),
    "amount" -> Field[Pure, Domain, Int, Unit](
      Applicative[Arg].unit, 
      PureResolver{ case (i, _) => i.amount },
      Eval.now(intScalar)
    )
  )
)
```

`Type`'s look very rough, but are significantly easier to define with the `dsl`:
```scala mdoc:silent
tpe[Pure, Domain](
  "Domain",
  "name" -> pure(_.name),
  "amount" -> pure(_.amount)
)
```

## Union
`Union` types allow unification of arbitary types.
The `Union` type defines a set of `PartialFunction`s that can specify the the type.
```scala mdoc:silent
sealed trait Animal
final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal

implicit lazy val dog = tpe[Pure, Dog](
  "Dog",
  "name" -> pure(_.name)
)

implicit lazy val cat = tpe[Pure, Cat](
  "Cat",
  "name" -> pure(_.name)
)

union[Pure, Animal](
  "Animal",
  instance[Dog]{ case x: Dog => x },
  instance[Cat]{ case x: Cat => x }
)
```
:::note
A curious reader might cosider the possibilty of using a total function form the unifying type to the subtypes.
This would also allow the scala compiler to catch non-exhaustive matches.
This is not possible, since the gql type needs to be available at the time of schema construction, and the specification function acts in query time.
:::
:::caution
Defining instances for `Animal` that are not referenced in the gql type is mostly safe, since any spread will simple give no fields.
Most GraphQL clients also handle this case gracefully, for backwards compatibility reasons.
The exception is `__typename`.
If the interpreter cannot find an instance of the value when querying for `__typename`, a GraphQL error will be returned.
:::

### Ad-hoc unions
In the true spirit of unification, `Union` types can be constructed in a more ad-hoc fashion:
```scala mdoc:silent
final case class Entity1(value: String)
final case class Entity2(value: String)

sealed trait Unification
object Unification {
  final case class E1(value: Entity1) extends Unification
  final case class E2(value: Entity2) extends Unification
}

implicit def entity1[F[_]: Monad]: Type[Pure, Entity1] = ???

implicit lazy val entity2: Type[Pure, Entity2] = ???

def t[F[_]: Monad] = 
union[F, Unification](
  "Unification",
  instance[Entity1]{ case Unification.E1(value) => value },
  instance[Entity2]{ case Unification.E2(value) => value }
)
```
### For the daring
Since the specify function is a `PartialFunction`, it is indeed possible to have no unifying type:
```scala mdoc
union[Pure, Any](
  "AnyUnification",
  instance[Entity1]{ case x: Entity1 => x },
  instance[Entity2]{ case x: Entity2 => x }
)
```
And also complex routing logic:
```scala mdoc
union[Pure, Unification](
  "RoutedUnification",
  instance[Entity1]{ case Unification.E1(x) if x.value == "Jane" => x },
  instance[Entity2]{ 
    case Unification.E1(x) => Entity2(x.value)
    case Unification.E2(x) => x
  },
)
```

## Interface
An interface is a combination of `Type` and `Union`.
```scala mdoc
sealed trait Node {
 def id: String
}

final case class Person(
  name: String,
  id: String
) extends Node

final case class Company(
  name: String,
  id: String
) extends Node

implicit lazy val person = tpe[Pure, Person](
  "Person",
  "name" -> pure(_.name),
  "id" -> pure(x => ID(x.id))
)
  
implicit lazy val company = tpe[Pure, Company](
  "Company",
  "name" -> pure(_.name),
  "id" -> pure(x => ID(x.id))
)
  
interface[Pure, Node](
  "Node",
  "id" -> pure(x => ID(x.id))
)(
  instance[Person]{ case x: Person => x },
  instance[Company]{ case x: Company => x }
)
```
:::note
In most GraphQL implementations types define the interfaces they implement, 
but in gql the interfaces define the types that extends it.
Defining interface implementations the other way around is ambiguous,
since the same type may appear two places in the schema, which raises the question of which type to use.
An important goal of gql is to be predictable, so such a design choice is not possible whilst maintaining predictability.

Furthermore, referencing implementations this way also ensures that all types in the schema can be discovered.
Inverting the relationship cloud lead to types that are not discoverable.
For the curious, draw a tree of a schema where types define the interfaces they implement and consider the implications.
:::
