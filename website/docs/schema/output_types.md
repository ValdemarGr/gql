---
title: Output types
---
An output type `Out[F[_], A]` is an ast node that can take some `A` as input and produce a graphql value in `F`.
Output types act as continuations of their input types, such that a schema effectively is a tree of continuations.
The output types of gql are defined in `gql.ast` and are named after their respective GraphQL types.

Lets import the things we need: 
```scala
import gql.ast._
import gql.resolver._
import gql.dsl._
import gql._
import cats._
import cats.data._
import cats.implicits._
```

## Scalar
`Scalar` types are composed of a name and a codec
The `Scalar` type can encode `A => Json` and decode `Json => Either[Error, A]`.
gql comes with a few predefined scalars, but you can also define your own.

For instance, the `ID` type is defined for any `Scalar` as follows:
```scala
final case class ID[A](value: A)
implicit def idScalar[F[_], A](implicit inner: Scalar[F, A]): Scalar[F, ID[A]] =
  Scalar("ID", inner.codec.imap(ID(_))(_.value))
  
implicitly[Scalar[Id, ID[String]]]
// res0: Scalar[Id, ID[String]] = Scalar(
//   name = "ID",
//   codec = io.circe.Codec$$anon$4@2d2c4b53
// )
```

## Enum
`Enum` types, like `Scalar` types, are terminal types that consist of a name and non-empty bi-directional mapping from a scala type to a `String`:
```scala
sealed trait Color
object Color {
  case object Red extends Color
  case object Green extends Color
  case object Blue extends Color
}

implicit def color[F[_]] = 
  Enum(
    "Color",
    NonEmptyList.of(
      "RED" -> Color.Red,
      "GREEN" -> Color.Green,
      "BLUE" -> Color.Blue
    )
  )
```

`Enum` types have no constraints on the values they can encode or decode, so they can in fact, be dynamically typed:
```scala
final case class UntypedEnum(s: String)

implicit def untypedEnum[F[_]] = 
  Enum(
    "UntypedEnum",
    NonEmptyList.of(
      "FIRST" -> UntypedEnum("FIRST")
    )
  )
```
:::caution
Encoding a value that has not been defined in the enum will result in a GraphQL error.
Therefore, it is recommended to enumerate the image of the enum; only use `sealed trait`s
:::

`Enum` types can also be constructed with the `dsl`.
```scala
implicit def color2[F[_]]: Enum[F, Color] = 
  enum(
    "Color",
    "RED" -> Color.Red,
    "GREEN" -> Color.Green,
    "BLUE" -> Color.Blue
  )
```

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
```scala
final case class Domain(
  name: String,
  amount: Int
)

implicit def domain[F[_]](implicit F: Applicative[F]): Type[F, Domain] =
  Type[F, Domain](
    "Domain",
    NonEmptyList.of(
      "name" -> Field[F, Domain, String, Unit](
        Applicative[Arg].unit,
        EffectResolver{ case (i, _) => F.pure(i.name.rightIor) },
        Eval.now(stringScalar)
      ),
      "amount" -> Field[F, Domain, Int, Unit](
        Applicative[Arg].unit, 
        EffectResolver{ case (i, _) => F.pure(i.amount.rightIor) },
        Eval.now(intScalar)
      )
    )
  )
```

`Type`'s look very rough, but are significantly easier to define with the `dsl`:
```scala
implicit def domain2[F[_]: Applicative]: Type[F, Domain] =
  tpe(
    "Domain",
    "name" -> pure(_.name),
    "amount" -> pure(_.amount)
  )
```

## Union
`Union` types allow unification of arbitary types.
The `Union` type defines a set of `PartialFunction`s that can specify the the type.
```scala
sealed trait Animal
final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal

implicit def dog[F[_]: Applicative] = tpe[F, Dog]("Dog", "name" -> pure(_.name))
implicit def cat[F[_]: Applicative] = tpe[F, Cat]("Cat", "name" -> pure(_.name))
implicit def animal[F[_]: Applicative] =
  union[F, Animal](
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
```scala
final case class Entity1(value: String)
final case class Entity2(value: String)

sealed trait Unification
object Unification {
  final case class E1(value: Entity1) extends Unification
  final case class E2(value: Entity2) extends Unification
}

implicit def entity1[F[_]: Applicative]: Type[F, Entity1] = ???
implicit def entity2[F[_]: Applicative]: Type[F, Entity2] = ???
implicit def unification[F[_]: Applicative] =
  union[F, Unification](
    "Unification",
    instance[Entity1]{ case Unification.E1(value) => value },
    instance[Entity2]{ case Unification.E2(value) => value }
  )
```
### For the daring
Since the specify function is a `PartialFunction`, it is indeed possible to have no unifying type:
```scala
def untypedUnification[F[_]: Applicative] =
  union[F, Any](
    "AnyUnification",
    instance[Entity1]{ case x: Entity1 => x },
    instance[Entity2]{ case x: Entity2 => x }
  )
```
And also complex routing logic:
```scala
def routedUnification[F[_]: Applicative] =
  union[F, Unification](
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
```scala
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

implicit def person[F[_]: Applicative] = 
  tpe[F, Person](
    "Person",
    "name" -> pure(_.name),
    "id" -> pure(x => ID(x.id))
  )
  
implicit def company[F[_]: Applicative] =
  tpe[F, Company](
    "Company",
    "name" -> pure(_.name),
    "id" -> pure(x => ID(x.id))
  )
  
implicit def node[F[_]: Applicative] =
  interface[F, Node](
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
