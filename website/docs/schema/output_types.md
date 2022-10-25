---
title: Output types
---
An output type `Out[F[_], A]` is an ast node that can take some `A` as input and produce a graphql value in the effect `F`.
Output types act as continuations of their input types, such that a schema effectively is a tree of continuations.
The output types of gql are defined in `gql.ast` and are named after their respective GraphQL types.

:::note
Most examples use the `dsl` to construct output types.
The types can naturally be constructed manually as well, but this can be verbose.
:::

Lets import the things we need: 
```scala
import gql.ast._
import gql.resolver._
import gql.dsl._
import gql._
import cats._
import cats.data._
import cats.implicits._
import cats.effect._
```

## Scalar
`Scalar` types are composed of a name, an encoder and a decoder.
The `Scalar` type can encode `A => Value` and decode `Value => Either[Error, A]`.
A `Value` is a graphql value, which is a superset of json.

gql comes with a few predefined scalars, but you can also define your own.
For instance, the `ID` type is defined for any `Scalar` as follows:
```scala
final case class ID[A](value: A)

object ID {
  implicit def idTpe[F[_], A](implicit s: Scalar[F, A]): Scalar[F, ID[A]] =
    s.imap(ID(_))(_.value)
      .rename("ID")
      .document(
        """|The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache.
           |The ID type appears in a JSON response as a String; however, it is not intended to be human-readable.
           |When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID."""".stripMargin
      )
}
  
implicitly[Scalar[IO, ID[String]]]
// res0: Scalar[IO, ID[String]] = Scalar(
//   name = "ID",
//   encoder = scala.Function1$$Lambda$24491/0x0000000101d63840@532eb24a,
//   decoder = scala.Function1$$Lambda$23260/0x000000010352f840@708cd008,
//   description = Some(
//     value = """The `ID` scalar type represents a unique identifier, often used to refetch an object or as key for a cache.
// The ID type appears in a JSON response as a String; however, it is not intended to be human-readable.
// When expected as an input type, any string (such as `\"4\"`) or integer (such as `4`) input value will be accepted as an ID.""""
//   )
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

enumType[IO, Color](
  "Color",
  "RED" -> enumVal(Color.Red),
  "GREEN" -> enumVal(Color.Green),
  "BLUE" -> enumVal(Color.Blue)
)
```

`Enum` types have no constraints on the values they can encode or decode, so they can in fact, be dynamically typed:
```scala
final case class UntypedEnum(s: String)

enumType[IO, UntypedEnum](
  "UntypedEnum",
  "FIRST" -> enumVal(UntypedEnum("FIRST"))
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
```scala
final case class Domain(
  name: String,
  amount: Int
)

Type[IO, Domain](
  "Domain",
  NonEmptyList.of(
    "name" -> Field[IO, Domain, String, Unit](
      Applicative[Arg].unit,
      PureResolver{ case (i, _) => i.name },
      Eval.now(stringScalar)
    ),
    "amount" -> Field[IO, Domain, Int, Unit](
      Applicative[Arg].unit, 
      PureResolver{ case (i, _) => i.amount },
      Eval.now(intScalar)
    )
  ),
  Nil
)
```

`Type`'s look very rough, but are significantly easier to define with the `dsl`:
```scala
tpe[IO, Domain](
  "Domain",
  "name" -> pure(_.name),
  "amount" -> pure(_.amount)
)
```

:::tip
It is highly reccomended to define all `Type`s, `Union`s and `Interface`s as either `val` or `lazy val`.
:::


## Union
`Union` types allow unification of arbitary types.
The `Union` type defines a set of `PartialFunction`s that can specify the the type.
```scala
sealed trait Animal
final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal

implicit lazy val dog = tpe[IO, Dog](
  "Dog",
  "name" -> pure(_.name)
)

implicit lazy val cat = tpe[IO, Cat](
  "Cat",
  "name" -> pure(_.name)
)

union[IO, Animal]("Animal")
  .variant{ case x: Dog => x }
  .subtype[Cat]
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

implicit lazy val entity1: Type[IO, Entity1] = ???

implicit lazy val entity2: Type[IO, Entity2] = ???

union[IO, Unification]("Unification")
  .variant{ case Unification.E1(value) => value }
  .variant{ case Unification.E2(value) => value }
```
### For the daring
Since the specify function is a `PartialFunction`, it is indeed possible to have no unifying type:
```scala
union[IO, Any]("AnyUnification")
  .variant{ case x: Entity1 => x }
  .variant{ case x: Entity2 => x }
// res7: Union[IO, Any] = Union(
//   name = "AnyUnification",
//   types = NonEmptyList(
//     head = Variant(tpe = cats.Later@36ddb2cd),
//     tail = List(Variant(tpe = cats.Later@6597d4d7))
//   ),
//   description = None
// )
```
And also complex routing logic:
```scala
union[IO, Unification]("RoutedUnification")
  .variant{ case Unification.E1(x) if x.value == "Jane" => x }
  .variant{ 
    case Unification.E1(x) => Entity2(x.value)
    case Unification.E2(x) => x
  }
// res8: Union[IO, Unification] = Union(
//   name = "RoutedUnification",
//   types = NonEmptyList(
//     head = Variant(tpe = cats.Later@438d96cd),
//     tail = List(Variant(tpe = cats.Later@556dd5fa))
//   ),
//   description = None
// )
```

## Interface
An interface is a `Type` that can be "implemented".

`Interface`s have fields like `Type`s and can also be implemented by other `Type`s and `Interface`s.
`Interface`s don't declare their implementations, but rather the implementations declare their interfaces.
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
  
implicit lazy val node = interface[IO, Node](
  "Node",
  "id" -> pure(x => ID(x.id))
)

lazy val person = tpe[IO, Person](
  "Person",
  "name" -> pure(_.name),
  "id" -> pure(x => ID(x.id))
).implements[Node]{ case x: Person => x }
  
lazy val company = tpe[IO, Company](
  "Company",
  "name" -> pure(_.name),
)
  .addFields(node.fieldsList: _*)
  .subtypeOf[Node]
```

### A note on interface relationships
:::info
This sub-section is a bit of a philosophical digression and can be skipped.
:::

The nature of the `Interface` type unfortunately causes some complications.
Since a relation goes from implementation to interface, cases of ambiguity can arise of what interface to consider the "truth".
Schema validation will catch such cases, but it can still feel like a somewhat arbitrary limitation.

One could argue that the relation could simple be inverted, like unions, but alas such an endeavour has another consequence.
Conceptually an interface is defined most generally (in a core library or a most general purpose module), where implementations occur in more specific places.
Inverting the relationships of the interface would mean that the interface would have to be defined in the most specific place instead of the most general.
That is, inverting the arrows (relationships) of an interface, produces a union instead (with some extra features such as fields).

Now we must define the scala type for the interface in the most general place, but the `Interface` in the most specific?
Connecting such a graph requires significant effort (exploitation of some laziness) and as such is not the chosen approach.

## Unreachable types
gql discovers types by traversing the schema types.
This also means that even if you have a type declared it must occur in the ast to be respected.

You might want to declare types that are not yet queryable.
Or maybe you only expose an interface, but not the implementing types, thus the implementations won't be discovered.

The schema lets you declare "extra" types that should occur in introspection, rendering and evaluation (if possible):
```scala
def getNode: Node = Company("gql", "1")

def shape = SchemaShape[IO](tpe[IO, Unit]("Query", "node" -> pure(_ => getNode)))

println(shape.render)
// type Query {
//   node: Node!
// }
// 
// interface Node {
//   id: ID!
// }

def withCompany = shape.addOutputTypes(company)

println(withCompany.render)
// type Company implements Node {
//   name: String!
//   id: ID!
// }
// 
// interface Node {
//   id: ID!
// }
// 
// type Query {
//   node: Node!
// }

println(withCompany.addOutputTypes(person).render)
// type Company implements Node {
//   name: String!
//   id: ID!
// }
// 
// interface Node {
//   id: ID!
// }
// 
// type Query {
//   node: Node!
// }
// 
// type Person implements Node {
//   name: String!
//   id: ID!
// }
```
