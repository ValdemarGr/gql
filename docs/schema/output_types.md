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
```

## Scalar
`Scalar` types are composed of a name and a codec
The `Scalar` type can encode `A => Json` and decode `Json => Either[Error, A]`.
gql comes with a few predefined scalars, but you can also define your own.

For instance, the `ID` type is defined for any `Scalar` as follows:
```scala mdoc
final case class ID[A](value: A)
implicit def idScalar[F[_], A](implicit inner: Scalar[F, A]): Scalar[F, ID[A]] =
  Scalar("ID", inner.codec.imap(ID(_))(_.value))
  
implicitly[Scalar[Id, ID[String]]]
```

## Enum
`Enum` types, like `Scalar` types, are terminal types that consist of a name and non-empty bi-directional mapping from a scala type to a `String`:
```scala mdoc
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
```scala mdoc
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
```scala mdoc
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
Check out the [resolver section](./resolvers) for more info on how resolvers work.
:::

## Type (object)
`Type` is the gql equivalent of `type` in GraphQL parlance.
A `Type` consists of a name and a non-empty list of fields.
```scala mdoc
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
```scala mdoc
implicit def domain2[F[_]: Applicative]: Type[F, Domain] =
  tpe(
    "Domain",
    "name" -> field(pure(_.name)),
    "amount" -> field(pure(_.amount))
  )
```

## Union
`Union` types allow unification of arbitary types.
```scala mdoc
sealed trait Animal
final case class Dog(name: String) extends Animal
final case class Cat(name: String) extends Animal

implicit def dog[F[_]: Applicative] = tpe[F, Dog]("Dog", "name" -> field(pure(_.name)))
implicit def cat[F[_]: Applicative] = tpe[F, Cat]("Cat", "name" -> field(pure(_.name)))
implicit def animal[F[_]: Applicative] =
  union[F, Animal](
    "Animal",
    instance[Dog]{ case x: Dog => x },
    instance[Cat]{ case x: Cat => x }
  )
```
:::caution
Defining instances for `Animal` that are not referenced in the gql type is mostly safe, since any spread will simple give no fields.
Most GraphQL clients also handle this case gracefully, for backwards compatibility reasons.
The exception is `__typename`.
If the interpreter cannot find an instance of the value when querying for `__typename`, a GraphQL error will be returned.
:::
In the true spirit of unification
