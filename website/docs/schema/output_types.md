---
title: Output types
---
An output type `Out[F[_], A]` is an ast node that can take some `A` as input and produce a graphql value in `F`.
Output types act as continuations of their input types, such that a schema effectively is a tree of continuations.
The output types of gql are defined in `gql.ast` and are named after their respective GraphQL types.

## Scalar
`Scalar` types are composed of a name and a codec
The `Scalar` type can encode `A => Json` and decode `Json => Either[Error, A]`.
gql comes with a few predefined scalars, but you can also define your own.

For instance, the `ID` type is defined for any `Scalar` as follows:
```scala
import cats.implicits._
import cats._

import gql.ast.{stringScalar, Scalar}

final case class ID[A](value: A)
implicit def idScalar[F[_], A](implicit inner: Scalar[F, A]): Scalar[F, ID[A]] =
  Scalar("ID", inner.codec.imap(ID(_))(_.value))
  
implicitly[Scalar[Id, ID[String]]]
// res0: Scalar[Id, ID[String]] = Scalar(
//   name = "ID",
//   codec = io.circe.Codec$$anon$4@5351004c
// )
```

## Enum
`Enum` types, like `Scalar` types, are terminal types that consist of a name and non-empty bi-directional mapping from a scala type to a `String`:
```scala
import cats.data._
import gql.ast.Enum

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
import cats.data._
import gql.ast.Enum

final case class UntypedEnum(s: String)

implicit def untypedEnum[F[_]] = 
  Enum(
    "UntypedEnum",
    NonEmptyList.of(
      "FIRST" -> UntypedEnum("FIRST")
    )
  )
```

## Field
`Field` is a type that represents a field in a graphql `type` or `interface`.
A `Field[F, I, T, A]` has arguments `Arg[A]`, a continuation `Out[F, T]` and a resolver that takes `(I, A)` to `F[T]`.
Field also lazily captures `Out[F, T]`, to allow recursive types.
:::tip
Check out the [resolver section](./resolvers) for more info on how resolvers work.
:::

## Type (object)
`Type` is the gql equivalent of `type` in GraphQL parlance.
A `Type` consists of a name and a non-empty list of fields.

