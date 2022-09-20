---
title: Output types
---
An output type `Out[F[_], A]` is a node in the graph that can take some `A` as input and produce a graphql value in `F`.
The output types of gql are defined in `gql.ast` and are named after their respective GraphQL types.

## Scalar
`Scalar` types are composed of a name and a codec
The `Scalar` type can encode `A => Json` and decode `Json => Either[Error, A]`.
gql comes with a few predefined scalars, but you can also define your own.

For instance, the `ID` type is defined for any `Scalar` as follows:
```scala mdoc
import cats.implicits._

import gql.ast.{stringScalar, Scalar}

final case class ID[A](value: A)
implicit def idScalar[F[_], A](implicit inner: Scalar[F, A]): Scalar[F, ID[A]] =
  Scalar("ID", inner.codec.imap(ID(_))(_.value))
```

## Enum
`Enum` types, like `Scalar` types, are terminal types that consist of a name and non-empty bi-directional mapping from a scala type to a `String`:
```scala mdoc:reset
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
```scala mdoc
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
