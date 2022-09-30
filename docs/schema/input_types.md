---
title: Input types
---
An input type `In[A]` defines an input type and a mapping from the input type to `A`.
Input types occur as parameters in queries as a way to let the caller provide arguments to query resolution.

## Scalar
The `Scalar` type defines a terminal input type, and can be any json value.
`Scalar`s act as both input and output types; refer to [output types](./output_types#scalar) for more information on how scalar types work.

## Enum
The `Enum` type defines a mapping from a string to a value (usually a sealed trait) `A`.
More information can be found in the [output types](./output_types#enum) section.

## Arg
The arg type has a couple of uses.
The first and simplest way of using args is for, well, arguments.
The dsl has a smart constructor for arguments that summons the `In[A]` type from the implicit scope, for the argument.
```scala mdoc
import gql.dsl._
import gql.ast._

arg[Int]("superCoolArg")
```
Args can also have default values:
```scala mdoc
arg[Int]("superCoolArg", Some(42))
```
Args also have an `Applicative` instance defined for them:
```scala mdoc
import cats.implicits._

(arg[Int]("arg1"), arg[Int]("arg2", Some(43))).mapN(_ + _)

arg[Int]("arg1") *> arg[Int]("arg2", Some(44))
```

Args can naturally be used in field definitions:
```scala mdoc
import cats._
import cats.effect._

final case class Data(str: String)

tpe[IO, Data](
  "Something",
  "field" -> pure(arg[String]("arg1", Some("default"))){ case (data, arg1) => data.str + arg1 }
)
```

## Input
Input is the record type for `In`.
Input consists of a `name` along with some fields.
It turns out that arguments and fields have the same properties and as such, `Arg` is used for fields.
```scala mdoc
final case class InputData(
  name: String,
  age: Int
)

input[InputData](
  "InputData",
  (
    arg[String]("name"),
    arg[Int]("age", Some(42))
  ).mapN(InputData.apply)
)
```
