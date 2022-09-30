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
```scala
import gql.dsl._
import gql.ast._

arg[Int]("superCoolArg")
// res0: gql.Arg[Int] = Arg(
//   entries = Vector(
//     ArgParam(
//       name = "superCoolArg",
//       input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@977510a),
//       default = None
//     )
//   ),
//   decode = gql.Arg$$$Lambda$13771/0x00000001035ba040@735f6998
// )
```
Args can also have default values:
```scala
arg[Int]("superCoolArg", Some(42))
// res1: gql.Arg[Int] = Arg(
//   entries = Vector(
//     ArgParam(
//       name = "superCoolArg",
//       input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@3d070bca),
//       default = Some(value = 42)
//     )
//   ),
//   decode = gql.Arg$$$Lambda$13771/0x00000001035ba040@735f6998
// )
```
Args also have an `Applicative` instance defined for them:
```scala
import cats.implicits._

(arg[Int]("arg1"), arg[Int]("arg2", Some(43))).mapN(_ + _)
// res2: gql.Arg[Int] = Arg(
//   entries = Vector(
//     ArgParam(
//       name = "arg1",
//       input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@d76210b),
//       default = None
//     ),
//     ArgParam(
//       name = "arg2",
//       input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@a20f492),
//       default = Some(value = 43)
//     )
//   ),
//   decode = gql.Arg$$anon$1$$Lambda$14684/0x0000000103949040@396c1bdd
// )

arg[Int]("arg1") *> arg[Int]("arg2", Some(44))
// res3: gql.Arg[Int] = Arg(
//   entries = Vector(
//     ArgParam(
//       name = "arg1",
//       input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@3c301e61),
//       default = None
//     ),
//     ArgParam(
//       name = "arg2",
//       input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@721e95),
//       default = Some(value = 44)
//     )
//   ),
//   decode = gql.Arg$$anon$1$$Lambda$14684/0x0000000103949040@d7bfe6d
// )
```

Args can naturally be used in field definitions:
```scala
import cats._
import cats.effect._

final case class Data(str: String)

tpe[IO, Data](
  "Something",
  "field" -> pure(arg[String]("arg1", Some("default"))){ case (data, arg1) => data.str + arg1 }
)
// res4: Type[IO, Data] = Type(
//   name = "Something",
//   fields = NonEmptyList(
//     head = (
//       "field",
//       Field(
//         args = Arg(
//           entries = Vector(
//             ArgParam(
//               name = "arg1",
//               input = Scalar(
//                 name = "String",
//                 codec = io.circe.Codec$$anon$4@5490aeec
//               ),
//               default = Some(value = "default")
//             )
//           ),
//           decode = gql.Arg$$$Lambda$13771/0x00000001035ba040@735f6998
//         ),
//         resolve = EffectResolver(
//           resolve = gql.dsl$$$Lambda$13774/0x00000001035be040@68ef6109
//         ),
//         output = cats.Later@78f95c37
//       )
//     ),
//     tail = List()
//   )
// )
```

## Input
Input is the record type for `In`.
Input consists of a `name` along with some fields.
It turns out that arguments and fields have the same properties and as such, `Arg` is used for fields.
```scala
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
// res5: Input[InputData] = Input(
//   name = "InputData",
//   fields = Arg(
//     entries = Vector(
//       ArgParam(
//         name = "name",
//         input = Scalar(name = "String", codec = io.circe.Codec$$anon$4@2a235dd3),
//         default = None
//       ),
//       ArgParam(
//         name = "age",
//         input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@7b9561b5),
//         default = Some(value = 42)
//       )
//     ),
//     decode = gql.Arg$$anon$1$$Lambda$14684/0x0000000103949040@6861b0e7
//   )
// )
```
