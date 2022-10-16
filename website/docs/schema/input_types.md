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
// res0: gql.NonEmptyArg[Int] = NonEmptyArg(
//   nec = Singleton(
//     a = ArgValue(
//       name = "superCoolArg",
//       input = cats.Later@12b4240c,
//       defaultValue = None,
//       description = None
//     )
//   ),
//   decode = gql.Arg$$$Lambda$6795/0x0000000101e7f040@2fdaf0e3
// )
```
Args can also have default values:
```scala
arg[Int]("superCoolArg", 42)
// res1: gql.NonEmptyArg[Int] = NonEmptyArg(
//   nec = Singleton(
//     a = ArgValue(
//       name = "superCoolArg",
//       input = cats.Later@5e272502,
//       defaultValue = Some(
//         value = Primitive(
//           value = 42,
//           in = Scalar(
//             name = "Int",
//             encoder = gql.ast$Scalar$$$Lambda$6965/0x0000000101f4f840@91913c,
//             decoder = gql.ast$Scalar$$$Lambda$6966/0x0000000101f4e840@33364166,
//             description = Some(
//               value = "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
//             )
//           )
//         )
//       ),
//       description = None
//     )
//   ),
//   decode = gql.Arg$$$Lambda$6795/0x0000000101e7f040@4b93f20b
// )
```
Args also have an `Applicative` instance defined for them:
```scala
import cats.implicits._

(arg[Int]("arg1"), arg[Int]("arg2", 43)).mapN(_ + _)
// res2: gql.NonEmptyArg[Int] = NonEmptyArg(
//   nec = Append(
//     leftNE = Singleton(
//       a = ArgValue(
//         name = "arg1",
//         input = cats.Later@2c7bd14e,
//         defaultValue = None,
//         description = None
//       )
//     ),
//     rightNE = Singleton(
//       a = ArgValue(
//         name = "arg2",
//         input = cats.Later@20efbd8c,
//         defaultValue = Some(
//           value = Primitive(
//             value = 43,
//             in = Scalar(
//               name = "Int",
//               encoder = gql.ast$Scalar$$$Lambda$6965/0x0000000101f4f840@2153e586,
//               decoder = gql.ast$Scalar$$$Lambda$6966/0x0000000101f4e840@7504480d,
//               description = Some(
//                 value = "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
//               )
//             )
//           )
//         ),
//         description = None
//       )
//     )
//   ),
//   decode = scala.Function1$$Lambda$6865/0x0000000101eef840@7071642
// )

arg[Int]("arg1") *> arg[Int]("arg2", 44)
// res3: gql.NonEmptyArg[Int] = NonEmptyArg(
//   nec = Append(
//     leftNE = Singleton(
//       a = ArgValue(
//         name = "arg1",
//         input = cats.Later@5ad9d8f3,
//         defaultValue = None,
//         description = None
//       )
//     ),
//     rightNE = Singleton(
//       a = ArgValue(
//         name = "arg2",
//         input = cats.Later@641879fb,
//         defaultValue = Some(
//           value = Primitive(
//             value = 44,
//             in = Scalar(
//               name = "Int",
//               encoder = gql.ast$Scalar$$$Lambda$6965/0x0000000101f4f840@330b0155,
//               decoder = gql.ast$Scalar$$$Lambda$6966/0x0000000101f4e840@c01a470,
//               description = Some(
//                 value = "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
//               )
//             )
//           )
//         ),
//         description = None
//       )
//     )
//   ),
//   decode = gql.NonEmptyArg$$anon$2$$Lambda$7664/0x0000000101325840@188bfb43
// )
```

Args can naturally be used in field definitions:
```scala
import cats._
import cats.effect._

final case class Data(str: String)

tpe[IO, Data](
  "Something",
  "field" -> pure(arg[String]("arg1", "default")){ case (data, arg1) => data.str + arg1 }
)
// res4: Type[IO, Data] = Type(
//   name = "Something",
//   fields = NonEmptyList(
//     head = (
//       "field",
//       Field(
//         args = NonEmptyArg(
//           nec = Singleton(
//             a = ArgValue(
//               name = "arg1",
//               input = cats.Later@3f7b9e08,
//               defaultValue = Some(
//                 value = Primitive(
//                   value = "default",
//                   in = Scalar(
//                     name = "String",
//                     encoder = gql.ast$Scalar$$$Lambda$6965/0x0000000101f4f840@5491211b,
//                     decoder = gql.ast$Scalar$$$Lambda$6966/0x0000000101f4e840@55b22ddd,
//                     description = Some(
//                       value = "The `String` is a UTF-8 character sequence usually representing human-readable text."
//                     )
//                   )
//                 )
//               ),
//               description = None
//             )
//           ),
//           decode = gql.Arg$$$Lambda$6795/0x0000000101e7f040@122495cb
//         ),
//         resolve = EffectResolver(
//           resolve = gql.dsl$$$Lambda$6798/0x0000000101e7d040@71ccd259
//         ),
//         output = cats.Later@2c88ac82,
//         description = None
//       )
//     ),
//     tail = List()
//   ),
//   description = None
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
    arg[Int]("age", 42)
  ).mapN(InputData.apply)
)
// res5: Input[InputData] = Input(
//   name = "InputData",
//   fields = NonEmptyArg(
//     nec = Append(
//       leftNE = Singleton(
//         a = ArgValue(
//           name = "name",
//           input = cats.Later@6890b00b,
//           defaultValue = None,
//           description = None
//         )
//       ),
//       rightNE = Singleton(
//         a = ArgValue(
//           name = "age",
//           input = cats.Later@5eacea1f,
//           defaultValue = Some(
//             value = Primitive(
//               value = 42,
//               in = Scalar(
//                 name = "Int",
//                 encoder = gql.ast$Scalar$$$Lambda$6965/0x0000000101f4f840@4965a589,
//                 decoder = gql.ast$Scalar$$$Lambda$6966/0x0000000101f4e840@20e0ec9b,
//                 description = Some(
//                   value = "The `Int` scalar type represents non-fractional signed whole numeric values. Int can represent values between -(2^31) and 2^31 - 1."
//                 )
//               )
//             )
//           ),
//           description = None
//         )
//       )
//     ),
//     decode = scala.Function1$$Lambda$6865/0x0000000101eef840@4d9d0064
//   ),
//   description = None
// )
```
