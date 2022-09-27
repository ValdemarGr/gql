---
title: The DSL
---
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
import gql.resolver._

def f = field(EffectResolver[IO, String, String](s => IO.pure(s.rightIor)))

def intArg = arg[Int]("intArg")
val withArg = field(intArg)(EffectResolver[IO, (String, Int), String]{ case (s, i) => 
  IO.pure((s + i.toString()).rightIor)
})
// withArg: gql.ast.Field[[A]IO[A], String, String, Int] = Field(
//   args = Arg(
//     entries = Vector(
//       ArgParam(
//         name = "intArg",
//         input = Scalar(name = "Int", codec = io.circe.Codec$$anon$4@2139435d),
//         default = None
//       )
//     ),
//     decode = gql.Arg$$$Lambda$6743/0x0000000101d24840@6e5166cc
//   ),
//   resolve = EffectResolver(resolve = <function1>),
//   output = cats.Later@b904065
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

def t =
  tpe[IO, Person](
    "Person",
    "name" -> pure(_.name),
    "nameEffect" -> eff(x => IO.delay(x.name)),
    "nameFallible" -> fallible { x => 
      IO.delay(Ior.both("some constructive error", x.name))
    }
  )
```
Thereafter we must decide if any of the fields requires arguments:
```scala
def familyName = arg[String]("familyName")

def t2 =
  tpe[IO, Person](
    "Person",
    "name" -> pure(familyName)(_ + _),
    "nameEffect" -> eff(familyName) { case (p, fn) => IO.delay(p.name + fn) },
    "nameFallible" -> fallible(familyName) { case (p, fn) => 
      IO.delay(Ior.both("some constructive error for $fn", p.name)) 
    }
  )
```

### Stream and Batch resolution
`StreamResolver`s can be constructed via the `stream` and `streamFallible` smart constructors.
Both smart constructors are overloaded with a variant that take an explicit "next" resolver and a variant that does not.

The `StreamResolver` and `BatchResolver` can be lifted into a `Field` via the `field` smart constructor.

## Sturctures
`Type`s, `Enum`s, `Interface`s and `Union`s are constructed via the `tpe`, `enum`, `interface` and `union` smart constructors respectively.
The structural type smart constructors take a varargs argument structural values that are converted into non empty lists.

## Unification instances
`Union`s and `Interface`s require implementations of their type.
The implementations are called `Instance`s and consist of a `PartialFunction` that goes from the unifying type to an implementing type and a gql type for the implementing type.

The `instance` smart constructor partially applies the implementing type parameter required for an `Instance`, such that the scala compiler can be leveraged to infer the remaining type parameters:
```scala
trait Animal {
  def sound: String
}

case object Dog extends Animal {
  def sound = "woof"
}
implicit def dogType = tpe[IO, Dog.type]("Dog", "sound" -> pure(_.sound))

val it =
  interface[IO, Animal](
    "Animal",
    "sound" -> pure(_.sound)
  )(instance[Dog.type]{ case Dog => Dog })
// it: gql.ast.Interface[[_]IO[_], Animal] = Interface(
//   name = "Animal",
//   instances = List(Instance(ol = cats.Later@37386ffc)),
//   fields = NonEmptyList(
//     head = (
//       "sound",
//       Field(
//         args = Arg(
//           entries = Vector(),
//           decode = gql.Arg$$anon$1$$Lambda$6944/0x0000000101e98840@61beee00
//         ),
//         resolve = EffectResolver(
//           resolve = gql.dsl$$$Lambda$6746/0x0000000101d23040@1333b9de
//         ),
//         output = cats.Later@3fba22c7
//       )
//     ),
//     tail = List()
//   )
// )
```
