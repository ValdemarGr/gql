# gql-test

# Schema
```scala
// ....scala
object syntax {
  implicit def intScalar[F[_]]: ScalarType[F, Int] = ???
  implicit def stringScalar[F[_]]: ScalarType[F, Int] = ???
  implicit def listType[F[_], A](implicit t: OutputType[A]): ListOutputType[F, Int] = ???
  
  def pure[F[_], I, A](resolve: I => A)(implicit tpe: => OutputType[F, A]): Field[F, I, A] = ???
  def effect[F[_], I, A](resolve: I => F[A])(implicit tpe: => OutputType[F, A]): Field[F, I, A] = ???
  def outputObject[F[_], I](
    name: String,
    hd: (String, Field[F, I, _]),
    tl: (String, Field[F, I, _])*
  ): ObjectOutputType[F, I, A] = ???
}

// App.scala
import ....syntax._

final case class Data[F[_]](
  a: Int,
  b: F[Int],
  c: F[List[Data[F]]]
)

object Data {
  implicit def gql[F[_]]: OutputType[Data[F]] = 
    outputObject(
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b),
      "c" -> effect(_.c)
    )
}

def makeData[F[_]]: Data[F] = ???
```

# Getting started
## Intro
### Deps
...

## Example

