# gql-test

# Schema
```
import ...syntax._

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
