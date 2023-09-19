---
title: Global object identification
---
gql also supports global object identification.

:::info
Global object identification is primarily used by Relay clients to refetch objects.
:::

Global object identification requires two things:
 1. An id field on the object type.
 2. A node field on the query type to look objects up.

## Codecs
gql's global object identification (goi) module introduces a codec type `IDCodec[A]` decodes an array of strings into some type `A` and encodes an `A` into an array of strings.
```scala mdoc:silent
import cats.implicits._
import gql._
import gql.goi._
import gql.goi.codec

final case class UserId(
  id1: String,
  id2: Int
)

val userIdCodec: IDCodec[UserId] = (codec.string *: codec.int).to[UserId]
```

:::info
The `*:` composition syntax is provided on top of the `twiddles` library to map tuples to and from case classes.
Consider taking a look at the [twiddles documentation](https://github.com/typelevel/twiddles)
:::

You won't be calling the encode and decode functions explicitly, but now that we have a codec for our `UserId`, let's try it out.
```scala mdoc
val encoded = userIdCodec.encode(UserId("abc", 123)).mkString_(":")
val decoded = userIdCodec.decode(encoded.split(":"))
```

Optional fields can also be modeled with the `opt` method:
```scala mdoc:nest
final case class WithOpt(id1: String, id2: Option[String])

lazy val c = (codec.string *: codec.string.opt).to[WithOpt]
c.encode(WithOpt("abc", Some("def"))).mkString_(":")
c.encode(WithOpt("abc", None)).mkString_(":")
```

Codecs can also handle errors.
```scala mdoc:nest
import java.util.UUID
lazy val uuidCodec = codec.string.eimap[UUID](
  str => Either.catchNonFatal(UUID.fromString(str)).leftMap(_ => s"Invalid UUID '$str'"),
)(_.toString())

uuidCodec.decode(Array("abc"))
```

## Schema builder dsl
GOI provides a dsl when building an object or interface that requires global object identification.
To add goi to a `Type[F, A]` you must provide:
* A function `A => B` where `B` has a `IDCodec` instance.
* A function `NonEmptyList[B] => F[Map[B, A]]` that can fetch items if requested through the `node` field.
```scala mdoc:silent
import gql.ast._
import gql.dsl._
import gql.goi.dsl._
import cats.effect._
import cats.data._

final case class MyId(id: String)
object MyId {
  implicit lazy val myIdCodec: IDCodec[MyId] = codec.string.to[MyId]
}

final case class MyData(id: MyId, name: String)
def getData(id: MyId): IO[Option[MyData]] = IO.pure(Some(MyData(id, "name")))

implicit val myData: Type[IO, MyData] = tpe[IO, MyData](
  "MyData",
  "name" -> lift(_.name)
).goi(_.id) { keys: NonEmptyList[MyId] =>
  keys.toList
    .traverse(k => getData(k).map(k -> _))
    .map(_.collect{ case (k, Some(v)) => k -> v }.toMap)
}
```

Once you are done declaring all of your types, you must accumulate a list of global object id's that the `node` field can fetch.

Gql's ast is extensible with user definable attributes, so we can introspect the schema to find all of the goi information we need.
```scala mdoc:nest
import cats.effect.unsafe.implicits.global
import io.circe.syntax._

def schemaWithGoi: IO[Schema[IO, Unit, Unit, Unit]] = Schema.simple {
  Goi.addSchemaGoi(SchemaShape.unit[IO](fields("data" -> eff(_ => getData(MyId("abc"))))))
}
  
def runWith(id: String) = {
  def compiled = schemaWithGoi.map{ schema =>
    Compiler[IO].compile(
      schema,
      s"""
        query {
          node(id: "$id") {
            ... on MyData {
              id
              name
            }
          }
        }
      """
    )
  }
    
  compiled
    .flatMap(_.traverse{ case Application.Query(fa) => fa })
    .unsafeRunSync()
    .toOption
    .get.asJson.spaces2
}

def makeId(str: String) = new String(java.util.Base64.getEncoder.encode(str.getBytes()))

runWith(makeId("MyData:abc"))

runWith(makeId(""))

runWith(makeId("Other"))

runWith(makeId("Query"))

runWith(makeId("MyData:abc:extra"))
```
