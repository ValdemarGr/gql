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
gql's GOI module introduces a codec type `IDCodec[A]` decodes an array of strings into some type `A` and encodes an `A` into an array of strings.
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

You won't be calling the encode and decode functions explicitly, but now that we have a codec for our `UserId`, let's try it out:
```scala mdoc
val encoded = userIdCodec.encode(UserId("abc", 123)).mkString_(":")
val decoded = userIdCodec.decode(encoded.split(":"))
```

Optional fields can also be modeled with `opt` method:
```scala mdoc:nest
final case class WithOpt(id1: String, id2: Option[String])

val c = (codec.string *: codec.string.opt).to[WithOpt]
c.encode(WithOpt("abc", Some("def"))).mkString_(":")
c.encode(WithOpt("abc", None)).mkString_(":")
```

Codecs can also handle errors:
```scala mdoc:nest
import java.util.UUID
val uuidCodec = codec.string.eimap[UUID](
  str => Either.catchNonFatal(UUID.fromString(str)).leftMap(_ => s"Invalid UUID '$str'"),
)(_.toString())

uuidCodec.decode(Array("abc"))
```

## Schema builder dsl
GOI provides a dsl when building na object or interface that requires global object identification.
```scala mdoc:silent
import gql.ast._
import gql.dsl._
import gql.goi.dsl._
import cats.effect._

final case class MyId(id: String)
object MyId {
  implicit lazy val myIdCodec: IDCodec[MyId] = codec.string.to[MyId]
}

final case class MyData(id: MyId, name: String)
def getData(id: MyId): IO[Option[MyData]] = IO.pure(Some(MyData(id, "name")))

val myDataGid = gid[IO, MyData, MyId](
  "MyData",
  (d: MyData) => d.id,
  (id: MyId) => getData(id)
)

implicit val myData: Type[IO, MyData] = myDataGid.tpe(
  "name" -> lift(_.name)
)
```

Once you are done declaring all of your types, you must accumulate a list of global object id's that the `node` field can fetch:
```scala mdoc:nest
import cats.effect.unsafe.implicits.global

def schemaWithGoi: IO[Schema[IO, Unit, Unit, Unit]] = Schema.simple(
    Goi.node(
      SchemaShape.unit[IO](fields("data" -> eff(_ => getData(MyId("abc"))))), 
      List(myDataGid)
    )
  )
  
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
    
  compiled.flatMap(_.traverse{ case Application.Query(fa) => fa }).unsafeRunSync()
}

def makeId(str: String) = new String(java.util.Base64.getEncoder.encode(str.getBytes()))

runWith(makeId("MyData:abc"))

runWith(makeId(""))

runWith(makeId("Other"))

runWith(makeId("MyData:abc:extra"))
```
