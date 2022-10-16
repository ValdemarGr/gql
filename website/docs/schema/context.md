---
title: Context
---

Many GraphQL implementations provide some method to pass query-wide parameters around in the graph.
gql has no such concept, it is rather a by-product of being written in tagless style.

## MTL
We can emulate context by using a `ReaderT`/`Kleisli` monad transformer from `cats`.
Writing `ReaderT`/`Kleisli` everywhere is tedious, instead consider opting for `cats.mtl.Ask`:
```scala
import gql._
import gql.dsl._
import gql.ast._
import cats.mtl.Ask
import cats._
import cats.data._
import cats.implicits._
import io.circe._
import cats.effect._
import cats.effect.unsafe.implicits.global

final case class Context(
  userId: String
)

def queries[F[_]: Functor](implicit A: Ask[F, Context]): Type[F, Unit] = 
  tpe[F, Unit](
    "Query",
    "me" -> eff(_ => A.ask.map(_.userId))
  )

type G[A] = Kleisli[IO, Context, A]

def query = """
  query {
    me
  }
"""

Statistics[IO].flatMap{ stats =>
  val schema =
    Schema.query(stats.mapK(Kleisli.liftK[IO, Context]))(queries[G])
    
  Compiler[G].compile(schema, query) match {
    case Right(Application.Query(fa)) => 
      fa
        .run(Context("john_doe"))
        .map(_.asGraphQL)
  }
}.unsafeRunSync()
// res0: JsonObject = object[data -> {
//   "me" : "john_doe"
// }]
```

## Working in a specific effect
If you are working in a specific effect, you most likely have more tools to work with.
For instance, if you are using `IO`, you can use `IOLocal` to wire context through your application.
:::note
For the case of `IOLocal`, I don't think it is possible to provide a context implementation without a bit of unsafe code or other compromises.
```scala
def makeSchema(implicit loc: IOLocal[Context]): Schema[IO, Unit] = ???

IOLocal(null: Context).flatMap{ implicit loc =>
  def s = makeSchema
  
  def runQueryWithSchema: IO[Unit] = ???
  
  def runAuthorizedQuery(userId: String): IO[Unit] =
    loc.set(Context(userId)) >> runQueryWithSchema
    
  runAuthorizedQuery("john_doe")
}
```
:::
