---
title: Context
---

Many GraphQL implementations provide some method to pass query-wide parameters around in the graph.
gql has no such concept, it is rather a by-product of being written in tagless style.

## MTL
We can emulate context by using a `ReaderT`/`Kleisli` monad transformer from `cats`.
Writing `ReaderT`/`Kleisli` everywhere is tedious, instead consider opting for `cats.mtl.Ask`:
```scala mdoc
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
```

## Working in a specific effect
If you are working in a specific effect, you most likely have more tools to work with.
For instance, if you are using `IO`, you can use `IOLocal` to wire context through your application.

```scala mdoc:silent

trait Authorized {
  def getAuth: IO[Ior[String, Context]]
}

object Authorized {
  def fromIOLocal(iol: IOLocal[Option[Context]]) = new Authorized {
    def getAuth = iol.get.map{
      case None => Ior.Left("You must authorize to perform this action")
      case Some(c) => Ior.Right(c)
    }
  }
}

def makeSchema(implicit auth: Authorized): Schema[IO, Unit, Unit, Unit] = ???

IOLocal[Option[Context]](None).flatMap{ implicit loc =>
  implicit val auth = Authorized.fromIOLocal(loc)
  
  def s = makeSchema
  
  def runQueryWithSchema: IO[Unit] = ???
  
  def runAuthorizedQuery(userId: String): IO[Unit] =
    loc.set(Some(Context(userId))) >> runQueryWithSchema
    
  runAuthorizedQuery("john_doe")
}
```
