---
title: Context
---

Many GraphQL implementations provide some method to pass query-wide parameters around in the graph.
gql has no such concept, it is rather a by-product of being written in tagless style.
We can emulate context by using a `ReaderT`/`Kleisli` monad transformer from `cats`.
Writing `ReaderT`/`Kleisli` everywhere is tedious, instead consider opting for `cats.mtl.Ask`~

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

def getSchema[F[_]: Applicative](implicit A: Ask[F, Context]): Schema[F, Unit] = {
  def schema: Schema[F, Unit] = Schema.simple[F, Unit](
    tpe(
      "Query",
      "me" -> eff(_ => A.ask.map(_.userId))
    )
  )
  
  schema
}

type G[A] = Kleisli[IO, Context, A]
def s = getSchema[G]

def query = """
  query {
    me
  }
"""
  
def parsed = gql.parser.parse(query).toOption.get
 
implicit lazy val stats = 
  Statistics[IO].unsafeRunSync().mapK(Kleisli.liftK[IO, Context])

def queryResult = Execute.executor(parsed, s, Map.empty) match {
  case Execute.ExecutorOutcome.Query(run) => run(()).map { case (_, output) => output } 
}

queryResult.run(Context("john_doe")).unsafeRunSync()
```
