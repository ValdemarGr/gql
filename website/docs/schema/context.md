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
// res0: JsonObject = object[me -> "john_doe"]
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
// res1: IO[Unit] = FlatMap(
//   ioe = Delay(
//     thunk = cats.effect.IOLocal$$$Lambda$9202/0x0000000102a0c040@38ddcdd3,
//     event = cats.effect.tracing.TracingEvent$StackTrace
//   ),
//   f = <function1>,
//   event = cats.effect.tracing.TracingEvent$StackTrace
// )
```
:::
