---
title: Compiler
---
:::warning
not up to date
:::
A `Compiler` is an abstraction for combining multiple parts of `gql` into a single executable.

An instance `Compiler` has the task of parsing, preparing (validating) and construction an executable version of a GraphQL query.
The output of query compilation is either an error or an `Application`; an executable version of the query that closes over all required inputs:
```scala
import gql._

sealed trait Application[F[_]]
object Application {
  final case class Query[F[_]](run: F[QueryResult]) extends Application[F]
  final case class Mutation[F[_]](run: F[QueryResult]) extends Application[F]
  final case class Subscription[F[_]](run: fs2.Stream[F, QueryResult]) extends Application[F]
}
```

For most non-trivial applications the implementation of `Compiler` will usually be a bit more complex than only performing query execution related tasks.
For instance production deployments do usually implement additional features such as caching, logging, metrics, tracing, authorization, to name a few.
The parts of `gql` that the default `Compiler` instance composes (parsing, preparing and assembling an application), are exposed as functions such that any need can be implemented with ease.

For instance, say that we would like to modify a compiler such that it logs the query that is being executed, if it is too slow:
```scala
import gql._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import scala.concurrent.duration._

trait Logger[F[_]] {
  def warn(msg: String): F[Unit]
}

def lg: Logger[IO] = ???

def logSlowQueries(compiler: Compiler[IO]): Compiler[IO] = cp =>
  compiler.compile(cp).map{
    case Right(Application.Query(fa)) => 
      Right {
        Application.Query {
          fa.timed.flatMap{ case (dur, a) =>
            if (dur > 1.second) lg.warn(s"Slow query: ${cp.query}") as a
            else IO.pure(a)
          }
        }
      }
    case x => x
}
```
