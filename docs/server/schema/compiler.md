---
title: Compiler
---
The `Compiler` is a utility for combining multiple parts of `gql` into a single executable.

The `Compiler` utility has the task of parsing, preparing (validating) and construction an executable version of a GraphQL query.
The output of query compilation is either an error or an `Application`; an executable version of the query that closes over all required inputs:
```scala mdoc
import gql._

sealed trait Application[F[_]]
object Application {
  final case class Query[F[_]](run: F[QueryResult]) extends Application[F]
  final case class Mutation[F[_]](run: F[QueryResult]) extends Application[F]
  final case class Subscription[F[_]](run: fs2.Stream[F, QueryResult]) extends Application[F]
}
```

For most applications there is need for more steps than just preparing the query.
For instance production deployments can implement features such as caching, logging, metrics, tracing, authorization, to name a few.
The compiler utility consists of methods for (parsing, preparing and assembling an application) and can be composed to solve sophisticated use cases.

For instance, say that we would like to modify a phase in query compilation, such that the final executable logs queries that are too slow.
```scala mdoc
import gql._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import scala.concurrent.duration._

trait Logger[F[_]] {
  def warn(msg: String): F[Unit]
}

def lg: Logger[IO] = ???

def logSlowQueries(query: String, app: Application[IO]): Application[IO] = app match {
  case Application.Query(fa) => 
    Application.Query {
      fa.timed.flatMap{ case (dur, a) =>
        if (dur > 1.second) lg.warn(s"Slow query: $query") as a
        else IO.pure(a)
      }
    }
  case x => x
}
```

Or another example, we have a cache that we wish to clear between subscription events.
```scala mdoc

trait Cache[F[_]] {
  def clear: F[Unit]
  // other cache related functions ...
}

def addCacheClearing(cache: Cache[IO], app: Application[IO]): Application[IO] = app match {
  case Application.Subscription(stream) => 
    Application.Subscription {
      // gql doesnt not evaluate the next event before the previous has been consumed
      stream.evalTap(_ => cache.clear)
    }
  case x => x
}
```