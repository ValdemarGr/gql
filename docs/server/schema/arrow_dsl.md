---
title: Monadic DSL
---
Modelling complex evaluation with `Resolver`s can be tricky.
It often involves using `first` to pair up an arrow's result with it's input and proceeding with `map` or `contramap`.

Gql introduces a in-language monadic arrow dsl that re-writes a monadic arrow expression into a series of `map`, `contramap` and `first` invocations.
:::info
This feature is akin to the `proc` [notation in Haskell](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Arrow_proc_notation).
:::

Using the notation is straightforward.
```scala mdoc
import cats.implicits._
import cats.effect._

val dsl = gql.arrow.dsl[IO]
import dsl._
val r: Resolver[IO, Int, String] = 
  compile[Int] { i: Var[Int] =>
    for {
      a <- i(_.evalMap(x => IO(x + 2)))
      b <- a(_.evalMap(x => IO(x * 3)))
      c <- (a, b).tupled.apply(_.evalMap{ case (aa, bb) => IO(aa + bb) }.map(_.toString))
    } yield c
  }
```

Closures are illegal in the dsl, as they are refer to variables that are not guaranteed to be available, so prefer invoking the `compile` method as the last step in the expression.
```scala mdoc:crash
compile[Int] { i =>
  for {
    x <- i(_.evalMap(x => IO(x + 2)))
    o <- x(_.andThen(compile[Int]{ _ =>
      x(_.map(y => y + 2))
    }))
  } yield o
}
```