---
title: Monadic Resolver DSL
---
Modelling complex evaluation with `Resolver`s can be tricky.
It often involves using `first` to pair up an arrow's result with it's input and proceeding with `map` or `contramap`.

Gql introduces a in-language monadic arrow dsl that re-writes a monadic arrow expression into a series of `map`, `contramap` and `first` invocations.
:::info
This feature is akin to the `proc` [notation in Haskell](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Arrow_proc_notation).
:::

Using the notation is straightforward, the same (covariant) combinators for `Resolver` exist in the arrow dsl.
```scala mdoc:silent
import gql.resolver._
import cats.implicits._
import cats.effect._
import gql.arrow._

// Bind the effect type (IO) to aid with compiler errors and inference
val d = dsl[IO]
import d._
val r: Resolver[IO, Int, String] = 
  proc[Int] { i: Var[Int] =>
    for {
      a <- i.evalMap(x => IO(x + 2))
      b <- a.evalMap(x => IO(x * 3))
      c <- (a, b).tupled.evalMap{ case (aa, bb) => IO(aa + bb) }
    } yield c.map(_.toString)
  }
```

<details>
  <summary>Most syntatic extensions don't make much sense unless the arrow type (Resolver) is bound which requires knowing the effect type. The full monadic arrows language is available as toplevel functions also.</summary>

```scala mdoc:nest
import gql.arrow.{Language => L}
L.proc[Resolver[IO, *, *], Int, String] { i =>
  for {
    x <- L.declare[Resolver[IO, *, *], Int, Int](i)(Resolver.lift[IO, Int](z => z * 2))
    y <- L.declare[Resolver[IO, *, *], (Int, Int), String]((x, x).tupled)(Resolver.lift[IO, (Int, Int)]{ case (a, b) => (a + b).toString() })
  } yield y
}
```

</details>

The underlying arrow is also available for composition via `apply`.
```scala mdoc:silent:nest
proc[Int] { i =>
  for {
    x <- i(_.evalMap(z => IO(z + 1)))
    out <- x.apply(_.map(_.toString))
  } yield out
}
```

### Technical details
The dsl introduces two datatypes, `Var` and `Decl`.
* `Var` is a reference to a set of variables that occur in the arrow. `Var` forms an `Applicative`.
* `Decl` is used to re-write the monadic (`flatMap`) structure into an arrow. `Decl` forms a `Monad`.

The primary use of `Decl` is to bind variables.
Every transformation on a `Var`iable introduces a new `Var`iable which is stored in the `Decl` structure.

:::info
Since `Var` forms an `Applicative` that implies that `map` is available for `Var`.
`map` for `Var` is not memoized since it does not lift `Var` into `Decl`.
`Var` has an extension [`rmap`](https://github.com/typelevel/cats/blob/c8aabcacd6045b9aed5c8626c4bf5308dd3f4912/core/src/main/scala/cats/arrow/Profunctor.scala#L59) which introduces a new `Var`iable that memoizes the result.
That is, the following equivalences holds:
```scala
declare((v: Var[A]).map(f))(Resolver.id[F, A]) <-> 
  (v: Var[A]).rmap(f) <->
  (v: Var[A]).apply(_.map(f))
```
:::

Closures are illegal in the dsl, as they are refer to variables that are not guaranteed to be available, so prefer invoking `proc` once per `Resolver`.
```scala mdoc
println {
  scala.util.Try {
    proc[Int] { i =>
      for {
        x <- i.evalMap(x => IO(x + 2))
        o <- x.andThen(proc[Int]{ _ =>
          x.rmap(y => y + 2)
        })
      } yield o
    }
  }.toEither.leftMap(_.getMessage)
}
```

### Builder extensions
The dsl includes an extension method to `FieldBuilder` that eases construction of `Field`s.
The dsl also enhances any resolver with a `proc` extension method.
```scala mdoc:silent
import gql.ast._

val gqlDsl = gql.dsl.GqlDsl[IO]
import gqlDsl._

builder[Unit]{ b =>
  b.tpe(
    "MyType",
    "field" -> b.proc{ i =>
      for {
        x <- i.evalMap(_ => IO(1 + 2))
        y <- x.rmap(_ + 3)
      } yield y
    },
    "otherField" -> b(_.proc{ i =>
      i.evalMap(_ => IO(1 + 2))
    })
  )
}
```

### Composition
Sharing common sub-arrows is a desirable property.
This can is expressed naturally with the dsl.
```scala mdoc:nest
def mulDiv(i: Var[Int]): Decl[Var[Int]] = for {
  x <- i.rmap(_ * 2)
  y <- x.rmap(_ / 2)
} yield y

proc[Int](mulDiv(_) >>= mulDiv)

proc[Int](mulDiv(_) >>= mulDiv >>= mulDiv)
```

#### Toplevel expressions
It is recommended to always work in a scope with your effect type (`F`) bound, to ease inference and type signatures.
There is however support for toplevel proc resolver expressions.
```scala mdoc:nest
def toplevelMulDiv[F[_]](i: Var[Int]): ResolverDecl[F, Var[Int]] = {
  val d = dsl[F]
  import d._
  for {
    x <- i.rmap(_ * 2)
    y <- x.rmap(_ / 2)
  } yield y
}
```

Passing the dsl as an implicit parameter is also an option.
```scala mdoc:nest
def toplevelMulDiv[F[_]](i: Var[Int])(implicit d: ResolverArrowDsl[F]): ResolverDecl[F, Var[Int]] = {
  import d._
  for {
    x <- i.rmap(_ * 2)
    y <- x.rmap(_ / 2)
  } yield y
}
```

## Lifting arguments
Request arguments is made easier by the arrow dsl.
```scala mdoc:silent
proc[Int] { i =>
  for {
    x <- i.evalMap(x => IO(x + 2))
    y <- argument(arg[Int]("age"))
    z <- (x, y).tupled.evalMap { case (a, b) => IO(a + b) }
  } yield z
}
```

## Choice
The dsl also covers `ArrowChoice`'s `choice` combinator.
```scala mdoc:silent
proc[Int] { i =>
  for {
    x <- i.rmap(v => if (v > 5) Left(v) else Right(v))
    y <- x.choice(
      l => l.rmap(_ * 2),
      r => for {
        a <- argument(arg[Int]("age"))
        out <- (a, r, i).tupled.rmap{ case (a, b, c) => a + b + c }
      } yield out
    )
  } yield y
}
```

## Batching example
Some steps commonly occur when writing batched resolvers:
1. Pulling an id out of the parent datatype.
2. Passing the id to a batching resolver.
3. Pairing the batched output with the parent datatype.

This pairing requires some clever use of `first` and `contramap/lmap`.
This behaviour is much easier to express monadically since we have access to closures.
```scala mdoc:silent
def getAddresses(ids: Set[Int]): IO[Map[Int, String]] =
  IO(ids.toList.map(id => id -> s"Address $id").toMap)

case class DataType(id: Int, name: String)
proc[DataType] { i =>
  for {
    id <- i.rmap(_.id)
    r = Resolver.inlineBatch[IO, Int, String](getAddresses).opt
    (addr: Var[Option[String]]) <- id.andThen(r)
    p = (i, addr).tupled
    out <- p.rmap{ case (dt, a) => s"${dt.name} @ ${a.getOrElse("<unknown>")}" }
  } yield out
}
```

## Arrowless final?
Expressions can be declared for any arrow, not just `Resolver`.
The usefullness of this property is not significant, but an interesting property nonetheless.
```scala mdoc:nest:silent
import cats.free._
import cats.arrow._
def mulDiv[F2[_, _]](v: Var[Int]): Free[DeclAlg[F2, *], Var[Int]] = {
  val d = new Language[F2] {}
  import d._
  // We can ask for the arrow evidence that must occur when some proc compiles us
  askArrow.flatMap{ implicit arrow: Arrow[F2] =>
    for {
      x <- v.rmap(_ * 2)
      y <- x.rmap(_ / 2)
    } yield y
  }
}

proc[Int] { i =>
  for {
    x <- i.rmap(_ * 2)
    y <- mulDiv(x)
  } yield y
}
```
