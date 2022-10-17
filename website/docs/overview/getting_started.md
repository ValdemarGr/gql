---
title: Getting started
---

heyy

```scala mdoc
import cats.implicits._

val x = 11
List(x, x).map(_ + 1)
val y = (Some(5), Option(4)).mapN(_ + _)
```
```scala mdoc:silent:reset
import cats.implicits._

val x = 11
List(x, x).map(_ + 1)
val y = (Some(5), Option(4)).mapN(_ + _)
```

