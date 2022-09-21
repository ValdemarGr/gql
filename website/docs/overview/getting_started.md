---
title: Getting started
---

heyy

```scala
import cats.implicits._

val x = 11
// x: Int = 11
List(x, x).map(_ + 1)
// res0: List[Int] = List(12, 12)
val y = (Some(5), Option(4)).mapN(_ + _)
// y: Option[Int] = Some(value = 9)
```
```scala
import cats.implicits._

val x = 11
List(x, x).map(_ + 1)
val y = (Some(5), Option(4)).mapN(_ + _)
```

