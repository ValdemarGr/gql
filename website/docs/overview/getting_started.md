---
title: Getting started
---

Get it here
```scala
libraryDependencies += "io.github.valdemargr" %% "gql-core" % "0.1-15a735f-20221111T001809Z-SNAPSHOT"
libraryDependencies += "io.github.valdemargr" %% "gql-http4s" % "0.1-15a735f-20221111T001809Z-SNAPSHOT"
libraryDependencies += "io.github.valdemargr" %% "gql-natchez" % "0.1-15a735f-20221111T001809Z-SNAPSHOT"
libraryDependencies += "io.github.valdemargr" %% "gql-graphqlws" % "0.1-15a735f-20221111T001809Z-SNAPSHOT"
```

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

