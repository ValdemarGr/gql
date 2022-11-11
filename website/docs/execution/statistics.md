---
title: Statistics
---
An instance of `Statistics` captures the runtime statistics of resolvers.
The `Statistics` structure uses an online linear regression algorithm to compute the relationship between batch size and execution time, such that memory usage is very minimal.

The `Statistics` object records a mapping from `String` to:
* `count`: the number of points the regression contains.
* `meanX`: the mean x coordinate of all the points.
* `meanY`: the mean y coordinate of all the points.
* `varX`: the variance of the x coordinates.
* `covXY`: the covariance of the x and y coordinates.

The `slope` of the function can be computed as `covXY / varX` and the `intercept` as `meanY - slope * meanX`.

The `intercept` acts the cost of one element while the `slope` is the per element cost.

The `intercept` is the important of the two, since it allows us to compare batch resolvers regardless of their average batch sizes.

```scala
import cats.effect._
import gql._
import scala.concurrent.duration._

import cats.effect.unsafe.implicits.global

Statistics[IO].flatMap{ stats =>
  stats.updateStats("foo", 1.millis, 1) >>
    stats.updateStats("foo", 2.millis, 4) >>
    stats.updateStats("foo", 3.millis, 7) >>
    stats.updateStats("foo", 4.millis, 10) >>
    stats.getStats("foo")
}.unsafeRunSync()
// res0: Stats = Stats(
//   initialCost = 1000.0,
//   extraElementCost = 333.3333333333333
// )
```
