package gql

import cats.effect._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.Queue
import cats.data.NonEmptyList

trait Statistics[F[_]] {
  def getStatsOpt(name: String): F[Option[Statistics.Stats]]

  def getStats(name: String): F[Statistics.Stats]

  def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit]
}

object Statistics {

  final case class CovVarRegression(
      count: Long,
      meanX: Double,
      meanY: Double,
      varX: Double,
      covXY: Double
  ) {
    lazy val slope: Double = covXY / varX

    lazy val intercept: Double = meanY - slope * meanX

    def apply(x: Double): Double = intercept + slope * x

    def add(x: Double, y: Double, weight: Double = 1d): CovVarRegression = {
      val n = (count + 1).toDouble

      val dx = x - meanX
      val dy = y - meanY

      val newVarX = varX + (((n - 1d) / n) * dx * dx * weight - varX) / n
      val newCovXY = covXY + (((n - 1d) / n) * dx * dy * weight - covXY) / n

      val newMeanX = meanX + (dx * weight) / n
      val newMeanY = meanY + (dy * weight) / n

      CovVarRegression(count + 1, newMeanX, newMeanY, newVarX, newCovXY)
    }
  }

  final case class NodeTypeRegression(
      sumx: Double,
      sumxx: Double,
      sumy: Double,
      sumxy: Double,
      n: Int,
      // averages
      xbar: Double,
      ybar: Double
  ) {
    def remove(x: Double, y: Double) = {
      val newN = n - 1
      val ratio = n.toDouble / newN.toDouble
      val dx = x - xbar
      val dy = y - ybar
      copy(
        sumxx = sumxx - dx * dx * ratio,
        sumxy = sumxy - dx * dy * ratio,
        xbar = xbar - dx / newN.toDouble,
        ybar = ybar - dy / newN.toDouble,
        n = newN,
        sumx = sumx - x,
        sumy = sumy - y
      )
    }

    def add(x: Double, y: Double): NodeTypeRegression = {
      val newN = n + 1
      val ratio = n.toDouble / newN.toDouble
      val dx = x - xbar
      val dy = y - ybar
      copy(
        sumxx = sumxx + dx * dx * ratio,
        sumxy = sumxy + dx * dy * ratio,
        xbar = xbar + dx / newN.toDouble,
        ybar = ybar + dy / newN.toDouble,
        n = newN,
        sumx = sumx + x,
        sumy = sumy + y
      )
    }

    lazy val intercept: Double = (sumy - slope * sumx) / n.toDouble

    lazy val slope: Double = sumxy / sumxx

    def apply(x: Double): Double =
      slope * x + intercept
  }

  final case class GradientDecentRegression(
      error: Double,
      n: Int,
      slope: Double,
      intercept: Double
  ) {
    def apply(x: Double): Double = slope * x + intercept

    def add(x: Double, y: Double): GradientDecentRegression = {
      val m = slope
      val b = intercept

      // val appliedPDB = 1d / n * points.map(p => pred(p.x) - p.y).sumAll
      // val appliedPDM = 1d / n * points.map(p => (pred(p.x) - p.y) * p.x).sumAll
      ???
    }
  }

  final case class Point(x: Double, y: Double)

  object GradientDecentRegression {
    def fit(points: NonEmptyList[Point]): GradientDecentRegression = {
      // the error/cost function will be 1 / n * sum((y - (slope * x + intercept))^2)
      // we want to figure out what direction to move the slope and intercept to minimize this error
      // the error function of n^2 is a parabola
      // such that taking the partial derivative is the rate of error
      //
      // now we should just re-adjust the slope and intercept by applying their respective derived functions
      // and multiply by a learning rate
      //
      // since a function of a parabola is linear, re-adjusting by the rate of error will very quickly converge

      val n = points.size.toDouble

      def go(its: Int, alpha: Double, m: Double, b: Double, prevErr: Double): (Double, Double) = {
        if (its == 1000) (m, b)
        else {
          def pred(x: Double) = m * x + b
          // val gradB = -2d * points.map(p => p.y - pred(p.x)).sumAll / n
          // val gradM = -2d * points.map(p => p.x * (p.y - pred(p.x))).sumAll / n
          // val newB = b + alpha * gradB
          // val newM = m - alpha * gradM
          val appliedPDB = 1d / n * points.map(p => pred(p.x) - p.y).sumAll
          val appliedPDM = 1d / n * points.map(p => (pred(p.x) - p.y) * p.x).sumAll
          val optB = b - alpha * appliedPDB
          val optM = m - alpha * appliedPDM
          val err = 1d / n * points.map(p => math.pow(p.y - (optM * p.x + optB), 2d)).sumAll
          // println(s"$its: err: $err, prevSlope: $m, slope: $optM, prevIntercept: $b, intercept: $optB, alpha: $alpha, pdb: $appliedPDB, pdm: $appliedPDM")
          // println(s"partial derivation b function = 1 / ${n.toInt} * sum i=1 to n {($m * x_i + b) - y_i}")
          // println(s"partial derivation for b=$b = 1 / ${n.toInt} * sum i=1 to n {($m * x_i + $b) - y_i} = $appliedPDB")
          // println(s"partial derivation m function = 1 / ${n.toInt} * sum i=1 to n {x_i * ((m * x_i + $b) - y_i)}")
          // println(s"partial derivation for m=$m = 1 / ${n.toInt} * sum i=1 to n {x_i * (($m * x_i + $b) - y_i)} = $appliedPDM")
          // println(s"error equation: 1 / ${n.toInt} * sum i=1 to n {y_i - ($optM * x_i + $optB)^2} = $err")
          // go(its + 1, alpha, newM, newB, prevErr)
          if (err >= prevErr) go(its + 1, alpha / 2d, m, b, prevErr)
          else go(its + 1, alpha * 2d, optM, optB, err)
        }
      }

      val (m, b) = go(0, 0.01d, 0d, 0d, Double.MaxValue)

      GradientDecentRegression(
        1d / n * points.map(p => math.pow(m * p.x + b - p.y, 2d)).sumAll,
        points.size,
        m,
        b
      )
    }
  }

  final case class BatchStats(
      batchSize: Int,
      elapsed: FiniteDuration
  )
  final case class Stats(
      name: String,
      count: Int,
      previousK: Queue[BatchStats],
      average0toK: FiniteDuration,
      average0toKBatchSize: Int,
      average0toKTimeBatchElem: Option[FiniteDuration]
  )

  def apply[F[_]](implicit F: Concurrent[F]): F[Statistics[F]] =
    F.ref(Map.empty[String, Stats])
      .map { state =>
        new Statistics[F] {
          override def getStatsOpt(name: String): F[Option[Stats]] =
            state.get.map(_.get(name))

          override def getStats(name: String): F[Stats] =
            getStatsOpt(name)
              .flatMap {
                case Some(x) => F.pure(x)
                case None    => F.raiseError(new Exception(s"stats not found for $name"))
              }

          override def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit] =
            state.update { m =>
              ???
            // m.get(name) match {
            //   case None => m + (name -> Stats(name, 1, elapsed, elapsed / batchElems))
            //   case Some(s) =>
            //     val avg = s.average
            //     val batchElemIncrease = batchElems * s.timePerBatchElem
            //     val newAvg = (avg + elapsed) / 2
            //     val newBatchElemTime = s.timePerBatchElem + (elapsed)
            //     ???
            //     // m + (name -> Stats(name, s.count + 1, s.totalTime + elapsed))
            // }
            }
        }
      }
}
