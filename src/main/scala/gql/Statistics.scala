package gql

import cats.effect._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.Queue
import cats.data.NonEmptyList
import scala.annotation.tailrec

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

    def add(x: Double, y: Double, errorImportance: Double = 1d / n.toDouble): GradientDecentRegression = {
      val m = slope
      val b = intercept

      val diff = (x * m + b) - y

      val err = error + errorImportance * math.pow(diff, 2d)

      val thisPDB = 2d * errorImportance * x * (b + m * x - y)
      val thisPDM = 2d * errorImportance * (b + m * x - y)

      val optB = b - 0.01d * thisPDB
      val optM = m - 0.01d * thisPDM

      GradientDecentRegression(err, n + 1, optM, optB)
    }
  }

  final case class Point(x: Double, y: Double)

  object GradientDecentRegression {
    //def fitPar(points: NonEmptyList[Point]): IO[GradientDecentRegression] = {
    //  // the error/cost function will be 1 / n * sum((y - (slope * x + intercept))^2)
    //  // we want to figure out what direction to move the slope and intercept to minimize this error
    //  // the error function of n^2 is a parabola
    //  // such that taking the partial derivative is the rate of error
    //  //
    //  // now we should just re-adjust the slope and intercept by applying their respective derived functions
    //  // and multiply by a learning rate
    //  //
    //  // since a function of a parabola is linear, re-adjusting by the rate of error will very quickly converge

    //  val n = points.size.toDouble

    //  def go(its: Int, strikes: Int, alpha: Double, m: Double, b: Double, prevErr: Double): IO[(Double, Double)] = {
    //    if (its == 100000 || strikes == 10) IO.pure((m, b))
    //    else {
    //      def pred(x: Double) = m * x + b
    //      (
    //        IO(1d / n * points.map(p => pred(p.x) - p.y).sumAll).map(b - alpha * _),
    //        IO(1d / n * points.map(p => (pred(p.x) - p.y) * p.x).sumAll).map(m - alpha * _)
    //      ).parTupled.flatMap { case (optB, optM) =>
    //        val err = 1d / n * points.map(p => math.pow(p.y - (optM * p.x + optB), 2d)).sumAll
    //        if (err >= prevErr) go(its + 1, strikes + 1, alpha / 2d, m, b, prevErr)
    //        else go(its + 1, 0, alpha * 2d, optM, optB, err)
    //      }
    //    }
    //  }

    //  go(0, 0, 0.01d, 0d, 0d, Double.MaxValue).map { case (m, b) =>
    //    GradientDecentRegression(
    //      1d / n * points.map(p => math.pow(m * p.x + b - p.y, 2d)).sumAll,
    //      points.size,
    //      m,
    //      b
    //    )
    //  }
    //}

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

      @tailrec
      def go(its: Int, strikes: Int, alpha: Double, m: Double, b: Double, prevErr: Double): (Double, Double) = {
        if (its == 100000 || strikes == 10) (m, b)
        else {
          def pred(x: Double) = m * x + b
          val appliedPDB = 1d / n * points.map(p => pred(p.x) - p.y).sumAll
          val appliedPDM = 1d / n * points.map(p => (pred(p.x) - p.y) * p.x).sumAll
          val optB = b - alpha * appliedPDB
          val optM = m - alpha * appliedPDM
          val err = 1d / n * points.map(p => math.pow(p.y - (optM * p.x + optB), 2d)).sumAll
          if (err >= prevErr) go(its + 1, strikes + 1, alpha / 2d, m, b, prevErr)
          else go(its + 1, 0, alpha * 2d, optM, optB, err)
        }
      }

      val (m, b) = go(0, 0, 0.01d, 0d, 0d, Double.MaxValue)

      GradientDecentRegression(
        1d / n * points.map(p => math.pow(m * p.x + b - p.y, 2d)).sumAll,
        points.size,
        m,
        b
      )
    }
  }

  final case class Stats(
      initialCost: Double,
      extraElementCost: Double
  )

  def apply[F[_]](implicit F: Concurrent[F]): F[Statistics[F]] =
    F.ref(Map.empty[String, Ref[F, Either[NonEmptyList[Point], CovVarRegression]]])
      .map { state =>
        new Statistics[F] {
          override def getStatsOpt(name: String): F[Option[Stats]] =
            state.get.flatMap(
              _.get(name).flatTraverse(_.get.map(_.toOption.map(reg => Stats(initialCost = reg.intercept, extraElementCost = reg.slope))))
            )

          override def getStats(name: String): F[Stats] =
            getStatsOpt(name)
              .flatMap {
                case Some(x) => F.pure(x)
                case None    => F.raiseError(new Exception(s"stats not found for $name"))
              }

          override def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit] = {
            val elapsedNorm = elapsed.toMillis
            val asPoint = Point(batchElems - 1, elapsedNorm)

            F.ref[Either[NonEmptyList[Point], CovVarRegression]](Left(NonEmptyList.of(asPoint)))
              .flatMap { fallbackState =>
                state.modify { m =>
                  m.get(name) match {
                    case None => (m + (name -> fallbackState), F.unit)
                    case Some(s) =>
                      val subroutine =
                        s.update {
                          case Left(xs) =>
                            val newPoints = xs.append(asPoint)
                            if (newPoints.size > 5) {
                              Right {
                                xs.foldLeft(CovVarRegression(0L, 0d, 0d, 0d, 0d)) { case (reg, point) =>
                                  reg.add(point.x, point.y)
                                }
                              }
                            } else Left(newPoints)
                          case Right(reg) => Right(reg.add(asPoint.x, asPoint.y))
                        }

                      (m, subroutine)
                  }
                }
              }
              .flatten
          }
        }
      }
}
