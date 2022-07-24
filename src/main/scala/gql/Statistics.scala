package gql

import cats.effect._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.Queue

trait Statistics[F[_]] {
  def getStatsOpt(name: String): F[Option[Statistics.Stats]]

  def getStats(name: String): F[Statistics.Stats]

  def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit]
}

object Statistics {
  /*
   * We have a sub-sequence of data points D = ((t_i, n_i), (t_i+1, n_i+1), ..., (t_k, n_k))
   * Where t is the elapsed time, n is the batch size i > 0 and k is the number of all points.
   *
   * From this sequence of data points we must guess how long an entry of n=1 takes, that is,
   * how long it takes to process a batch of size 1.
   * We must figure out the *additional* cost of n compared to n - 1.
   * Let the additional element cost be b.
   * B can be found by computing the standard deviation (that is, the average difference) of both t and n
   * Then we can divide \sigma(t) by \sigma(n) to get the "time per element"
   *
   * An example:
   * D = ((2, 2), (3, 4))
   * (2 - 3, 2 - 4) = (1, 2) = 1 / 2 = 0.5
   * avg(2, 3) = 2.5
   * delta_t = avgsum(2 - 2.5, 3 - 2.5) = (0.5 + 0.5) / 2 = 0.5
   * avg(2, 4) = 3
   * delta_n = avgsum(2 - 3, 4 - 3) = (1 + 1) / 2 = 1
   * b = delta_t / delta_n = 0.5
   *
   * D = ((2, 2), (3, 4), (10, 16))
   * avg(2, 3, 10) = 5
   * delta_t = avgsum(2 - 5, 3 - 5, 10 - 5) = (3 + 2 + 5) / 3 = 3 + 1/3
   * avg(2, 4, 16) = 24 / 3 = 8
   * delta_n = avgsum(2 - 8, 4 - 8, 16 - 8) = (6 + 4 + 8) / 3 = 18 / 3 = 6
   * b = delta_t / delta_n = (3.33 / 6) ~= 0.55555
   *
   * D = ((2, 2), (3, 4), (10, 16), (10, 20))
   * t_avg = avg(2, 3, 10, 10) = 5
   * delta_t = avgsum(|2 - 5|, |3 - 5|, |10 - 5|, |10 - 5|) = (3 + 2 + 5 + 5) / 4 = 15 / 4 = 3.75
   * n_avg = avg(2, 4, 16, 20) = 44 / 4 = 11
   * delta_n = avgsum(|2 - 11|, |4 - 11|, |16 - 11|, |20 - 11|) = (9 + 7 + 5 + 9) / 4 = 30 / 4 = 7.5
   * b = delta_t / delta_n = (3.75 / 7.5) ~= 0.5
   *
   * Linreg
   */
  final case class NodeTypeRegression(
      sumx: Double,
      sumxx: Double,
      sumy: Double,
      sumyy: Double,
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
        sumyy = sumyy - dy * dy * ratio,
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
        sumyy = sumyy + dy * dy * ratio,
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
