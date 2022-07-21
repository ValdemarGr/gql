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
   * Let the additional element cost be b such that for n'=1 then t' = avg((t_i, t_i+1, ..., t_k)).
   *
   * An example:
   * D = ((2, 2), (3, 4))
   * Such that b=0.5 and t'=1.5 such that 
   * t_1 = t' + (n_1 - 1) * b = 1.5 + (2 - 1) * 0.5 = 1.5 + 1 * 0.5 = 2
   * t_2 = t' + (4 - 1) * b = 1.5 + 3 * 0.5 = 1.5 + 1.5 = 3
   * n_3 = 7
   * t_3 = 1.5 + 6 * 0.5 = 1.5 + 3 = 4.5
   *
   * That is we must solve for b and average all bs:
   * b_i = |t_i / (n_i - 1) - t'|
   * b_1 = |2 / (2 - 1) - t'| = |2/1 - 1.5| = 0.5
   * b_2 = |3 / 3 - 1.5| = 0.5
   * b = avg((b_1, b_2)) = 0.5
   */
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
