package gql

import cats.effect._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration

trait Statistics[F[_]] {
  def getStatsOpt(name: String): F[Option[Statistics.Stats]]

  def getStats(name: String): F[Statistics.Stats]

  def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit]
}

object Statistics {
  final case class Stats(
      name: String,
      count: Int,
      totalTime: FiniteDuration,
      timePerBatchElem: FiniteDuration
  ) {
    lazy val average: FiniteDuration = totalTime / count
  }

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
            state.update{ m =>
              m.get(name) match {
                case None => m + (name -> Stats(name, 1, elapsed, elapsed / batchElems))
                case Some(s) => 
                  val avg = s.average
                  val batchElemIncrease = batchElems * s.timePerBatchElem
                  val newAvg = (avg + elapsed) / 2
                  val newBatchElemTime = s.timePerBatchElem + (elapsed)
                  ???
                  // m + (name -> Stats(name, s.count + 1, s.totalTime + elapsed))
              }
            }
        }
      }
}
