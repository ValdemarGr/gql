package gql

import cats._
import cats.effect._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.Queue
import cats.data.NonEmptyList
import scala.annotation.tailrec

trait Statistics[F[_]] { self =>
  def getSnapshot: F[String => F[Option[Statistics.Stats]]]

  def getStatsOpt(name: String): F[Option[Statistics.Stats]]

  def getStats(name: String): F[Statistics.Stats]

  def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit]

  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): Statistics[G] =
    new Statistics[G] {
      def getSnapshot: G[String => G[Option[Statistics.Stats]]] =
        f(self.getSnapshot.map(_.andThen(f.apply)))

      def getStatsOpt(name: String): G[Option[Statistics.Stats]] =
        f(self.getStatsOpt(name))

      def getStats(name: String): G[Statistics.Stats] =
        f(self.getStats(name))

      def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): G[Unit] =
        f(self.updateStats(name, elapsed, batchElems))
    }
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

    def scale(newN: Long): CovVarRegression = {
      val factor = newN.toDouble / count.toDouble
      copy(
        varX = varX * math.pow(factor, 2d),
        covXY = covXY * math.pow(factor, 2d),
        count = newN
      )
    }
  }

  final case class Point(x: Double, y: Double)

  final case class Stats(
      initialCost: Double,
      extraElementCost: Double
  )

  def apply[F[_]](implicit F: Async[F]): F[Statistics[F]] =
    F.ref(Map.empty[String, Ref[F, Either[NonEmptyList[Point], CovVarRegression]]])
      .map { state =>
        new Statistics[F] {
          override def getSnapshot: F[String => F[Option[Stats]]] =
            state.get.map(m =>
              s =>
                m.get(s)
                  .flatTraverse(_.get.map(_.toOption.map { reg =>
                    if (reg.varX == 0d || reg.covXY == 0d) Stats(initialCost = reg.meanY, extraElementCost = 0d)
                    else Stats(initialCost = reg.intercept, extraElementCost = reg.slope)
                  }))
            )

          override def getStatsOpt(name: String): F[Option[Stats]] =
            getSnapshot.flatMap(_(name))

          override def getStats(name: String): F[Stats] =
            getStatsOpt(name)
              .flatMap {
                case Some(x) => F.pure(x)
                case None    => F.raiseError(new Exception(s"stats not found for $name"))
              }

          override def updateStats(name: String, elapsed: FiniteDuration, batchElems: Int): F[Unit] = {
            val elapsedNorm = math.max(1L, elapsed.toMicros)
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
                            if (newPoints.size > 3) {
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
