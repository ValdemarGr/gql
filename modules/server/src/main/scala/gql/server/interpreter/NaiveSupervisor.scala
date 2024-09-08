package gql.server.interpreter

import cats.effect.implicits._
import cats.effect._
import cats.implicits._
import cats.effect.std._

object NaiveSupervisor {
  sealed trait State[F[_]]
  object State {
    final case class Closed[F[_]]() extends State[F]
    final case class Open[F[_]](runningFibers: Map[Unique.Token, Fiber[F, Throwable, ?]]) extends State[F]
  }

  def make[F[_]](implicit F: Async[F]) = {
    Resource
      .make {
        F.ref[State[F]](State.Open[F](Map.empty))
      } { state =>
        state.get.flatMap {
          case State.Closed() => F.raiseError(new RuntimeException("impossible"))
          case State.Open(runningFibers) =>
            runningFibers.values.toList.parTraverse_(_.cancel)
        }
      }
      .map { state =>
        new Supervisor[F] {
          override def supervise[A](fa: F[A]): F[Fiber[F, Throwable, A]] =
            F.uncancelable { _ =>
              def closed[B] = F.raiseError[B](new RuntimeException("supervisor closed"))
              state.get.flatMap {
                case State.Closed() => closed
                case State.Open(_) =>
                  F.unique.flatMap { tok =>
                    fa.start.flatMap { fib =>
                      val synthetic = new Fiber[F, Throwable, A] {
                        override def cancel: F[Unit] =
                          fib.cancel.guarantee(state.update {
                            case State.Closed()            => State.Closed()
                            case State.Open(runningFibers) => State.Open(runningFibers - tok)
                          })

                        override def join: F[Outcome[F, Throwable, A]] = fib.join
                      }

                      state
                        .modify {
                          case State.Closed() => (State.Closed(), fib.cancel *> closed)
                          case State.Open(runningFibers) =>
                            (State.Open(runningFibers + (tok -> synthetic)), F.unit)
                        }
                        .as(fib)
                    }
                  }
              }
            }
        }
      }
  }
}
