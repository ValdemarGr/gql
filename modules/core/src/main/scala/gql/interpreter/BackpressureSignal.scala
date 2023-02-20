package gql.interpreter

import cats.effect._
import cats.implicits._
import cats.effect.std._

trait BackpressureSignal[F[_], A, B] {
  def unconsAll: F[A]

  def uncons: fs2.Stream[F, A] = fs2.Stream.repeatEval(unconsAll)

  def publish(b: B): F[Unit]
}

object BackpressureSignal {
  final case class State[F[_], A](
      pullDone: Deferred[F, Unit],
      pushDone: Deferred[F, Unit],
      value: A
  )

  def apply[F[_], A, B](
      initial: A,
      tryPublish: (A, B) => Option[A]
  )(implicit F: Concurrent[F]) = {
    Semaphore[F](1).flatMap { sem =>
      (F.deferred[Unit], F.deferred[Unit])
        .flatMapN((pull, push) => F.ref(State(pull, push, initial)))
        .map { state =>
          def publish0(b: B): F[Unit] =
            state.modify { current =>
              tryPublish(current.value, b) match {
                case None    => (current, current.pullDone.get >> publish0(b))
                case Some(a) => current.copy(value = a) -> current.pushDone.complete(()).void
              }
            }.flatten

          def unconsAll0: F[A] =
            sem.permit.surround {
              F.deferred[Unit].flatMap { newPullDone =>
                state.get.flatMap(_.pushDone.get) >>
                  state.modify { current =>
                    val fa = current.pullDone.complete(()).void
                    current.copy(pullDone = newPullDone, value = initial) -> fa.as(current.value)
                  }.flatten
              }
            }

          new BackpressureSignal[F, A, B] {
            def publish(b: B): F[Unit] = publish0(b)

            def unconsAll: F[A] = unconsAll0
          }
        }
    }
  }
}
