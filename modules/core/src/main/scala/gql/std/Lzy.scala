package gql.std

import cats._

final case class Lzy[A](eval: Eval[A]) extends AnyVal

object Lzy extends LowPriorityLzy {
  implicit def liftLazy[A](implicit a: => A): Lzy[A] = Lzy(Eval.later(a))
}
trait LowPriorityLzy {
  implicit def liftAnyValue[A](a: => A): Lzy[A] = Lzy(Eval.now(a))
}
