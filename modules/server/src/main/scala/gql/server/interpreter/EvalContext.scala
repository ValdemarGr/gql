package gql.server.interpreter

import cats.implicits._
import cats.effect._
import gql.Cursor

final case class EvalNode[F[_], +A](
    cursor: Cursor,
    value: A,
    parentLeases: Set[Unique.Token],
    parentLease: Resource[F, Option[Int]],
    interruptContext: F[Unit]
) {
  def map[B](f: A => B): EvalNode[F, B] = copy(value = f(value))

  def setValue[B](value: B): EvalNode[F, B] = copy(value = value)

  def addParentPath(token: Unique.Token): EvalNode[F, A] =
    copy(parentLeases = parentLeases + token)

  def setParentLease(lease: Resource[F, Option[Int]]): EvalNode[F, A] =
    copy(parentLease = lease)

  def setInterruptContext(context: F[Unit]): EvalNode[F, A] =
    copy(interruptContext = context)

  def modify(f: Cursor => Cursor): EvalNode[F, A] = copy(cursor = f(cursor))

  def succeed[B](value: B, f: Cursor => Cursor): EvalNode[F, B] =
    copy(cursor = f(cursor), value)

  def succeed[B](value: B): EvalNode[F, B] = succeed(value, identity)
}

object EvalNode {
  def empty[F[_], A](value: A)(implicit F: Async[F]) =
    EvalNode[F, A](Cursor.empty, value, Set.empty, Resource.pure(Some(1)), F.never)
}

final case class EvalState[F[_]](
    // None if we are currently not consuming
    values: Option[List[EvalState.Entry[F, ?]]],
    ps: EvalState.ProduceConsume[F]
)

object EvalState {
  final case class Entry[F[_], A](
      // id of the node in the tree
      token: Unique.Token,
      // the continuation of the node
      cont: Continuation[F, A],
      a: EvalNode[F, A]
  )

  final case class ProduceConsume[F[_]](
      consumed: F[Unit],
      notifyConsumed: F[Unit],
      produced: F[Unit],
      notifyProduced: F[Unit]
  )
  object ProduceConsume {
    def make[F[_]](implicit F: Async[F]): F[ProduceConsume[F]] =
      for {
        consumedD <- F.deferred[Unit]
        producedD <- F.deferred[Unit]
      } yield ProduceConsume(consumedD.get, consumedD.complete(()).void, producedD.get, producedD.complete(()).void)
  }

  final case class EvalStateApi[F[_]](
      ref: Ref[F, EvalState[F]],
      ps: ProduceConsume[F]
  )
  def init[F[_]](implicit F: Async[F]): F[EvalStateApi[F]] =
    for {
      ps <- ProduceConsume.make[F]
      ref <- F.ref(EvalState[F](Some(Nil), ps))
    } yield EvalStateApi(ref, ps)
}
