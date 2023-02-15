package gql.interpreter

import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import cats.data._

trait Scope[F[_]] {
  def id: Unique.Token

  def parent: Option[Scope[F]]

  // Child order is reverse allocation order
  def children: F[List[Scope[F]]]

  def releaseChildren(children: NonEmptyList[Unique.Token]): F[Unit]

  def openChild[A](r: Scope[F] => Resource[F, A]): F[Option[(Scope[F], A)]]

  def close: F[Unit]
}

object Scope {
  final case class Child[F[_]](scope: Scope[F], release: F[Unit])

  sealed trait State[F[_]]
  object State {
    final case class Closed[F[_]]() extends State[F]
    final case class Open[F[_]](children: List[Child[F]]) extends State[F]
  }

  def apply[F[_]](parent: Option[Scope[F]])(implicit F: Concurrent[F]): Resource[F, Scope[F]] = {
    val parent0 = parent
    val stateF =
      Resource.make(F.ref[State[F]](State.Open(Nil)))(_.getAndSet(State.Closed()).flatMap {
        case State.Closed()       => F.unit
        case State.Open(children) => children.parTraverse_(_.release)
      })

    stateF.evalMap { state =>
      F.unique.map { tok =>
        new Scope[F] { self =>
          override def id: Unique.Token = tok

          override def parent: Option[Scope[F]] = parent0

          override def children: F[List[Scope[F]]] = state.get.map {
            case State.Closed()       => Nil
            case State.Open(children) => children.map(_.scope)
          }

          override def releaseChildren(children: NonEmptyList[Unique.Token]): F[Unit] =
            state.modify {
              case State.Closed() => State.Closed() -> F.unit
              case State.Open(xs) =>
                val asSet = children.toList.toSet
                val (toRelease, toKeep) = xs.partition(c => asSet.contains(c.scope.id))
                State.Open(toKeep) -> toRelease.parTraverse_(_.release)
            }.flatten

          override def openChild[A](r: Scope[F] => Resource[F, A]): F[Option[(Scope[F], A)]] =
            F.uncancelable { _ =>
              state.get.flatMap {
                case State.Closed() => F.pure(None)
                case State.Open(_) =>
                  Scope[F](Some(self)).flatMap(s => r(s) tupleLeft s).allocated.flatMap { case ((s, a), release) =>
                    state
                      .modify[F[Option[(Scope[F], A)]]] {
                        case State.Closed()       => State.Closed() -> release.as(None)
                        case State.Open(children) => State.Open(Child(s, release) :: children) -> F.pure(Some(s, a))
                      }
                      .flatten
                  }
              }
            }

          override def close: F[Unit] =
            parent0.traverse_(_.releaseChildren(NonEmptyList.one(id)))
        }
      }
    }
  }
}
