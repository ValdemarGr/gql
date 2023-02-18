package gql.interpreter

import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import cats.data._

trait Scope[F[_]] {
  def id: Unique.Token

  def parent: Option[Scope[F]]

  def isOpen: F[Boolean]

  // Child order is reverse allocation order
  def children: F[List[Scope[F]]]

  def releaseChildren(children: NonEmptyList[Unique.Token]): F[Unit]

  // def leases: F[List[Lease[F]]]

  // def releaseLeases(leases: NonEmptyList[Unique.Token]): F[Unit]

  def openChild[A](r: Scope[F] => Resource[F, A]): F[Option[(Scope[F], A)]]

  // None if already closed
  def lease[A](r: Resource[F, A]): F[Option[A]]

  // None if already closed
  def child: F[Option[Scope[F]]]

  // Closes all children in parallel
  // Then closes all leases in parallel
  def close: F[Unit]

  def string: F[String]
}

object Scope {
  sealed trait State[F[_]]
  object State {
    final case class Closed[F[_]]() extends State[F]
    final case class Open[F[_]](
        leases: List[Lease[F]],
        children: List[Scope[F]]
    ) extends State[F]
  }

  def apply[F[_]](parent: Option[Scope[F]])(implicit F: Async[F]): Resource[F, Scope[F]] = {
    val parent0 = parent
    val stateF =
      Resource.make(F.ref[State[F]](State.Open(Nil, Nil)))(_.getAndSet(State.Closed()).flatMap {
        case State.Closed() => F.unit
        case State.Open(leases, children) =>
          children.parTraverse_(_.close) *> leases.parTraverse_(_.release)
      })

    stateF.evalMap { state =>
      F.unique.map { tok =>
        new Scope[F] { self =>
          override def id: Unique.Token = tok

          override def parent: Option[Scope[F]] = parent0

          override def children: F[List[Scope[F]]] = state.get.map {
            case State.Closed()          => Nil
            case State.Open(_, children) => children
          }

          override def releaseChildren(children: NonEmptyList[Unique.Token]): F[Unit] =
            state.modify {
              case State.Closed() => State.Closed() -> F.unit
              case State.Open(leases, xs) =>
                val asSet = children.toList.toSet
                val (toRelease, toKeep) = xs.partition(c => asSet.contains(c.id))
                State.Open(leases, toKeep) -> toRelease.parTraverse_(_.close)
            }.flatten

          override def openChild[A](r: Scope[F] => Resource[F, A]): F[Option[(Scope[F], A)]] =
            F.uncancelable { _ =>
              child.flatMap {
                case None    => F.pure(None)
                case Some(s) => s.lease(r(s)).map(_ tupleLeft s)
              }
            }

          // None if already closed
          override def lease[A](r: Resource[F, A]): F[Option[A]] =
            F.uncancelable { _ =>
              state.get.flatMap {
                case State.Closed() => F.pure(None)
                case State.Open(_, _) =>
                  F.unique.flatMap { leaseToken =>
                    r.allocated.flatMap { case (a, release) =>
                      state
                        .modify[F[Option[A]]] {
                          case State.Closed() => State.Closed() -> release.as(None)
                          case State.Open(leases, children) =>
                            val release0 = release
                            val lease = new Lease[F] {
                              override def scope: Scope[F] = self

                              override def id: Unique.Token = leaseToken

                              override def release: F[Unit] = release0
                            }
                            State.Open(lease :: leases, children) -> F.pure(Some(a))
                        }
                        .flatten
                    }
                  }
              }
            }

          // None if already closed
          override def child: F[Option[Scope[F]]] =
            Scope[F](Some(this)).allocated.flatMap { case (s, release) =>
              state
                .modify[F[Option[Scope[F]]]] {
                  case State.Closed()               => State.Closed() -> release.as(None)
                  case State.Open(leases, children) => State.Open(leases, s :: children) -> F.pure(Some(s))
                }
                .flatten
            }

          override def close: F[Unit] =
            state.modify {
              case State.Closed() => State.Closed() -> F.unit
              case State.Open(leases, children) =>
                State.Closed() -> {
                  children.parTraverse_(_.close) *> leases.parTraverse_(_.release)
                }
            }.flatten

          // override def closeInParent: F[Unit] = state.get.flatMap {
          //   case State.Closed()   => F.unit
          //   case State.Open(_, _) => parent.traverse_(_.releaseChildren(NonEmptyList.one(id)))
          // }

          override def isOpen: F[Boolean] = state.get.map {
            case State.Closed()   => false
            case State.Open(_, _) => true
          }

          // override def leases: F[List[Lease[F]]] = state.get.map {
          //   case State.Closed()        => Nil
          //   case State.Open(leases, _) => leases
          // }

          // override def releaseLeases(ids: NonEmptyList[Unique.Token]): F[Unit] = state.modify {
          //   case State.Closed() => State.Closed() -> F.unit
          //   case State.Open(leases, children) =>
          //     val asSet = ids.toList.toSet
          //     val (toRelease, toKeep) = leases.partition(l => asSet.contains(l.id))
          //     State.Open(toKeep, children) -> toRelease.parTraverse_(_.release)
          // }.flatten

          def string: F[String] = {
            def loop(s: Scope[F], indent: Int): F[Chain[String]] = {
              val pad = " " * indent + "|- "
              val hd = pad + s.id.toString()

              s.isOpen.flatMap {
                case false => F.pure(Chain(hd + " closed"))
                case true =>
                  s.children.flatMap { cs =>
                    Chain
                      .fromSeq(cs)
                      .flatTraverse { c =>
                        loop(c, indent + 2)
                      }
                      .map { strs =>
                        if (strs.size === 0) Chain(hd)
                        else Chain(hd, " " * indent + "|-+") ++ strs
                      }
                  }
              }
            }

            loop(self, 0).map(_.mkString_("\n"))
          }
        }
      }
    }
  }
}
