package gql.std

import cats._

sealed abstract class FreeApply[F[_], +A] extends Product with Serializable {
  def foldMap[G[_], B >: A](fk: F ~> G)(implicit G: Applicative[G]): G[B] = this match {
    case FreeApply.Lift(fa)    => G.map(fk(fa))(x => x)
    case FreeApply.Ap(ff, fa)  => G.map(G.ap(ff.foldMap(fk))(fa.foldMap(fk)))(x => x)
    case FreeApply.Fmap(fa, f) => G.map(fa.foldMap(fk))(f)
  }
}

object FreeApply {
  final case class Ap[F[_], A, B](ff: FreeApply[F, A => B], fa: FreeApply[F, A]) extends FreeApply[F, B]

  final case class Lift[F[_], A](fa: F[A]) extends FreeApply[F, A]

  // Allows O(1) fmap
  final case class Fmap[F[_], A, B](fa: FreeApply[F, A], f: A => B) extends FreeApply[F, B]

  def lift[F[_], A](fa: F[A]): FreeApply[F, A] = Lift(fa)

  implicit def freeApply[F[_]]: Apply[FreeApply[F, *]] = {
    type G[A] = FreeApply[F, A]
    new Apply[G] {
      override def map[A, B](fa: G[A])(f: A => B): G[B] = Fmap(fa, f)
      override def ap[A, B](ff: G[A => B])(fa: G[A]): G[B] = Ap(ff, fa)
    }
  }
}
