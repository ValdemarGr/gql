package gql.resolver

import cats.implicits._
import cats._
import cats.effect._
import cats.data._

abstract case class BatchResolver[F[_], I, O](
    id: BatchResolver.ResolverKey,
    run: I => F[IorNec[String, (Set[Any], Map[Any, Any] => F[IorNec[String, O]])]]
) extends Resolver[F, I, O] {
  override def mapK[F2[x] >: F[x], G[_]: Functor](fk: F2 ~> G): BatchResolver[G, I, O] =
    new BatchResolver[G, I, O](id, i => fk(run(i)).map(_.map { case (ks, f) => (ks, m => fk(f(m))) })) {}

  def contramapF[B](g: B => F[IorNec[String, I]])(implicit F: Monad[F]): BatchResolver[F, B, O] =
    new BatchResolver[F, B, O](id, b => IorT(g(b)).flatMap(i => IorT(run(i))).value) {}

  override def contramap[B](g: B => I): BatchResolver[F, B, O] =
    new BatchResolver[F, B, O](id, b => run(g(b))) {}

  def map[O2](f: (I, O) => O2)(implicit F: Functor[F]): BatchResolver[F, I, O2] =
    new BatchResolver[F, I, O2](id, i => run(i).map(_.map { case (ks, g) => (ks, g.andThen(_.map(_.map(o => f(i, o))))) })) {}
}

object BatchResolver {
  final case class ResolverKey(id: Int) extends AnyVal

  object ResolverKey {
    implicit val order: Order[ResolverKey] = Order.by(_.id)
  }

  def apply[F[_], K, T](
      f: Set[K] => F[Map[K, T]]
  )(implicit F: Monad[F]): State[gql.SchemaState[F], BatchResolver[F, Set[K], Map[K, T]]] =
    State { s =>
      val id = s.nextId
      val rk = ResolverKey(id)
      val r = new BatchResolver[F, Set[K], Map[K, T]](
        rk,
        k =>
          F.pure(
            (k.asInstanceOf[Set[Any]], (m: Map[Any, Any]) => F.pure(m.asInstanceOf[Map[K, T]].rightIor[NonEmptyChain[String]]))
              .rightIor[NonEmptyChain[String]]
          )
      ) {}
      val entry = f.asInstanceOf[Set[Any] => F[Map[Any, Any]]]
      (s.copy(nextId = id + 1, batchers = s.batchers + (rk -> entry)), r)
    }
}
