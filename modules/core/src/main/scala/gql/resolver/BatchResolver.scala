package gql.resolver

import cats.implicits._
import cats._
import cats.data._

abstract case class BatchResolver[F[_], I, O](
    id: BatchResolver.ResolverKey,
    run: I => (Set[Any], Map[Any, Any] => O)
) extends Resolver[F, I, O] {
  override def mapK[G[_]: Functor](fk: F ~> G): BatchResolver[G, I, O] =
    new BatchResolver[G, I, O](id, run) {}

  override def contramap[B](g: B => I): BatchResolver[F, B, O] =
    new BatchResolver[F, B, O](id, b => run(g(b))) {}

  def mapBoth[O2](f: (I, O) => O2): BatchResolver[F, I, O2] =
    new BatchResolver[F, I, O2](
      id,
      { i =>
        val (keys, finalize) = run(i)
        (keys, finalize.andThen(f(i, _)))
      }
    ) {}
}

object BatchResolver {
  final case class ResolverKey(id: Int) extends AnyVal

  object ResolverKey {
    implicit val order: Order[ResolverKey] = Order.by(_.id)
  }

  def apply[F[_], K, T](
      f: Set[K] => F[Map[K, T]]
  ): State[gql.SchemaState[F], BatchResolver[F, Set[K], Map[K, T]]] =
    State { s =>
      val id = s.nextId
      val rk = ResolverKey(id)
      val r = new BatchResolver[F, Set[K], Map[K, T]](
        rk,
        k => (k.asInstanceOf[Set[Any]], (m: Map[Any, Any]) => m.asInstanceOf[Map[K, T]])
      ) {}
      val entry = f.asInstanceOf[Set[Any] => F[Map[Any, Any]]]
      (s.copy(nextId = id + 1, batchers = s.batchers + (rk -> entry)), r)
    }
}
