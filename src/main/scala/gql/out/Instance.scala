package gql.out

import cats.effect._
import cats._

final case class Instance[F[_], A, B](
    ol: ObjLike[F, B]
)(implicit val specify: SimplePrism[A, B]) {
  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Instance[G, A, B] =
    Instance(ol.mapK(fk))

  def contramap[C](g: C => A): Instance[F, C, B] =
    Instance[F, C, B](ol)(specify.contramap[C](g))
}
