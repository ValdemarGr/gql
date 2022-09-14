package gql.out

import cats.effect._
import cats._
import cats.data._

final case class Interface[F[_], A](
    name: String,
    instances: List[Instance[F, A, Any]],
    fields: NonEmptyList[(String, Field[F, A, _, _])]
) extends ObjLike[F, A] {
  override def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Interface[G, A] =
    copy[G, A](
      instances = instances.map(_.mapK(fk)),
      fields = fields.map { case (k, v) => k -> v.mapK(fk) }
    )

  lazy val fieldsList = fields.toList

  lazy val fieldMap = fields.toNem.toSortedMap.toMap

  lazy val instanceMap = instances.map(x => x.ol.name -> x).toMap

  def contramap[B](g: B => A): Interface[F, B] =
    Interface(
      name,
      instances.map(_.contramap(g)),
      fields.map { case (k, v) => k -> v.contramap(g) }
    )
}
