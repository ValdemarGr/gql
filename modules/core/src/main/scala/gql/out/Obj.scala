package gql.out

import cats.data._
import cats._
import cats.effect._

final case class Obj[F[_], A](
    name: String,
    fields: NonEmptyList[(String, Field[F, A, _, _])]
) extends ObjLike[F, A] {
  lazy val fieldsList: List[(String, Field[F, A, _, _])] = fields.toList

  override def contramap[B](f: B => A): Obj[F, B] =
    Obj(name, fields.map { case (k, v) => k -> v.contramap(f) })

  lazy val fieldMap = fields.toNem.toSortedMap.toMap

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Obj[G, A] =
    Obj(name, fields.map { case (k, v) => k -> v.mapK(fk) })
}
