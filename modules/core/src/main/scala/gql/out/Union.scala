package gql.out

import cats.effect._
import cats._
import cats.data._

final case class Union[F[_], A](
    name: String,
    types: NonEmptyMap[String, Instance[F, A, Any]]
) extends ObjLike[F, A] {
  override def contramap[B](f: B => A): Union[F, B] =
    Union(name, types.map(_.contramap(f)))

  lazy val instances = types.toSortedMap.toMap

  lazy val fieldMap = Map.empty

  lazy val fieldsList: List[(String, Field[F, A, _, _])] = Nil

  def mapK[G[_]: MonadCancelThrow](fk: F ~> G): Union[G, A] =
    Union(
      name,
      types.map(_.mapK(fk))
    )
}
