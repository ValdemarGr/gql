package gql.goi

import gql.ast._
import cats.data._

final case class GoiAttribute[F[_], A, Id](
    codec: IDCodec[Id],
    fromIds: NonEmptyList[Id] => F[Map[Id, A]]
) extends FieldAttribute[F, A, Id]
