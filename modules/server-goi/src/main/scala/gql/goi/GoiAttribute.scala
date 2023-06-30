package gql.goi

import gql.ast._
import gql.resolver._
import cats.data._

final case class GoiAttribute[F[_], A, B, Id](
    toId: Resolver[F, A, B],
    codec: IDCodec[Id],
    fromIds: NonEmptyList[Id] => F[Map[Id, A]]
) extends FieldAttribute[F, A, B]
