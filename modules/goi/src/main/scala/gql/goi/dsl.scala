package gql.goi

import gql.ast._
import cats.effect._
import gql.resolver._

object dsl {
  implicit class InterfaceGOIOps[F[_], A](val i: Interface[F, A]) {
    def goi: Interface[F, A] = Goi.addId[F, A](i)
  }

  implicit class GlobalIDOps[F[_], A, K](val gid: GlobalID[F, A, K]) extends AnyVal {
    def tpe(
        hd: (String, Field[F, A, ?, ?]),
        tl: (String, Field[F, A, ?, ?])*
    )(implicit F: Sync[F]) = {
      implicit val codec = gid.codec
      Goi.addId[F, A, K](gid.toId, gql.dsl.tpe[F, A](gid.typename, hd, tl: _*))
    }
  }

  def gid[F[_], T, A](typename: String, toId: T => A, fromId: A => F[Option[T]])(implicit codec: IDCodec[A]): GlobalID[F, T, A] =
    GlobalID(typename, PureResolver(toId), fromId)

  def gidFrom[F[_], T, A](typename: String, toId: Resolver[F, T, A], fromId: A => F[Option[T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] =
    GlobalID(typename, toId, fromId)
}
