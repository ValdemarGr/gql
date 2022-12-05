package gql.goi

import gql.ast._
import cats.effect._
import scala.reflect.ClassTag
import gql.resolver._

object dsl {
  implicit class TypeGOIOps[F[_], A](val t: Type[F, A]) extends AnyVal {
    def goi[B](resolve: Resolver[F, A, B])(implicit F: Sync[F], ct: ClassTag[A], idCodec: IDCodec[B]): Type[F, A] =
      Goi.addId[F, A, B](resolve, t)

    def goi[B](f: A => B)(implicit F: Sync[F], ct: ClassTag[A], idCodec: IDCodec[B]): Type[F, A] =
      Goi.addId[F, A, B](PureResolver(f), t)
  }

  implicit class InterfaceGOIOps[F[_], A](val t: Interface[F, A]) extends AnyVal {
    def goi[B](resolve: Resolver[F, A, B])(implicit F: Sync[F], ct: ClassTag[A], idCodec: IDCodec[B]): Interface[F, A] =
      Goi.addId[F, A, B](resolve, t)

    def goi[B](f: A => B)(implicit F: Sync[F], ct: ClassTag[A], idCodec: IDCodec[B]): Interface[F, A] =
      Goi.addId[F, A, B](PureResolver(f), t)
  }
}
