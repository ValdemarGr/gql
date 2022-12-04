package gql.goi

import gql.ast._
import cats.effect._
import scala.reflect.ClassTag
import gql.resolver._

object dsl {
  implicit class TypeGOIOps[F[_], A](val t: Type[F, A]) extends AnyVal {
    def goi(resolve: Resolver[F, A, String])(implicit F: Sync[F], ct: ClassTag[A]): Type[F, A] =
      Goi.addId[F, A](resolve, t)

    def goi(f: A => String)(implicit F: Sync[F], ct: ClassTag[A]): Type[F, A] =
      Goi.addId[F, A](PureResolver(f), t)
  }

  implicit class InterfaceGOIOps[F[_], A](val t: Interface[F, A]) extends AnyVal {
    def goi(resolve: Resolver[F, A, String])(implicit F: Sync[F], ct: ClassTag[A]): Interface[F, A] =
      Goi.addId[F, A](resolve, t)

    def goi(f: A => String)(implicit F: Sync[F], ct: ClassTag[A]): Interface[F, A] =
      Goi.addId[F, A](PureResolver(f), t)
  }
}
