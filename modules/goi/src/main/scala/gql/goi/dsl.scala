package gql.goi

import cats._
import cats.implicits._
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

  def instanceFrom[F[_]: Functor, A](ot: ObjectLike[F, A])(
      f: String => F[Either[String, Option[A]]]
  ): (String, String => F[Either[String, Option[?]]]) =
    (ot.name, f.map(_.map(_.map(x => x))))

  final case class PartiallyAppliedInstance[B](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](ot: ObjectLike[F, A])(f: B => F[Option[A]])(implicit
        idCodec: IDCodec[B],
        F: Monad[F]
    ): (String, String => F[Either[String, Option[?]]]) =
      instanceFrom[F, A](ot)(id => F.pure(Goi.decodeString[B](idCodec, id).toEither.leftMap(_.mkString_("\n"))).flatMap(_.traverse(f)))
  }

  def instance[B]: PartiallyAppliedInstance[B] = PartiallyAppliedInstance[B]()
}
