package gql.goi

import gql.ast._
import cats.effect._
import gql.resolver._
import cats.data.NonEmptyList

object dsl {
  implicit class GlobalIDOps[F[_], A, K](val gid: GlobalID[F, A, K]) extends AnyVal {
    def tpe(
        hd: (String, Field[F, A, ?, ?]),
        tl: (String, Field[F, A, ?, ?])*
    )(implicit F: Sync[F]) = {
      implicit val codec = gid.codec
      Goi.addId[F, A, K](gid.toId, gql.dsl.tpe[F, A](gid.typename, hd, tl: _*))
    }

    def interface(
        hd: (String, AbstractField[F, ?, ?]),
        tl: (String, AbstractField[F, ?, ?])*
    )(implicit F: Sync[F]): Interface[F, A] = {
      implicit val codec = gid.codec
      Goi.addId[F, A, K](gid.toId, gql.dsl.interface[F, A](gid.typename, hd, tl: _*))
    }

    def interface(
        xs: NonEmptyList[(String, AbstractField[F, ?, ?])]
    )(implicit F: Sync[F]): Interface[F, A] =
      interface(xs.head, xs.tail: _*)

    def interfaceFrom(
        xs: NonEmptyList[(String, Field[F, A, ?, ?])]
    )(implicit F: Sync[F]): Interface[F, A] = {
      val fixed = xs.map { case (k, v) => k -> v.asAbstract }
      interface(fixed.head, fixed.tail: _*)
    }

    def interfaceFrom(
        hd: (String, Field[F, A, ?, ?]),
        tl: (String, Field[F, A, ?, ?])*
    )(implicit F: Sync[F]): Interface[F, A] = interfaceFrom(NonEmptyList(hd, tl.toList))
  }

  def gid[F[_], T, A](typename: String, toId: T => A, fromId: A => F[Option[T]])(implicit codec: IDCodec[A]): GlobalID[F, T, A] =
    GlobalID(typename, PureResolver(toId), fromId)

  def gidFrom[F[_], T, A](typename: String, toId: Resolver[F, T, A], fromId: A => F[Option[T]])(implicit
      codec: IDCodec[A]
  ): GlobalID[F, T, A] =
    GlobalID(typename, toId, fromId)

  // def instanceFrom[F[_]: Functor, A](ot: ObjectLike[F, A])(
  //     f: Array[String] => F[Either[String, Option[A]]]
  // ): (String, Array[String] => F[Either[String, Option[?]]]) =
  //   (ot.name, f.map(_.map(_.map(x => x))))

  // final case class PartiallyAppliedInstance[B](private val dummy: Boolean = false) extends AnyVal {
  //   def apply[F[_], A](ot: ObjectLike[F, A])(f: B => F[Option[A]])(implicit
  //       idCodec: IDCodec[B],
  //       F: Monad[F]
  //   ): (String, Array[String] => F[Either[String, Option[?]]]) =
  //     instanceFrom[F, A](ot)(id => F.pure(Goi.decodeInput[B](idCodec, id).toEither.leftMap(_.mkString_("\n"))).flatMap(_.traverse(f)))
  // }

  // def instance[B]: PartiallyAppliedInstance[B] = PartiallyAppliedInstance[B]()
}
