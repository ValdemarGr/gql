package gql.goi

import cats._
import cats.implicits._
import gql.ast._
import cats.effect._
import scala.reflect.ClassTag
import gql.resolver._
import java.util.UUID
import gql.SchemaShape

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
    f: Array[String] => F[Either[String, Option[A]]]
  ): (String, Array[String] => F[Either[String, Option[?]]]) =
    (ot.name, f.map(_.map(_.map(x => x))))

  final case class PartiallyAppliedInstance[B](private val dummy: Boolean = false) extends AnyVal {
    def apply[F[_], A](ot: ObjectLike[F, A])(f: B => F[Option[A]])(implicit
      idCodec: IDCodec[B],
      F: Monad[F]
    ): (String, Array[String] => F[Either[String, Option[?]]]) =
      instanceFrom[F, A](ot)(id =>
        F.pure(Goi.decodeInput[B](idCodec, id).toEither.leftMap(_.mkString_("\n"))).flatMap(_.traverse(f))
      )
  }

  def instance[B]: PartiallyAppliedInstance[B] = PartiallyAppliedInstance[B]()

  def test[F[_]: cats.effect.Sync] = {
    import gql.dsl._
    case class Lol(org: String, id: UUID, a: String)
    case class LolKey(org: String, a: UUID)
    import gql.goi.codec._
    object LolKey {
      implicit val idCodec: IDCodec[LolKey] = (string, uuid).imapN(LolKey.apply)(x => (x.org, x.a))
    }
    val lol = tpe[F, Lol](
      "Lol",
      "a" -> pure(_.a)
    ).goi(x => LolKey(x.org, x.id))

    // ...

    def getlol(key: LolKey): F[Option[Lol]] = ???

    val ss: SchemaShape[F, Unit, Unit, Unit] = ???
    val ss2 = Goi.node(
      ss,
      instance[LolKey](lol)(key => getlol(key))
    )
  }
}
