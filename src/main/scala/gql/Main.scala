package gql

import cats.data._
import cats.implicits._
import cats.effect._
import io.circe._
import cats._
import cats.arrow.FunctionK

sealed trait GQLType[F[_], A] {
  def contramap[B](f: B => A)(implicit F: Functor[F]): GQLType[F, B]

  def mapK[G[_]](fk: F ~> G): GQLType[G, A]
}

case class GQLObject[F[_], A](name: String, fields: Eval[NonEmptyList[GQLField[F, A, _]]]) extends GQLType[F, A] {
  def contramap[B](f: B => A)(implicit F: Functor[F]): GQLObject[F, B] = GQLObject(name, fields.map(_.map(_.contramap(f))))

  def mapK[G[_]](fk: F ~> G): GQLType[G, A] = GQLObject(name, fields.map(_.map(_.mapK(fk))))
}

case class GQLList[F[_], A](inner: GQLType[F, A]) extends GQLType[F, List[A]] {
  override def mapK[G[_]](fk: F ~> G): GQLType[G,List[A]] = ???

  override def contramap[B](f: B => List[A])(implicit F: Functor[F]): GQLType[F,B] = ???
}

case class GQLScalar[F[_], A](name: String, encoder: A => io.circe.Json) extends GQLType[F, A] {
  def contramap[B](f: B => A)(implicit F: Functor[F]): GQLScalar[F, B] = GQLScalar[F, B](name, x => encoder(f(x)))
  
  def mapK[G[_]](fk: F ~> G): GQLType[G, A] = GQLScalar(name, encoder)
}

case class GQLField[F[_], A, B](name: String, eval: A => F[B])(implicit val tpe: Eval[GQLType[F, B]]) {
  def contramap[C](g: C => A)(implicit F: Functor[F]) = GQLField[F, C, B](name, x => eval(g(x)))(tpe)

  def imap[C](f: B => C)(g: C => B)(implicit F: Functor[F]): GQLField[F, A, C] = GQLField[F, A, C](name, x => eval(x).map(f))(tpe.map(_.contramap(g)))

  def map[C](f: B => C)(implicit F: Functor[F], newType: GQLType[F, C]): GQLField[F, A, C] = GQLField[F, A, C](name, x => eval(x).map(f))(Eval.later(newType))

  def mapK[G[_]](fk: F ~> G) = GQLField[G, A, B](name, x => fk(eval(x)))(tpe.map(_.mapK(fk)))
}

object Main extends App {
  implicit def GQLTypeFkForId[F[_]](implicit F: Applicative[F]): Id ~> F = new FunctionK[Id, F] {
    def apply[A](a: A): F[A] = F.pure(a)
  }

  def scalar[A](name: String, encode: A => Json): GQLScalar[Id, A] = GQLScalar[Id, A](name, encode)

  case class PartiallyAppliedPureField[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](name: String, access: A => B)(implicit F: Applicative[F], tpe: GQLType[F, B]) = GQLField[F, A, B](name, x => F.pure(access(x)))(Eval.later(tpe))
  }

  def pureField[A] = PartiallyAppliedPureField[A]()

  case class PartiallyAppliedEffectfulField[A](val dummy: Boolean = true) extends AnyVal {
    def apply[F[_], B](name: String, access: A => F[B])(implicit tpe: GQLType[F, B]) = GQLField[F, A, B](name, access)(Eval.later(tpe))
  }

  def field[F[_], A] = PartiallyAppliedEffectfulField[A]()

  def gqlObject[F[_], A](
    name: String,
    xs: => NonEmptyList[GQLField[F, A, _]]
  ) = GQLObject(name, Eval.later(xs))

  final case class Data[F[_]](
    name: String,
    age: F[Int]
  )
  
  def getFriends[F[_]](name: String)(implicit F: Sync[F]): F[List[Data[F]]] =
    if (name == "John") F.delay(getData[F]("Jane")).map(List(_))
    else if (name == "Jane") F.delay(getData[F]("John")).map(List(_))
    else F.pure(Nil)

  def getData[F[_]](name: String)(implicit F: Sync[F]): Data[F] =
    Data[F](
      name,
      F.delay(if (name == "John") 22 else 20)
    )

  import io.circe.syntax._

  implicit def stringScalar[F[_]] = GQLScalar[F, String]("String", _.asJson)
  implicit def intScalar[F[_]] = GQLScalar[F, Int]("Int", _.asJson)
  implicit def listGQLType[F[_], A](implicit tpe: GQLType[F, A]): GQLType[F, List[A]] = GQLList(tpe)

  implicit def dataObject[F[_]](implicit F: Sync[F]): GQLObject[F, Data[F]] = 
    gqlObject[F, Data[F]](
      "Data",
      NonEmptyList.of(
        pureField[Data[F]]("name", _.name),
        field[F, Data[F]]("age", _.age),
        field[F, Data[F]]("friends", x => getFriends[F](x.name)),
      )
    )

  /*
  implicit lazy val stringEncoder: GQLEncoder[String] = new GQLEncoder[String] {
    def encode(a: String) = a.asJson
  }

  implicit lazy val intEncoder: GQLEncoder[Int] = new GQLEncoder[Int] {
    def encode(a: Int) = a.asJson
  }

  implicit def listEncoder[F[_], A](implicit enc: GQLEncoder[A]): GQLEncoder[List[A]] = new GQLEncoder[List[A]] {
    def encode(xs: List[A]) = xs.map(enc.encode).asJson
  }

  def dataGQLType[F[_]](implicit F: Sync[F]): GQLType =
    GQLObject(
      "Data",
      NonEmptyList.of(
        GQLField("name", GQLScalar("String")),
        GQLField("age", GQLScalar("Int")),
        GQLField("friends", GQLList(Eval.later(dataGQLType[F]))),
      )
    )



  implicit def encoder[F[_]: Sync]: GQLEncoder[Data[F]] = new GQLEncoder[Data[F]] {
    def encode(data: Data[F]) = 
      GQLObject(
        NonEmptyList.of(
          GQLField("name", ResolvePure(data.name)),
          GQLField("age", ResolveF(data.age)),
          GQLField("friends", ResolveF(getFriends[F](data.name))),
        )
      )
    }

  println(implicitly[GQLEncoder[Data[IO]]].encode(getData[IO]("John")))
  println(implicitly[GQLEncoder[Data[IO]]].gqlType)*/

  //println(dataGQLType[IO])
}
