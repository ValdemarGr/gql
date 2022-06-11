package gql

import cats.data._
import cats.implicits._
import cats.effect._
import io.circe._
import cats._
import cats.arrow.FunctionK

sealed trait GQLOutputType[A]

sealed trait GQLInputType[A]

object syntax {
  def outputObject[F[_], A](
      name: String,
      hd: (String, GQLField[F, A, _]),
      tl: (String, GQLField[F, A, _])*
  ) = GQLOutputObjectType[F, A](name, NonEmptyList(hd, tl.toList))

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => GQLOutputType[T]): GQLField[F, I, T] =
    GQLSimpleField[F, I, T](resolver andThen (fa => DeferredResolution(fa)), Eval.later(tpe))

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => GQLOutputType[T]): GQLField[F, I, T] =
    GQLSimpleField[F, I, T](resolver andThen (fa => PureResolution(fa)), Eval.later(tpe))
}

final case class GQLOutputObjectType[F[_], A](
    name: String,
    fields: NonEmptyList[(String, GQLField[F, A, _])]
) extends GQLOutputType[A] {
  def contramap[B](g: B => A): GQLOutputObjectType[F, B] =
    GQLOutputObjectType(name, fields.map { case (k, v) => k -> v.contramap(g) })
}

final case class GQLOutputListType[A](of: GQLOutputType[A]) extends GQLOutputType[List[A]]

final case class GQLOutputUnionType[F[_], A](
    name: String,
    types: NonEmptyList[GQLOutputObjectType[F, A]]
) extends GQLOutputType[A]

final case class GQLOutputScalarType[A](name: String, encoder: Encoder[A]) extends GQLOutputType[A] {
  def contramap[B](g: B => A): GQLOutputScalarType[B] =
    GQLOutputScalarType(name, encoder.contramap(g))
}

final case class GQLInputScalarType[A](name: String, decoder: Decoder[A]) extends GQLInputType[A]

sealed trait Resolution[F[_], A]
final case class PureResolution[F[_], A](value: A) extends Resolution[F, A]
final case class DeferredResolution[F[_], A](f: F[A]) extends Resolution[F, A]

sealed trait GQLField[F[_], I, T] {
  def graphqlType: Eval[GQLOutputType[T]]

  def contramap[B](g: B => I): GQLField[F, B, T]
}

final case class GQLArgField[F[_], I, A, T](
    args: GQLInputType[A],
    resolve: (I, A) => Resolution[F, T],
    graphqlType: Eval[GQLOutputType[T]]
) extends GQLField[F, I, T] {
  def contramap[B](g: B => I): GQLField[F, B, T] =
    GQLArgField(args, (b, a) => resolve(g(b), a), graphqlType)
}

final case class GQLSimpleField[F[_], I, T](
    resolve: I => Resolution[F, T],
    graphqlType: Eval[GQLOutputType[T]]
) extends GQLField[F, I, T] {
  def contramap[B](g: B => I): GQLField[F, B, T] =
    GQLSimpleField(g andThen resolve, graphqlType)
}

object Main extends App {
  final case class Data[F[_]](
      a: String,
      b: F[Int],
      c: F[List[Data[F]]]
  )

  def getFriends[F[_]](name: String)(implicit F: Sync[F]): F[List[Data[F]]] =
    if (name == "John") F.delay(getData[F]("Jane")).map(List(_))
    else if (name == "Jane") F.delay(getData[F]("John")).map(List(_))
    else F.pure(Nil)

  def getData[F[_]](name: String)(implicit F: Sync[F]): Data[F] =
    Data[F](
      name,
      F.delay(if (name == "John") 22 else 20),
      F.defer(getFriends[F](name))
    )

  implicit lazy val intType = GQLOutputScalarType("Int", Encoder.encodeInt)

  implicit lazy val stringType = GQLOutputScalarType("String", Encoder.encodeString)

  implicit def listTypeForSome[A](implicit of: GQLOutputType[A]) = GQLOutputListType(of)

  import syntax._
  implicit def dataType[F[_]]: GQLOutputObjectType[F, Data[F]] =
    outputObject[F, Data[F]](
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b),
      "c" -> effect(_.c)
    )

  def root[F[_]: Sync] = getData[F]("John")

  def renderType(tpe: GQLOutputType[_]): String = tpe match {
    case GQLOutputObjectType(name, _) => name
    case GQLOutputListType(of)        => s"[${renderType(of)}]"
    case GQLOutputUnionType(name, _)  => name
    case GQLOutputScalarType(name, _) => name
  }

  def render[A](root: GQLOutputType[A], accum: List[String], encountered: Set[String]): (List[String], Set[String]) =
    root match {
      case GQLOutputObjectType(name, fields) =>
        lazy val thisType =
          s"""
          type $name {
            ${fields
            .map { case (k, field) =>
              s"$k: ${renderType(field.graphqlType.value)}"
            }
            .mkString_(",\n")}
          }
          """
        if (encountered.contains(name)) (accum, encountered)
        else {
          val newEncountered = encountered + name
          val newAccum = accum :+ thisType
          val next = fields.filter { case (_, field) => !encountered.contains(renderType(field.graphqlType.value)) }
          next.foldLeft((newAccum, newEncountered)) { case ((accum, encountered), (_, field)) =>
            render(field.graphqlType.value, accum, encountered)
          }
        }
      case GQLOutputListType(of)           => render(of, accum, encountered)
      case GQLOutputUnionType(name, types) => ???
      case GQLOutputScalarType(name, _) =>
        (
          s"""
        scalar $name
        """ :: accum,
          encountered
        )
    }

  println(root[IO])
  println(dataType[IO])
  println(dataType[IO].fields)
  val (res, _) = render(dataType[IO], Nil, Set.empty)
  println(res.mkString("\n"))
}

/*
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
 */
