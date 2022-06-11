package gql

import cats.data._
import cats.implicits._
import cats.effect._
import io.circe._
import cats._
import cats.arrow.FunctionK

sealed trait GQLOutputType[F[_], A]

sealed trait GQLToplevelOutputType[F[_]]

sealed trait GQLInputType[A]

object syntax {
  def outputObject[F[_], A](
      name: String,
      hd: (String, GQLField[F, A, _]),
      tl: (String, GQLField[F, A, _])*
  ) = GQLOutputObjectType[F, A](name, NonEmptyList(hd, tl.toList))

  def effect[F[_], I, T](resolver: I => F[T])(implicit tpe: => GQLOutputType[F, T]): GQLField[F, I, T] =
    GQLSimpleField[F, I, T](resolver andThen (fa => DeferredResolution(fa)), Eval.later(tpe))

  def pure[F[_], I, T](resolver: I => T)(implicit tpe: => GQLOutputType[F, T]): GQLField[F, I, T] =
    GQLSimpleField[F, I, T](resolver andThen (fa => PureResolution(fa)), Eval.later(tpe))
}

final case class GQLOutputObjectType[F[_], A](
    name: String,
    fields: NonEmptyList[(String, GQLField[F, A, _])]
) extends GQLOutputType[F, A]
    with GQLToplevelOutputType[F] {
  def contramap[B](g: B => A): GQLOutputObjectType[F, B] =
    GQLOutputObjectType(name, fields.map { case (k, v) => k -> v.contramap(g) })
}

final case class GQLOutputListType[F[_], A](of: GQLOutputType[F, A]) extends GQLOutputType[F, List[A]]

final case class GQLOutputUnionType[F[_], A](
    name: String,
    types: NonEmptyList[GQLOutputObjectType[F, A]]
) extends GQLOutputType[F, A]
    with GQLToplevelOutputType[F]

final case class GQLOutputScalarType[F[_], A](name: String, encoder: Encoder[A]) extends GQLOutputType[F, A] with GQLToplevelOutputType[F] {
  def contramap[B](g: B => A): GQLOutputScalarType[F, B] =
    GQLOutputScalarType(name, encoder.contramap(g))
}

final case class GQLInputScalarType[A](name: String, decoder: Decoder[A]) extends GQLInputType[A]

sealed trait Resolution[F[_], A]
final case class PureResolution[F[_], A](value: A) extends Resolution[F, A]
final case class DeferredResolution[F[_], A](f: F[A]) extends Resolution[F, A]

sealed trait GQLField[F[_], I, T] {
  def graphqlType: Eval[GQLOutputType[F, T]]

  def contramap[B](g: B => I): GQLField[F, B, T]
}

final case class GQLArgsType[A](
    entries: NonEmptyList[(String, GQLInputType[_])]
)

final case class GQLArgField[F[_], I, A, T](
    args: GQLArgsType[A],
    resolve: (I, A) => Resolution[F, T],
    graphqlType: Eval[GQLOutputType[F, T]]
) extends GQLField[F, I, T] {
  def contramap[B](g: B => I): GQLField[F, B, T] =
    GQLArgField(args, (b, a) => resolve(g(b), a), graphqlType)
}

final case class GQLSimpleField[F[_], I, T](
    resolve: I => Resolution[F, T],
    graphqlType: Eval[GQLOutputType[F, T]]
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

  implicit def intType[F[_]] = GQLOutputScalarType[F, Int]("Int", Encoder.encodeInt)

  implicit def stringType[F[_]] = GQLOutputScalarType[F, String]("String", Encoder.encodeString)

  implicit def listTypeForSome[F[_], A](implicit of: GQLOutputType[F, A]) = GQLOutputListType(of)

  import syntax._
  implicit def dataType[F[_]]: GQLOutputObjectType[F, Data[F]] =
    outputObject[F, Data[F]](
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b),
      "c" -> effect(_.c)
    )

  def root[F[_]: Sync] = getData[F]("John")

  def renderType[F[_]](tpe: GQLOutputType[F, _]): String = tpe match {
    case GQLOutputObjectType(name, _) => name
    case GQLOutputListType(of)        => s"[${renderType(of)}]"
    case GQLOutputUnionType(name, _)  => name
    case GQLOutputScalarType(name, _) => name
  }

  def render[F[_], A](root: GQLOutputType[F, A], accum: List[String], encountered: Set[String]): (List[String], Set[String]) =
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

  val q = """
query FragmentTyping {
  profiles(handles: ["zuck", "cocacola"]) {
    handle
    ...userFragment
    ...pageFragment
  }
}

fragment userFragment on User {
  friends {
    count
  }
}

fragment pageFragment on Page {
  likers {
    count
  }
}
  """

  val q2 = """
query withNestedFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: 10) {
      ...friendFields
    }
  }
}

fragment friendFields on User {
  id
  name
  ...standardProfilePic
}

fragment standardProfilePic on User {
  profilePic(size: 50)
}
  """

  val q3 = """
query inlineFragmentTyping {
  profiles(handles: ["zuck", "cocacola"]) {
    handle
    ... on User {
      friends {
        count
      }
    }
    ... on Page {
      likers {
        count
      }
    }
  }
}
  """

  val q4 = """
query inlineFragmentNoType($expandedInfo: Boolean) {
  user(handle: "zuck") {
    id
    name
    ... @include(if: $expandedInfo) {
      firstName
      lastName
      birthday
    }
  }
}
  """

  val tq = "\"\"\""
  val q5 = s"""
mutation {
  sendEmail(message: $tq
    Hello,
      World!

    Yours,
      GraphQL.
  $tq)
}
"""

  val q6 = s"""
query {
  user(id: 4) {
    id
    name
    smallPic: profilePic(size: 64)
    bigPic: profilePic(size: 1024)
  }
}
"""

  val p = GQLParser.executableDefinition.rep
  def tryParse[A](p: cats.parse.Parser[A], q: String): Unit =
    p.parseAll(q) match {
      case Left(e) =>
        val (left, right) = q.splitAt(e.failedAtOffset)
        val conflict = s"<<${q(e.failedAtOffset)}>>"
        val chunk = s"${left.takeRight(40)}$conflict${right.drop(1).take(40)}"
        // val chunk = q.drop(math.min(e.failedAtOffset - 10, 0)).take(20)
        val c = q(e.failedAtOffset)
        println(s"failed with char $c at offset ${e.failedAtOffset} with code ${c.toInt}: ${e.expected}")
        println(chunk)
      case Right(x) => println(x)
    }

  tryParse(p, q)
  tryParse(p, q2)
  tryParse(p, q3)
  tryParse(p, q4)
  tryParse(p, q5)
  tryParse(p, q6)
}
