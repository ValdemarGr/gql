package gql

import scala.concurrent.duration._
import cats.data._
import cats.implicits._
import cats.effect._
import io.circe._
import cats._
import cats.arrow.FunctionK
import cats.effect.unsafe.implicits.global
import gql.GQLParser.Value.VariableValue
import gql.GQLParser.Value.FloatValue
import gql.GQLParser.Value.NullValue
import gql.GQLParser.Value.ObjectValue
import gql.GQLParser.Value.EnumValue
import gql.GQLParser.Value.BooleanValue
import gql.GQLParser.Value.IntValue
import gql.GQLParser.Value.ListValue
import gql.GQLParser.Value.StringValue
import conversions._
import cats.effect.std.Random

object Main extends App {
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

  // p.parseAll(q2).map { ed => }
  // tryParse(p, q)
  // tryParse(p, q2)
  // tryParse(p, q3)
  // tryParse(p, q4)
  // tryParse(p, q5)
  // tryParse(p, q6)

  final case class Data[F[_]](
      a: String,
      b: F[Int],
      c: F[Seq[Data[F]]]
  )

  final case class OtherData[F[_]](
      value: String,
      d1: F[Data[F]]
  )

  sealed trait Datas[F[_]]
  object Datas {
    final case class Other[F[_]](value: OtherData[F]) extends Datas[F]
    final case class Dat[F[_]](value: Data[F]) extends Datas[F]
  }

  def getFriends[F[_]](name: String)(implicit F: Sync[F]): F[Seq[Data[F]]] =
    if (name == "John") F.delay(getData[F]("Jane")).map(List(_))
    else if (name == "Jane") F.delay(getData[F]("John")).map(List(_))
    else F.pure(Nil)

  def getData[F[_]](name: String)(implicit F: Sync[F]): Data[F] =
    Data[F](
      name,
      F.delay(if (name == "John") 22 else 20),
      F.defer(getFriends[F](name))
    )
  import gql.syntax._
  implicit def intType[F[_]]: Output.Scalar[F, Int] = Output.Scalar("Int", Encoder.encodeInt)

  implicit def stringType[F[_]]: Output.Scalar[F, String] = Output.Scalar("String", Encoder.encodeString)

  implicit def listTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Seq[A]] = Output.Arr(of)

  implicit def dataType[F[_]: Async]: Output.Obj[F, Data[F]] =
    outputObject[F, Data[F]](
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b),
      "c" -> effect(_.c)
    )

  implicit def otherDataType[F[_]: Async]: Output.Obj[F, OtherData[F]] =
    outputObject[F, OtherData[F]](
      "OtherData",
      "value" -> pure(_.value),
      "d1" -> effect(_.d1)
    )

  implicit def datasType[F[_]: Async]: Output.Union[F, Datas[F]] =
    union[F, Datas[F]](
      "Datas",
      instance(dataType[F].contramap[Datas.Dat[F]](_.value)),
      instance(otherDataType[F].contramap[Datas.Other[F]](_.value))
    )

  def root[F[_]: Sync]: Data[F] = getData[F]("John")

  def datasRoot[F[_]: Async]: Datas[F] =
    Datas.Other(
      OtherData(
        "toplevel",
        Async[F].delay(getData[F]("Jane"))
      )
    )

  val q0 = """
query withNestedFragments {
  getData {
    ... on Data {
      a
      b
      c {
        ... DataFragment
      }
    }
  }
}

    fragment DataFragment on Data {
      a
      b
      c {
        ... NestedData
      }
    }

    fragment NestedData on Data {
      a
      b
      c {
        ... NestedData2
      }
    }

    fragment NestedData2 on Data {
      a
      b
    }
  """

  val qn = """
query withNestedFragments {
  getDatas {
    ... on OtherData {
      value
    }
  }
}
  """

  val schema = Schema[IO, Unit](
    outputObject[IO, Unit](
      "Query",
      "getData" -> pure(_ => root[IO]),
      "getDatas" -> pure(_ => datasRoot[IO])
    ),
  Map.empty
  )

  val result =
    p.parseAll(qn).map { xs =>
      PreparedQuery.prepare(xs, schema, Map.empty)
    }

  // println(
  //   otherDataType[IO].fields.head._2
  //     .asInstanceOf[Output.Fields.SimpleField[IO, Any, Any]]
  //     .resolve(
  //       OtherData(
  //         "toplevel",
  //         IO.delay(getData[IO]("Jane"))
  //       )
  //     )
  // )
  // println(
  //   datasType[IO].types.last.ol.fields.head._2
  //     .asInstanceOf[Output.Fields.SimpleField[IO, Any, Any]]
  //     .resolve(
  //       Datas.Other(
  //         OtherData(
  //           "toplevel",
  //           IO.delay(getData[IO]("Jane"))
  //         )
  //       )
  //     )
  // )
  // println(
  //   datasType[IO].types.last.specify(
  //     Datas.Other(
  //       OtherData(
  //         "toplevel",
  //         IO.delay(getData[IO]("Jane"))
  //       )
  //     )
  //   )
  // )
  // println(result)

  val x = result.toOption.get.toOption.get

  println(Interpreter.interpret[IO]((), x).unsafeRunSync())
}
