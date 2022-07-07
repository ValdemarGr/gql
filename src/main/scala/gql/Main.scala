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
import cats.parse.Parser
import cats.parse.Parser.Expectation._
import scala.io.AnsiColor

object Main extends App {
  def showExpectation(e: Parser.Expectation): Eval[String] =
    Eval.defer {
      e match {
        case WithContext(contextStr, expect) =>
          showExpectation(expect).map(x => s"context:\n$contextStr\nwith underlying $x")
        case Fail(offset) =>
          Eval.now(s"failed at offset $offset")
        case FailWith(offset, message) =>
          Eval.now(s"failed at offset $offset with message $message")
        case OneOfStr(offset, strs) =>
          Eval.now(s"failed at offset $offset with one of ${strs.map(s => s"\"$s\"").mkString(" | ")}")
        case StartOfString(offset) =>
          Eval.now(s"failed at offset $offset with start of string")
        case Length(offset, expected, actual) =>
          Eval.now(s"failed at offset $offset with length $expected, but found $actual")
        case EndOfString(offset, length) =>
          Eval.now(s"failed at offset $offset with end of string but expected length $length")
        case InRange(offset, lower, upper) =>
          Eval.now(
            s"failed at offset $offset with char in range $lower to $upper (code ${lower.toInt} to ${upper.toInt})"
          )
        case ExpectedFailureAt(offset, matched) =>
          Eval.now(s"failed at offset $offset with expected failure at $matched")
      }
    }

  def showExpectations(es: NonEmptyList[cats.parse.Parser.Expectation]): String =
    es.traverse(showExpectation).value.mkString_("-" * 10)

  def errorMessage(data: String, e: Parser.Error) = {
    val (left, right) = data.splitAt(e.failedAtOffset)
    val c = data(e.failedAtOffset)
    val ln = left.count(_ == '\n') + 1

    val virtualErrorLineOffset = left.reverse.takeWhile(_ != '\n').length()
    val virtualN = 3
    // val virtualLineCharsLeft = math.min(virtualErrorLineOffset - 3, 0)
    val virtualLineStart = math.max(virtualErrorLineOffset - 3, 0)
    val virtualErrorLine: String =
      (">" * virtualLineStart) + ("^" * (virtualN * 2 + 1)) + s" line:$ln code:${c.toInt}"

    val green = AnsiColor.RESET + AnsiColor.GREEN

    val conflict = s"${data(e.failedAtOffset)}"
    val n = 400
    val leftChunk = left.takeRight(n)
    // val leftThisLine = leftChunk.takeRight(virtualErrorLineOffset)
    // val leftOtherLines = left.dropRight(n)

    val rightChunk = right.drop(1).take(n)
    val rightChunkThisline = rightChunk.takeWhile(_ != '\n')
    val rightChunkNextLines = rightChunk.dropWhile(_ != '\n')
    val rightChunks =
      green + rightChunkThisline + "\n" +
        AnsiColor.RED + virtualErrorLine +
        green + rightChunkNextLines + AnsiColor.RESET

    val conflictFmt = scala.io.AnsiColor.RED_B + scala.io.AnsiColor.BLACK + conflict

    val chunk =
      green + leftChunk + conflictFmt + rightChunks

    val msg =
      green + chunk
        .split("\n")
        .map(x => s"| $x")
        .mkString("\n")
    val niceError = showExpectations(e.expected)
    scala.io.AnsiColor.BLUE +
      s"failed with char $c at offset ${e.failedAtOffset} on line $ln with code ${c.toInt}: \n${niceError}\nfor data:\n$msg"
  }

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

  val qn = """
query withNestedFragments {
  getDatas {
    ... Frag
  }
  getData {
    ... F2
  }
}

fragment F2 on Data {
  a
  b
  c {
    a
    b
  }
}

  fragment Frag on Datas {
    ... on OtherData {
      value
      d1 {
        a
        b
        c {
          ... F2
        }
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

  result match {
    case Left(e)        => println(errorMessage(qn, e))
    case Right(Left(x)) => println(x)
    case Right(Right(x)) =>
      println(Interpreter.interpret[IO]((), x).unsafeRunSync())
  }
}
