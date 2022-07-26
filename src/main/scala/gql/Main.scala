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

  implicit def listTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Vector[A]] = Output.Arr(of)

  implicit def optTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Option[A]] = Output.Opt(of)

  implicit def dataType[F[_]: Async]: Output.Obj[F, Data[F]] =
    outputObject[F, Data[F]](
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b),
      "c" -> effect(_.c.map(_.toVector))
    )

  implicit def otherDataType[F[_]: Async]: Output.Obj[F, OtherData[F]] =
    outputObject[F, OtherData[F]](
      "OtherData",
      "value" -> pure(_.value),
      "d1" -> effect(_.d1)
    )

  def satnh[F[_]: Async]: Output.Unification.Instance[F, Datas.Dat[F], Data[F]] = {
    instance(dataType[F]).contramap[Datas.Dat[F]](_.value)
  }

  implicit def datasType[F[_]: Async]: Output.Union[F, Datas[F]] =
    union[F, Datas[F]](
      "Datas",
      contra[Data[F]] { case Datas.Dat(d) => d },
      contra[OtherData[F]] { case Datas.Other(o) => o }
    )

  trait A {
    def a: String
  }
  object A {
    implicit def t[F[_]]: Output.Interface[F, A] =
      interface[F, A](
        outputObject(
          "A",
          "a" -> pure(_ => "A")
        ),
        contra[B] { case b: B => b },
        contra[C] { case c: C => c }
      )
  }

  trait D {
    def d: String
  }
  object D {
    implicit def t[F[_]]: Output.Interface[F, D] =
      interface[F, D](
        outputObject(
          "D",
          "d" -> pure(_ => "D")
        ),
        contra[C] { case c: C => c }
      )
  }

  final case class B(a: String) extends A
  object B {
    implicit def t[F[_]]: Output.Obj[F, B] = outputObject[F, B]("B", "a" -> pure(_ => "B"), "b" -> pure(_ => Option("BO")))
  }
  final case class C(a: String, d: String) extends A with D
  object C {
    implicit def t[F[_]]: Output.Obj[F, C] = outputObject[F, C]("C", "a" -> pure(_ => "C"), "d" -> pure(_ => "D"))
  }

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
  getInterface {
    ... F4
  }
}

fragment F4 on A {
  a
}

fragment F3 on A {
  ... on B {
    a
  }
  ... on C {
    a
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
      "getDatas" -> pure(_ => datasRoot[IO]),
      "getInterface" -> pure(_ => (C("hey", "tun"): A)),
      "getOther" -> pure(_ => (C("hey", "tun"): D))
    ),
    Map.empty
  )

  def parse = {
    val b1 = System.currentTimeMillis()
    val result =
      p.parseAll(qn).map { xs =>
        PreparedQuery.prepare(xs, schema, Map.empty)
      }
    println(System.currentTimeMillis() - b1)
    result
  }

  val result = parse

  def go =
    result match {
      case Left(e)        => println(errorMessage(qn, e))
      case Right(Left(x)) => println(x)
      case Right(Right(x)) =>
        implicit lazy val stats = Statistics[IO].unsafeRunSync()
        def showTree(indent: Int, nodes: NonEmptyList[Optimizer.Node]): String = {
          val pad = "  " * indent
          nodes
            .map { n =>
              val thisInfo = pad + s"name: ${n.name}, cost: ${n.cost.toInt}, start: ${n.start}, end: ${n.end}, id: ${n.id}\n"
              thisInfo + n.children.toNel.map(showTree(indent + 1, _)).mkString_("")
            }
            .mkString_("")
        }

        def showDiff(indent: Int, fa: NonEmptyList[Optimizer.Node], fb: NonEmptyList[Optimizer.Node]): String = {
          val pad = "  " * indent
          fa.sortBy(_.id)
            .zip(fb.sortBy(_.id))
            .map { case (a, b) =>
              val thisInfo =
                if (a.end.toInt != b.end.toInt) {
                  AnsiColor.GREEN_B + pad + s"name: ${a.name}, cost: ${a.cost.toInt}, start: ${a.start}, end: ${a.end}, id: ${a.id}" + AnsiColor.RESET + "\n" +
                    AnsiColor.RED_B + pad + s"name: ${b.name}, cost: ${b.cost.toInt}, start: ${b.start}, end: ${b.end}, id: ${b.id}" + AnsiColor.RESET + "\n"
                } else pad + s"name: ${a.name}, cost: ${a.cost.toInt}, start: ${a.start}, end: ${a.end}, id: ${a.id}\n"

              thisInfo + a.children.toNel.map(showDiff(indent + 1, _, b.children.toNel.get)).mkString_("")
            }
            .mkString_("")
        }

        def planCost(nodes: NonEmptyList[Optimizer.Node]): Double = {
          val fnt = Optimizer.flattenNodeTree(nodes)
          // val g =
          //   fnt.groupBy(_.name).map { case (k, nodes) =>
          //     k -> ((nodes.head.cost, nodes.head.elemCost, nodes.groupBy(_.start.toInt)))
          //   }

          fnt
            .groupBy(_.name)
            .toList
            .map { case (_, nodes) =>
              val c = nodes.head.cost
              val e = nodes.head.elemCost
              val costCnt = nodes.groupBy(_.start.toInt)
              val groups = costCnt.size
              val batched = nodes.size - groups
              groups * c + batched * e
            }
            .sumAll
        }

        val costTree = Optimizer.costTree[IO](x).unsafeRunSync()
        val p = Optimizer.plan(costTree)
        // println(showTree(0, costTree))
        // println(showTree(0, p))
        println(showDiff(0, p, costTree))
        println(s"inital plan cost: ${planCost(costTree)}")
        println(s"optimized plan cost: ${planCost(p)}")
        println(Interpreter.interpret[IO]((), x).unsafeRunSync())
    }

  go

  println(Render.renderSchema(schema))

  // {
  //   println("apache")
  //   val n =
  //     Statistics.NodeTypeRegression(
  //       sumx = 0d,
  //       sumxx = 0d,
  //       sumy = 0d,
  //       sumxy = 0d,
  //       n = 0,
  //       // averages
  //       xbar = 0d,
  //       ybar = 0d
  //     )

  //   val res = n
  //     .add(2 - 1, 2)
  //     .add(4 - 1, 3)
  //     .add(16 - 1, 10)
  //   println(s"${res.slope} * x + ${res.intercept}")

  //   // val res1 = res
  //   //   .remove(16 - 1, 10)
  //   // println(s"${res1.slope} * x + ${res1.intercept}")

  //   val res2 = n
  //     .add(2 - 1, 2)
  //     .add(4 - 1, 3)
  //   println(s"${res2.slope} * x + ${res2.intercept}")

  //   val res3 = res
  //     .add(10 - 1, 10)
  //   println(s"${res3.slope} * x + ${res3.intercept}")
  // }

  // {
  //   println("covariance variance")
  //   val n =
  //     Statistics.CovVarRegression(
  //       0,
  //       0d,
  //       0d,
  //       0d,
  //       0d
  //     )

  //   val res = n
  //     .add(2 - 1, 2)
  //     .add(4 - 1, 3)
  //     .add(16 - 1, 10)
  //   println("original")
  //   println(s"${res.slope} * x + ${res.intercept}")

  //   val res2 = n
  //     .add(2 - 1, 2)
  //     .add(4 - 1, 3)
  //   println(s"${res2.slope} * x + ${res2.intercept}")

  //   val res3 = res
  //     .add(10 - 1, 10)
  //   println(s"${res3.slope} * x + ${res3.intercept}")

  //   val res4 = res
  //     .scale(3)
  //     .add(10 - 1, 10)
  //     .scale(328)
  //   println("scaled 3")
  //   println(s"${res4.slope} * x + ${res4.intercept}")

  //   val res5 = res
  //     .scale(1000)
  //     .add(10 - 1, 10)
  //   println("scaled 10000")
  //   println(s"${res5.slope} * x + ${res5.intercept}")
  // }

  // {
  //   println("gradient")
  //   val b0 =
  //     NonEmptyList.of(
  //       Statistics.Point(2 - 1, 2),
  //       Statistics.Point(4 - 1, 3),
  //       Statistics.Point(16 - 1, 10)
  //     )
  //   val b1 = b0 concatNel b0
  //   val b2 = b1 concatNel b1
  //   val b3 = b2 concatNel b2
  //   val b4 = b3 concatNel b3
  //   val b5 = b4 concatNel b4
  //   val b6 = b5 concatNel b5
  //   val b7 = b6 concatNel b6
  //   val b8 = b7 concatNel b7
  //   val b9 = b8 concatNel b8
  //   val b10 = b9 concatNel b9
  //   val b11 = b10 concatNel b10
  //   val b12 = b11 concatNel b11
  //   val b13 = b12 concatNel b12
  //   val b14 = b13 concatNel b13
  //   val b15 = b14 concatNel b14
  //   val b16 = b15 concatNel b15
  //   println(b10.size)

  //   val n = Statistics.GradientDecentRegression.fit(b10)
  //   println(s"${n.slope} * x + ${n.intercept}")

  //   val n1 = n.add(10 - 1, 10)
  //   println(s"${n1.slope} * x + ${n1.intercept}")
  // }
}
