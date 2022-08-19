package gql

import cats.effect.implicits._
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
import fetch.Fetch
import scala.concurrent.ExecutionContext
import fetch.Unfetch
import alleycats.Empty
import gql.resolver.BatchResolver
import gql.resolver.BatcherReference
import gql.resolver.StreamReference
import gql.resolver.SignalResolver
import gql.resolver.EffectResolver
import gql.resolver.PureResolver

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

  def showTree(indent: Int, nodes: NonEmptyList[Planner.Node]): String = {
    val pad = "  " * indent
    nodes
      .map { n =>
        val thisInfo =
          pad + s"name: ${n.name}, cost: ${n.cost.toInt}), start: ${n.start}, end: ${n.end}, id: ${n.id}\n"
        thisInfo + n.children.toNel.map(showTree(indent + 1, _)).mkString_("")
      }
      .mkString_("")
  }

  def showDiff_(fa: NonEmptyList[Planner.Node], fb: NonEmptyList[Planner.Node], maxEnd: Double): String = {
    fa.sortBy(_.id)
      .zip(fb.sortBy(_.id))
      .map { case (a, b) =>
        val per = math.max((maxEnd / 40d).toInt, 1)
        val thisInfo =
          if (a.end.toInt != b.end.toInt) {
            (" " * (b.start.toInt / per)) + AnsiColor.RED_B + s"name: ${b.name}, batchName: ${b.batchName}, cost: ${b.cost.toInt}, start: ${b.start}, end: ${b.end}, id: ${b.id}" + AnsiColor.RESET + "\n" +
              (" " * (b.start.toInt / per)) + AnsiColor.BLUE_B + (">" * ((a.start - b.start).toInt / per)) + AnsiColor.GREEN_B + s"name: ${a.name}, batchName: ${b.batchName}, cost: ${a.cost.toInt}, start: ${a.start}, end: ${a.end}, id: ${a.id}" + AnsiColor.RESET + "\n"
          } else
            (" " * (a.start.toInt / per)) + s"name: ${a.name}, batchName: ${b.batchName}, cost: ${a.cost.toInt}, start: ${a.start}, end: ${a.end}, id: ${a.id}\n"

        thisInfo + a.children.toNel.map(showDiff_(_, b.children.toNel.get, maxEnd)).mkString_("")
      }
      .mkString_("")
  }

  def showDiff(fa: NonEmptyList[Planner.Node], fb: NonEmptyList[Planner.Node]) = {
    val me = Planner.flattenNodeTree(fa).maximumBy(_.end).end
    AnsiColor.RED_B + "old field schedule" + AnsiColor.RESET + "\n" +
      AnsiColor.GREEN_B + "new field schedule" + AnsiColor.RESET + "\n" +
      AnsiColor.BLUE_B + "new field offset (deferral of execution)" + AnsiColor.RESET + "\n" +
      showDiff_(fa, fb, me)
  }

  def planCost(nodes: NonEmptyList[Planner.Node]): Double = {
    val fnt = Planner.flattenNodeTree(nodes)

    fnt
      .groupBy(_.name)
      .toList
      .collect { case (_, nodes) =>
        val c = nodes.head.cost
        val e = nodes.head.elemCost
        val costCnt = nodes.groupBy(_.start.toInt)
        val groups = costCnt.size
        val batched = nodes.size - groups
        groups * c + batched * e
      }
      .sumAll
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
        val c = q(e.failedAtOffset)
        println(s"failed with char $c at offset ${e.failedAtOffset} with code ${c.toInt}: ${e.expected}")
        println(chunk)
      case Right(x) => println(x)
    }

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

  import gql.syntax.out._
  import gql.syntax._
  implicit def intType[F[_]]: Output.Scalar[F, Int] = Output.Scalar("Int", Encoder.encodeInt)

  implicit def stringType[F[_]]: Output.Scalar[F, String] = Output.Scalar("String", Encoder.encodeString)

  // implicit def listTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Vector[A]] = Output.Arr(of)
  implicit def seqTypeForAny[F[_], A](implicit of: Output[F, A]): Output[F, Seq[A]] = Output.Arr(of)

  implicit def optTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Option[A]] = Output.Opt(of)

  implicit val intInput: Input.Scalar[Int] = Input.Scalar("Int", Decoder.decodeInt)

  implicit val stringInput: Input.Scalar[String] = Input.Scalar("String", Decoder.decodeString)

  implicit def listInputType[A](implicit tpe: Input[A]): Input[Vector[A]] = Input.Arr(tpe)

  def testSchemaShape[F[_]](implicit F: Async[F]): State[SchemaState[F], SchemaShape[F, Unit]] = {
    final case class IdentityData(value: Int, value2: String)

    final case class InputData(
        value: Int,
        val2: String
    )

    final case class ServerData(value: Int)

    final case class DataIds(ids: List[Int])

    /*
     * BatchReference[F, UUID, Case](getCaseEffeciently).flatMap{ caseBatchReference =>
     *   StreamReference[F, UUID, UUID](subscribeToCaseKafkaTopic).flatMap{ caseStreamReference =>
     *     ...
     *     "caseData" -> signal(_.caseId: T Intermediate, batch(caseBatchReference)(identity: Batch Key from T)){ c =>
     *       (caseStreamReference, c.caseId: Key)
     *     }
     *     ...
     *   }
     * }
     *
     */
    StreamReference[F, String, Unit](k =>
      Resource.pure(fs2.Stream(k).repeat.lift[F].metered((if (k == "John") 200 else 500).millis).as(()))
    ).flatMap { nameStreamReference =>
      BatcherReference[F, Int, ServerData](xs => F.pure(xs.map(x => x -> ServerData(x)).toMap)).map { serverDataBatcher =>
        implicit val inputDataType: Input[InputData] = in.obj[InputData](
          "InputData",
          (
            arg[Int]("value", Some(42)),
            arg[String]("val2")
          ).mapN(InputData.apply)
        )

        val valueArgs: Arg[(Int, String, Vector[String])] =
          (
            (
              arg[Int]("num", Some(42)),
              arg[Int]("num2", Some(9))
            ).mapN(_ + _),
            arg[String]("text"),
            arg[Vector[String]]("xs", Vector.empty.some)
          ).tupled

        val inputDataArg = arg[InputData]("input")

        implicit def identityDataType: Output.Obj[F, IdentityData] =
          obj[F, IdentityData](
            "IdentityData",
            "value" -> effect((valueArgs, inputDataArg).tupled) { case (x, ((y, z, hs), i)) =>
              F.pure(s"${x.value2} + $z - ${(x.value + y).toString()} - (${hs.mkString(",")}) - $i")
            }
          )

        implicit def serverData: Output.Obj[F, ServerData] =
          obj[F, ServerData](
            "ServerData",
            "value" -> pure(_.value)
          )

        // def makeCommonFields[F[_]](b: FieldBuilder[F, Data[F]]): Unit =
        //   b.arg(arg[Int]("num", Some(42)))(pur { case (i, _) => i.a })

        // implicit class LalaOps[A](a: A) {
        //   def ++(a2: A) = NonEmptyList.of(a, a2)
        // }

        implicit lazy val dataType: Output.Obj[F, Data[F]] =
          obj2[F, Data[F]]("Data") { f =>
            fields(
              "a" -> f(pur(_.a)),
              "a2" -> f(arg[Int]("num", Some(42)))(pur { case (i, _) => i.a }),
              "b" -> f(eff(_.b)),
              "sd" -> f(serverDataBatcher.traverse(_.b.map(i => Seq(i, i + 1, i * 2)))),
              "c" -> f(eff(_.c.map(_.toSeq))),
              "nestedSignal" ->
                f(nameStreamReference[F, Data[F], Data[F]](eff { case (i, _) => i.c.map(_.head) })(_ => F.unit)(i => F.pure(i.a))),
              "nestedSignal2" ->
                f(nameStreamReference[F, Data[F], Data[F]](eff { case (i, _) => i.c.map(_.head) })(_ => F.unit)(i => F.pure(i.a)))
            )
          }

        // implicit def dataType: Output.Obj[F, Data[F]] =
        //   obj[F, Data[F]](
        //     "Data",
        //     "a" -> field(pur(_.a)),
        //     "a2" -> argumented(arg[Int]("num", Some(42)))(pur { case (i, _) => i.a }),
        //     "b" -> field(eff(_.b)),
        //     "sd" -> field(serverDataBatcher.traverse(_.b.map(i => Seq(i, i + 1, i * 2)))),
        //     "c" -> field(eff(_.c.map(_.toSeq))),
        //     "nestedSignal" ->
        //       field(nameStreamReference[F, Data[F], Data[F]](eff { case (i, _) => i.c.map(_.head) })(_ => F.unit)(i => F.pure(i.a))),
        //     // TODO crashes
        //     "nestedSignal2" ->
        //       field(nameStreamReference[F, Data[F], Data[F]](eff { case (i, _) => i.c.map(_.head) })(_ => F.unit)(i => F.pure(i.a)))
        //     // "nestedSignal2" ->
        //     //   argumented(arg[Int]("num", Some(42)))(
        //     //     nameStreamReference[F, (Data[F], Int), Data[F]](eff { case ((i, _), _) => i.c.map(_.head) })(_ => F.unit) { case (i, _) =>
        //     //       F.pure(i.a)
        //     //     }
        //     //   )
        //   )

        implicit def otherDataType: Output.Obj[F, OtherData[F]] =
          obj[F, OtherData[F]](
            "OtherData",
            "value" -> pure(_.value),
            "d1" -> effect(_.d1)
          )

        implicit def datasType: Output.Union[F, Datas[F]] =
          union[F, Datas[F]](
            "Datas",
            contra[Data[F]] { case Datas.Dat(d) => d },
            contra[OtherData[F]] { case Datas.Other(o) => o }
          )

        trait A {
          def a: String
        }
        object A {
          implicit def t: Output.Interface[F, A] =
            interface[F, A](
              obj(
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
          implicit def t: Output.Interface[F, D] =
            interface[F, D](
              obj(
                "D",
                "d" -> pure(_ => "D")
              ),
              contra[C] { case c: C => c }
            )
        }

        final case class B(a: String) extends A
        object B {
          implicit def t: Output.Obj[F, B] = obj[F, B]("B", "a" -> pure(_ => "B"), "b" -> pure(_ => Option("BO")))
        }
        final case class C(a: String, d: String) extends A with D
        object C {
          implicit def t: Output.Obj[F, C] = obj[F, C]("C", "a" -> pure(_ => "C"), "d" -> pure(_ => "D"))
        }

        SchemaShape[F, Unit](
          obj[F, Unit](
            "Query",
            "getData" -> pure(_ => root[F]),
            "getDatas" -> pure(_ => datasRoot[F]),
            "getInterface" -> pure(_ => (C("hey", "tun"): A)),
            "getOther" -> pure(_ => (C("hey", "tun"): D)),
            "doIdentity" -> pure(_ => IdentityData(2, "hello"))
          )
        )
      }
    }
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
  sd {
    value
  }
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
        sd {
          value
        }
        c {
          ... F2
        }
      }
    }
  }
  """

  val schema = Schema.stateful[IO, Unit](testSchemaShape[IO])

  def parseAndPrep(q: String): Option[NonEmptyList[PreparedQuery.PreparedField[IO, Any]]] =
    p.parseAll(q).map(PreparedQuery.prepare(_, schema, Map.empty)) match {
      case Left(e) =>
        println(errorMessage(q, e))
        None
      case Right(Left(x)) =>
        println(x)
        None
      case Right(Right(x)) => Some(x)
    }

  {
    def go =
      parseAndPrep(qn).map { x =>
        implicit lazy val stats = Statistics[IO].unsafeRunSync()

        def planAndRun = {
          val costTree = Planner.costTree[IO](x).unsafeRunSync()
          val p = Planner.plan(costTree)
          println(showDiff(p, costTree))
          println(s"inital plan cost: ${planCost(costTree)}")
          println(s"optimized plan cost: ${planCost(p)}")
          println(interpreter.Interpreter.run[IO]((), x, p, schema.state).unsafeRunSync())
        }

        planAndRun
        planAndRun
        planAndRun
      }

    go
  }

  println(Render.renderSchema(schema))

  val inputQuery = """
query withNestedFragments {
  doIdentity {
    value(num: 6, text: "world", xs: ["world", "hello"], input: {
      value: 42
      val2: "holla"
    })
  }
}
  """

  parseAndPrep(inputQuery).map { x =>
    implicit lazy val stats = Statistics[IO].unsafeRunSync()

    val costTree = Planner.costTree[IO](x).unsafeRunSync()
    val p = Planner.plan(costTree)
    println(showDiff(p, costTree))
    println(s"inital plan cost: ${planCost(costTree)}")
    println(s"optimized plan cost: ${planCost(p)}")
    println(interpreter.Interpreter.run[IO]((), x, p, schema.state).unsafeRunSync())
  }

  val qsig = """
query withNestedFragments {
  getData {
    nestedSignal2 {
      a
    }
    nestedSignal {
      a
      nestedSignal {
        a
        nestedSignal {
          a
          nestedSignal {
            a
          }
        }
      }
    }
  }
}
  """

  parseAndPrep(qsig).map { x =>
    implicit lazy val stats = Statistics[IO].unsafeRunSync()

    val costTree = Planner.costTree[IO](x).unsafeRunSync()
    println(showTree(0, costTree))

    interpreter.Interpreter
      .runStreamed[IO]((), x, schema.state)
      .evalMap(x => IO.println(s"got new subtree ${x.mkString_("\n")}"))
      .compile
      .drain
      .unsafeRunSync()
  }
}
