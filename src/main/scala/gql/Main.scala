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

  def showTree(indent: Int, nodes: NonEmptyList[Optimizer.Node]): String = {
    val pad = "  " * indent
    nodes
      .map { n =>
        val thisInfo =
          pad + s"name: ${n.name}, cost: ${n.cost.toInt}), start: ${n.start}, end: ${n.end}, id: ${n.id}\n"
        thisInfo + n.children.toNel.map(showTree(indent + 1, _)).mkString_("")
      }
      .mkString_("")
  }

  def showDiff_(fa: NonEmptyList[Optimizer.Node], fb: NonEmptyList[Optimizer.Node], maxEnd: Double): String = {
    fa.sortBy(_.id)
      .zip(fb.sortBy(_.id))
      .map { case (a, b) =>
        val per = math.max((maxEnd / 40d).toInt, 1)
        println(s"for $maxEnd ${b.name}: ${a.start.toInt}/$per")
        val thisInfo =
          if (a.end.toInt != b.end.toInt) {
            (" " * (b.start.toInt / per)) + AnsiColor.RED_B + s"name: ${b.name}, cost: ${b.cost.toInt}, start: ${b.start}, end: ${b.end}, id: ${b.id}" + AnsiColor.RESET + "\n" +
              (" " * (b.start.toInt / per)) + AnsiColor.BLUE_B + (">" * ((a.start - b.start).toInt / per)) + AnsiColor.GREEN_B + s"name: ${a.name}, cost: ${a.cost.toInt}, start: ${a.start}, end: ${a.end}, id: ${a.id}" + AnsiColor.RESET + "\n"
          } else
            (" " * (a.start.toInt / per)) + s"name: ${a.name}, cost: ${a.cost.toInt}, start: ${a.start}, end: ${a.end}, id: ${a.id}\n"

        thisInfo + a.children.toNel.map(showDiff_(_, b.children.toNel.get, maxEnd)).mkString_("")
      }
      .mkString_("")
  }

  def showDiff(fa: NonEmptyList[Optimizer.Node], fb: NonEmptyList[Optimizer.Node]) = {
    val me = Optimizer.flattenNodeTree(fa).maximumBy(_.end).end
    AnsiColor.RED_B + "old field schedule" + AnsiColor.RESET + "\n" +
      AnsiColor.GREEN_B + "new field schedule" + AnsiColor.RESET + "\n" +
      AnsiColor.BLUE_B + "new field offset (deferral of execution)" + AnsiColor.RESET + "\n" +
      showDiff_(fa, fb, me)
  }

  def planCost(nodes: NonEmptyList[Optimizer.Node]): Double = {
    // val fnt = Optimizer.flattenNodeTree(nodes)

    // fnt
    //   .groupBy(_.meta.map(_.name))
    //   .toList
    //   .collect { case (_, nodes) =>
    //     val c = nodes.head.meta.get.cost
    //     val e = nodes.head.meta.get.elemCost
    //     val costCnt = nodes.groupBy(_.start.toInt)
    //     val groups = costCnt.size
    //     val batched = nodes.size - groups
    //     groups * c + batched * e
    //   }
    //   .sumAll
    0d
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

  implicit val intInput: Input.Scalar[Int] = Input.Scalar("Int", Decoder.decodeInt)

  implicit val stringInput: Input.Scalar[String] = Input.Scalar("String", Decoder.decodeString)

  implicit def listInputType[A](implicit tpe: Input[A]): Input[Vector[A]] =
    Input.Arr(tpe)

  final case class IdentityData(value: Int, value2: String)

  val valueArgs: Output.Fields.Arg[(Int, String, Vector[String])] =
    (
      (
        arg[Int]("num", Some(42)),
        arg[Int]("num2", Some(9))
      ).mapN(_ + _),
      arg[String]("text"),
      arg[Vector[String]]("xs", Vector.empty.some)
    ).tupled
  implicit def identityDataType[F[_]](implicit F: Async[F]): Output.Obj[F, IdentityData] =
    outputObject[F, IdentityData](
      "IdentityData",
      "value" -> effectArg(valueArgs) { case (x, (y, z, hs)) =>
        F.pure(s"${x.value2} + $z - ${(x.value + y).toString()} - (${hs.mkString(",")})")
      }
    )

  implicit def dataType[F[_]: Async]: Output.Obj[F, Data[F]] =
    outputObject[F, Data[F]](
      "Data",
      "a" -> pure(_.a),
      "b" -> effect(_.b /*.delayBy(50.millis)*/ ),
      "c" -> effect(_.c.map(_.toVector) /*.delayBy(250.millis)*/ )
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
      "getOther" -> pure(_ => (C("hey", "tun"): D)),
      "doIdentity" -> pure(_ => IdentityData(2, "hello"))
    ),
    Map.empty
  )

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

  def go =
    parseAndPrep(qn).map { x =>
      implicit lazy val stats = Statistics[IO].unsafeRunSync()

      def planAndRun = {
        val costTree = Optimizer.costTree[IO](x).unsafeRunSync()
        val p = Optimizer.plan(costTree)
        println(showDiff(p, costTree))
        println(s"inital plan cost: ${planCost(costTree)}")
        println(s"optimized plan cost: ${planCost(p)}")
        println(Interpreter.Planned.run[IO]((), x, p).unsafeRunSync())
      }

      planAndRun
      planAndRun
    }

  go

  println(Render.renderSchema(schema))

  val inputQuery = """
query withNestedFragments {
  doIdentity {
    value(num: 6, text: "world", xs: ["world", "hello"])
  }
}
  """

  parseAndPrep(inputQuery).map { x =>
    implicit lazy val stats = Statistics[IO].unsafeRunSync()

    val costTree = Optimizer.costTree[IO](x).unsafeRunSync()
    val p = Optimizer.plan(costTree)
    println(showDiff(p, costTree))
    println(s"inital plan cost: ${planCost(costTree)}")
    println(s"optimized plan cost: ${planCost(p)}")
    println(Interpreter.Planned.run[IO]((), x, p).unsafeRunSync())
  }

  type H[A] = Fetch[IO, A]
  implicit def asyncForFetch[F[_]](implicit F: Async[F]): Async[Fetch[F, *]] = {
    type G[A] = Fetch[F, A]
    new Async[G] {
      override def map[A, B](fa: G[A])(f: A => B): G[B] =
        fetch.fetchM[F].map(fa)(f)

      override def map2[A, B, Z](fa: G[A], fb: G[B])(f: (A, B) => Z): G[Z] =
        fetch.fetchM[F].map2(fa, fb)(f)

      override def map2Eval[A, B, Z](fa: G[A], fb: Eval[G[B]])(
          f: (A, B) => Z
      ): Eval[G[Z]] = fetch.fetchM[F].map2Eval(fa, fb)(f)

      override def product[A, B](fa: G[A], fb: G[B]): G[(A, B)] =
        fetch.fetchM[F].product(fa, fb)

      override def productR[A, B](fa: G[A])(fb: G[B]): G[B] =
        fetch.fetchM[F].productR(fa)(fb)

      override def flatMap[A, B](fa: G[A])(f: A => G[B]): G[B] =
        fetch.fetchM[F].flatMap(fa)(f)

      override def pure[A](x: A): G[A] = Fetch.pure(x)

      override def raiseError[A](e: Throwable): G[A] = Fetch.error(e)

      override def handleErrorWith[A](fa: G[A])(f: Throwable => G[A]): G[A] = {
        val o =
          Fetch
            .run(fa.map[Either[A, G[A]]](Left(_)))
            .handleError(x => Right(f(x)))

        Fetch.liftF(o).flatMap {
          case Left(x)  => Fetch.pure(x)
          case Right(x) => x
        }
      }

      override def tailRecM[A, B](a: A)(f: A => G[Either[A, B]]): G[B] =
        Monad[G].tailRecM[A, B](a)(f)

      override def forceR[A, B](fa: G[A])(fb: G[B]): G[B] = {
        val a = Fetch.run(fa)
        val b = Fetch.run(fb)
        Fetch.liftF(F.forceR(a)(b))
      }

      override def uncancelable[A](body: Poll[G] => G[A]): G[A] = {
        val fa = F.uncancelable { poll =>
          val poll2 =
            new Poll[G] {
              override def apply[A](fa: G[A]): G[A] =
                Fetch.liftF(poll(Fetch.run(fa)))
            }
          Fetch.run(body(poll2))
        }
        Fetch.liftF(fa)
      }

      override def canceled: G[Unit] =
        Fetch.liftF(F.canceled)

      override def onCancel[A](fa: G[A], fin: G[Unit]): G[A] =
        Fetch.liftF(Fetch.run(fa).onCancel(Fetch.run(fin)))

      override def monotonic: G[FiniteDuration] =
        Fetch.liftF(F.monotonic)

      override def realTime: G[FiniteDuration] =
        Fetch.liftF(F.realTime)

      override def suspend[A](hint: cats.effect.kernel.Sync.Type)(thunk: => A): G[A] =
        Fetch.liftF(F.suspend(hint)(thunk))

      override def start[A](fa: G[A]): G[Fiber[G, Throwable, A]] = {
        val fib: F[Fiber[F, Throwable, A]] = Fetch.run(fa).start
        Fetch.liftF {
          fib.map { x =>
            new Fiber[G, Throwable, A] {
              override def cancel: G[Unit] = Fetch.liftF(x.cancel)
              override def join: G[Outcome[G, Throwable, A]] =
                Fetch.liftF(x.join.map(_.mapK(new FunctionK[F, G] {
                  override def apply[A](fa: F[A]): G[A] = Fetch.liftF(fa)
                })))
            }
          }
        }
      }

      override def cede: G[Unit] =
        Fetch.liftF(F.cede)

      override def ref[A](a: A): G[Ref[G, A]] =
        Fetch.liftF(
          F
            .ref(a)
            .map(_.mapK(new FunctionK[F, G] {
              override def apply[A](fa: F[A]): G[A] = Fetch.liftF(fa)
            }))
        )

      override def deferred[A]: G[Deferred[G, A]] =
        Fetch.liftF(
          F
            .deferred[A]
            .map(_.mapK(new FunctionK[F, G] {
              override def apply[A](fa: F[A]): G[A] = Fetch.liftF(fa)
            }))
        )

      override def sleep(time: FiniteDuration): G[Unit] =
        Fetch.liftF(F.sleep(time))

      override def evalOn[A](fa: G[A], ec: ExecutionContext): G[A] =
        Fetch.liftF(F.evalOn(Fetch.run(fa), ec))

      override def executionContext: G[ExecutionContext] =
        Fetch.liftF(F.executionContext)

      override def cont[K, R](body: cats.effect.kernel.Cont[G, K, R]): G[R] =
        Async.defaultCont[G, K, R](body)
    }
  }

  val schema2 = Schema[H, Unit](
    outputObject[H, Unit](
      "Query",
      "getData" -> pure(_ => root[H]),
      "getDatas" -> pure(_ => datasRoot[H]),
      "getInterface" -> pure(_ => (C("hey", "tun"): A)),
      "getOther" -> pure(_ => (C("hey", "tun"): D)),
      "doIdentity" -> pure(_ => IdentityData(2, "hello"))
    ),
    Map.empty
  )

  def parseAndPrepFetch(q: String): Option[NonEmptyList[PreparedQuery.PreparedField[H, Any]]] =
    p.parseAll(q).map(PreparedQuery.prepare(_, schema2, Map.empty)) match {
      case Left(e) =>
        println(errorMessage(q, e))
        None
      case Right(Left(x)) =>
        println(x)
        None
      case Right(Right(x)) => Some(x)
    }

  println("running with fetch")

  parseAndPrepFetch(qn).map { x =>
    implicit lazy val stats = Fetch.run(Statistics[H]).unsafeRunSync()

    val costTree = Fetch.run(Optimizer.costTree[H](x)).unsafeRunSync()
    val p = Optimizer.plan(costTree)
    println(showDiff(p, costTree))
    println(s"inital plan cost: ${planCost(costTree)}")
    println(s"optimized plan cost: ${planCost(p)}")
    println(Fetch.run(Interpreter.Planned.run[H]((), x, p)).unsafeRunSync())
  }
}
