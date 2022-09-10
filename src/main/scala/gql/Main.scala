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
import gql.parser.QueryParser
import gql.parser.QueryParser.Value._
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
import cats.mtl._
import gql.out._
import cats.instances.unit
import gql.parser.ParserUtil

object Main extends App {
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

  val p = QueryParser.executableDefinition.rep
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
  implicit def intType[F[_]]: Scalar[F, Int] = Scalar("Int", Encoder.encodeInt)

  implicit def stringType[F[_]]: Scalar[F, String] = Scalar("String", Encoder.encodeString)

  implicit def listTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Vector[A]] = Arr(of)
  implicit def seqTypeForAny[F[_], A](implicit of: Output[F, A]): Output[F, Seq[A]] = Arr(of)

  implicit def optTypeForSome[F[_], A](implicit of: Output[F, A]): Output[F, Option[A]] = Opt(of)

  implicit val intInput: Input.Scalar[Int] = Input.Scalar("Int", Decoder.decodeInt)

  implicit val stringInput: Input.Scalar[String] = Input.Scalar("String", Decoder.decodeString)

  implicit def listInputType[A](implicit tpe: Input[A]): Input[Vector[A]] = Input.Arr(tpe)

  final case class Deps(v: String)

  def testSchemaShape[F[_]](implicit F: Async[F], Ask: Ask[F, Deps]): State[SchemaState[F], SchemaShape[F, Unit]] = {
    final case class IdentityData(value: Int, value2: String)

    final case class InputData(
        value: Int,
        val2: String
    )

    final case class ServerData(value: Int)

    final case class DataIds(ids: List[Int])

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

        implicit def identityDataType: Obj[F, IdentityData] =
          obj[F, IdentityData](
            "IdentityData",
            "value" -> effect((valueArgs, inputDataArg).tupled) { case (x, ((y, z, hs), i)) =>
              F.pure(s"${x.value2} + $z - ${(x.value + y).toString()} - (${hs.mkString(",")}) - $i")
            }
          )

        implicit def serverData: Obj[F, ServerData] =
          obj[F, ServerData](
            "ServerData",
            "value" -> pure(_.value)
          )

        implicit lazy val dataType: Obj[F, Data[F]] =
          obj2[F, Data[F]]("Data") { f =>
            fields(
              "dep" -> f(eff(_ => Ask.reader(_.v))),
              "a" -> f(full(x => IorT.bothT[F]("Oh no, an error!", "Hahaa"))),
              "a2" -> f(arg[Int]("num", Some(42)))(pur { case (i, _) => i.a }),
              "b" -> f(eff(_.b)),
              "sd" -> f(serverDataBatcher.traverse(x => IorT.liftF(x.b.map(i => Seq(i, i + 1, i * 2))))),
              "c" -> f(eff(_.c.map(_.toSeq))),
              "doo" -> f(pur(_ => Vector(Vector(Vector.empty[String])))),
              "nestedSignal" ->
                f(
                  nameStreamReference[F, Data[F], Data[F]](eff { case (i, _) => i.c.map(_.head) })(_ => IorT.pure(()))(i =>
                    // IorT.liftF(F.pure(i.a))
                    IorT.leftT[F, String]("Hahaaaa, Nested signal errrorrororor!")
                  )
                ),
              "nestedSignal2" ->
                f(
                  nameStreamReference[F, Data[F], Data[F]](eff { case (i, _) => i.c.map(_.head) })(_ => IorT.pure(()))(i =>
                    IorT.liftF(F.pure(i.a))
                  )
                )
            )
          }

        implicit def otherDataType: Obj[F, OtherData[F]] =
          obj[F, OtherData[F]](
            "OtherData",
            "value" -> pure(_.value),
            "d1" -> effect(_.d1)
          )

        implicit def datasType: Union[F, Datas[F]] =
          union[F, Datas[F]](
            "Datas",
            contra[Data[F]] { case Datas.Dat(d) => d },
            contra[OtherData[F]] { case Datas.Other(o) => o }
          )

        trait A {
          def a: String
        }
        object A {
          implicit def t: Interface[F, A] =
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
          implicit def t: Interface[F, D] =
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
          implicit def t: Obj[F, B] = obj[F, B]("B", "a" -> pure(_ => "B"), "b" -> pure(_ => Option("BO")))
        }
        final case class C(a: String, d: String) extends A with D
        object C {
          implicit def t: Obj[F, C] = obj[F, C]("C", "a" -> pure(_ => "C"), "d" -> pure(_ => "D"))
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

  type D[A] = Kleisli[IO, Deps, A]

  def mainProgram[F[_]](implicit F: Async[F], A: Ask[F, Deps], C: std.Console[F]): F[Unit] = {
    val schema = Schema.stateful[F, Unit](testSchemaShape[F])

    def parseAndPrep(q: String): Option[NonEmptyList[PreparedQuery.PreparedField[F, Any]]] =
      ParserUtil.parse(q).map(PreparedQuery.prepare(_, schema, Map.empty)) match {
        case Left(e) =>
          println(e.prettyError.value)
          None
        case Right(Left(x)) =>
          println(x)
          None
        case Right(Right(x)) => Some(x)
      }

    val qsig = """
query withNestedFragments {
  getData {
    dep
    doo
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

    F.fromOption(parseAndPrep(qn), new Exception(":((")).flatMap { x =>
      Statistics[F].flatMap { implicit stats =>
        Planner.costTree[F](x).flatMap { costTree =>
          println(showTree(0, costTree))

          interpreter.Interpreter
            .runSync[F]((), x, schema.state)
            .flatMap(x => C.println(x))
        }
      }
    } >>
      F.fromOption(parseAndPrep(qsig), new Exception(":((")).flatMap { x =>
        Statistics[F].flatMap { implicit stats =>
          Planner.costTree[F](x).flatMap { costTree =>
            println(showTree(0, costTree))

            interpreter.Interpreter
              .runStreamed[F]((), x, schema.state)
              .evalMap { case (failures, x) =>
                C.println(s"got new subtree") >>
                  C.println("errors:") >>
                  C.println(Execute.formatErrors(failures)) >>
                  C.println(x.toString())
              }
              .take(10)
              .compile
              .drain
          }
        }
      }
  }

  mainProgram[D].run(Deps("hey")).unsafeRunSync()
}
