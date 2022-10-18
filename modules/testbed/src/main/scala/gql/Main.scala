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
import scala.concurrent.ExecutionContext
import alleycats.Empty
import gql.resolver._
import cats.mtl._
import cats.instances.unit
import gql.parser.ParserUtil
import gql.ast._
import fs2.Pull
import fs2.concurrent.SignallingRef
import org.http4s.blaze.server.BlazeServerBuilder
import gql.http4s.Http4sRoutes
import gql.http4s.Http4sCompiler

object Main extends IOApp {

  def showTree(indent: Int, nodes: List[Planner.Node]): String = "" /*{
    val pad = "  " * indent
    nodes
      .map { n =>
        val thisInfo =
          pad + s"name: ${n.name}, cost: ${n.cost.toInt}), start: ${n.start}, end: ${n.end}, id: ${n.id}\n"
        thisInfo + n.children.toNel.map(showTree(indent + 1, _)).mkString_("")
      }
      .mkString_("")
  }*/

  def showDiff_(fa: NonEmptyList[Planner.Node], fb: NonEmptyList[Planner.Node], maxEnd: Double): String = "" /*{
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
  }*/

  def showDiff(fa: NonEmptyList[Planner.Node], fb: NonEmptyList[Planner.Node]) = "" /*{
    val me = Planner.NodeTree(fa).flattened.maximumBy(_.end).end
    AnsiColor.RED_B + "old field schedule" + AnsiColor.RESET + "\n" +
      AnsiColor.GREEN_B + "new field schedule" + AnsiColor.RESET + "\n" +
      AnsiColor.BLUE_B + "new field offset (deferral of execution)" + AnsiColor.RESET + "\n" +
      showDiff_(fa, fb, me)
  }*/

  def planCost(nodes: NonEmptyList[Planner.Node]): Double = 0d /*{
    val fnt = Planner.NodeTree(nodes).flattened

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
  }*/

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

  import gql.dsl._

  final case class Person(
      name: String,
      age: Int
  )

  def getPerson(name: String) =
    if (name == "John") Person("John", 30)
    else if (name == "Bob") Person("Bob", 40)
    else Person("Jane", 25)

  def getFriends(name: String) = IO {
    if (name == "John") List("Jane", "Bob")
    else if (name == "Bob") List("John", "Jane")
    else List("John")
  }

  implicit lazy val personType: Type[IO, Person] = tpe[IO, Person](
    "Person",
    "name" -> pure(_.name),
    "age" -> pure(_.age),
    "friends" -> eff(x => getFriends(x.name).map(_.map(getPerson)))
  )

  val schemaShape =
    BatchResolver[IO, String, String](xs => IO(xs.map(x => x -> x).toMap)).map { r =>
      val fixed = r.contramap[String](Set(_)).map { case (i, o) => getPerson(o.values.toList.head) }

      final case class Nest(name: String)
      implicit lazy val nestType: Type[IO, Nest] = tpe[IO, Nest](
        "Nest",
        "person" -> field(fixed.contramap[Nest](_.name))
      )

      SchemaShape[IO, Unit, Unit, Unit](
        tpe[IO, Unit](
          "Query",
          "person1" -> field(fixed.contramap[Unit](_ => "John")).document("John"),
          "person2" -> field(fixed.contramap[Unit](_ => "Jane")).document("Jane"),
          "nest" -> pure(_ => Nest("Bob"))
        ).document {
          """|Query
               |The Query type is the entrypoint for most operations.
               |Something else cool.
               |  Indented""".stripMargin
        }
      )
    }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      schema0 <- Schema.stateful(schemaShape)
      schema = schema0.copy(planner = new Planner[IO] {
        def plan(naive: Planner.NodeTree): IO[Planner.NodeTree] =
          schema0.planner.plan(naive).flatTap { output =>
            IO.println(output.show(showImprovement = true)) >>
              IO.println(naive.totalCost) >>
              IO.println(output.totalCost)
          }
      })

      _ <- {
        Compiler[IO].compile(
          schema,
          """
      query {
        person1 {
          name
          age
          friends {
            name
            age
          }
        }
        person2 {
          name
          age
          friends {
            name
            age
          }
        }
        nest {
          person {
            name
            age
            friends {
              name
              age
              friends {
                name
                age
              }
            }
          }
        }
      }
    """
        ) match {
          case Left(err) => IO.println(err)
          case Right(x) =>
            x match {
              case Application.Query(q) => q.flatMap(IO.println)
            }
        }
      }

      compiler = Compiler[IO].make(schema)

      _ <- IO.println(schema.shape.render)

      ec <- BlazeServerBuilder[IO]
        .withHttpWebSocketApp { wsb =>
          val s = Http4sRoutes.sync[IO](Http4sCompiler.fromCompiler[IO](compiler))
          val ws = Http4sRoutes.ws[IO](_ => IO.pure(Right(compiler)), wsb)
          (s <+> ws).orNotFound
        }
        .bindHttp(8080, "127.0.0.1")
        .serve
        .compile
        .lastOrError
    } yield ec
  }

  // Auto apply + covariance

  trait Base[+F[_], A]

  trait InvariantBase[F[_], A] extends Base[F, A]

  // trait Inv[B[f[_], y] <: Base[f, y]] {
  //   type B0[F[_], A] = B[F, A]
  // }
  // implicit def autoApplyInv[B[f[_], y] <: Base[f, y]]: Inv[B] =

  final case class Impl[+F[_], A]() extends Base[F, A]

  sealed trait Other[F[_], A]

  def fromBase[F[_], A](implicit base: Base[F, A]): Other[F, A] = ???

  // def pure[F[_], A](f: Unit => A)(implicit base: Base[F, A]): Other[F, A] = ???

  implicit def impl1[F[_]]: Impl[F, Int] = Impl[F, Int]()

  implicit lazy val impl2: Impl[Nothing, String] = Impl[Nothing, String]()

  implicit def impl3[F[_]: Monad]: Impl[F, Boolean] = Impl[F, Boolean]()

  // "field1" -> pure[IO, Boolean](_ => true)

  fromBase[IO, Int]
  fromBase[IO, String]
  // fromBase[IO, Boolean]
}
