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

object Main extends App {
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
        Some(
          tpe[IO, Unit](
            "Query",
            "person1" -> field(fixed.contramap[Unit](_ => "John")),
            "person2" -> field(fixed.contramap[Unit](_ => "Jane")),
            "nest" -> pure(_ => Nest("Bob"))
          )
        )
      )
    }

  val schema0 = Schema.stateful(schemaShape).unsafeRunSync()
  val schema = schema0.copy(planner = new Planner[IO] {
    def plan(naive: Planner.NodeTree): IO[Planner.NodeTree] =
      schema0.planner.plan(naive).flatTap { output =>
        IO.println(output.show(showImprovement = true)) >>
          IO.println(naive.totalCost) >>
          IO.println(output.totalCost)
      }
  })

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
    case Left(err) => println(err)
    case Right(x) =>
      x match {
        case Application.Query(q) => println(q.unsafeRunSync())
      }
  }
}
