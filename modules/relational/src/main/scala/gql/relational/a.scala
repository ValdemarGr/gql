package gql.relational

import cats.effect._
import skunk.implicits._
import gql.ast._
import gql.dsl._
import cats.implicits._
import fs2.Pure
import skunk.codec.all._
import skunk._
import cats._
import gql.resolver.Resolver
import cats.data._
import scala.reflect.ClassTag
import java.util.UUID
import gql.Arg
import gql.EmptyableArg
import gql.resolver.FieldMeta
import cats.mtl.Stateful
import cats.mtl.Tell
import gql.{preparation => prep}
import cats.mtl.Raise
import gql.Schema
import gql.SchemaShape
import gql.Application
import natchez.TraceValue
import natchez.Kernel
import natchez.Span
import java.net.URI
import cats.arrow.FunctionK
import doobie.util.transactor

object Main extends IOApp.Simple {
  override def run: IO[Unit] = {
    implicit val emptyIOTrace = new natchez.Trace[IO] {
      override def put(fields: (String, TraceValue)*): IO[Unit] = IO.unit
      override def log(fields: (String, TraceValue)*): IO[Unit] = IO.unit
      override def log(event: String): IO[Unit] = IO.unit
      override def attachError(err: Throwable, fields: (String, TraceValue)*): IO[Unit] = IO.unit
      override def kernel: IO[Kernel] = IO.pure(Kernel(Map.empty))
      override def spanR(name: String, options: Span.Options): Resource[IO, IO ~> IO] = Resource.pure(FunctionK.id[IO])
      override def span[A](name: String, options: Span.Options)(k: IO[A]): IO[A] = k
      override def traceId: IO[Option[String]] = IO.pure(None)
      override def traceUri: IO[Option[URI]] = IO.pure(None)
    }

    val doobieConn = transactor.Transactor.fromDriverManager[IO](
        driver = "org.postgresql.Driver",  // JDBC driver classname
        url = "jdbc:postgresql:postgres",     // Connect URL
        user = "postgres",                 // Database user name
        password = "1234",             // Database password
        logHandler = None   
    )
    val pool =
      Session
        .single[IO](
          host = "127.0.0.1",
          user = "postgres",
          database = "postgres",
          password = "1234".some
        )

    val xaPool = pool.flatMap(ses => ses.transaction as ses)

    import gql.relational.MySchema
    import gql.relational.SkunkSchema
    val ms = new MySchema(xaPool)
    import MySchema._
    import ms._
    import ExampleDoobie.{ContractTable => CT}
    import SkunkSchema._
    implicit val c0: Out[IO, QueryResult[MySchema.ContractTable]] = ms.contract
    val ss = SchemaShape.unit[IO](
      fields[IO, Unit](
        "name" -> lift(_ => "edlav"),
        "contract" -> SkunkSchema.runField(xaPool, arg[UUID]("contractId"))((_: NonEmptyList[Unit], a: UUID) =>
          MySchema.contractTable.join[Option](c => sql"${c.id} = ${uuid}".apply(a)).map(t => (().pure[SkunkSchema.Query.Select], t))
        ),
        "contract2" -> DoobieIntegraion.runField[IO, List, Unit, CT](doobieConn){(_: NonEmptyList[Unit]) =>
          import doobie.implicits._
          ExampleDoobie.contractTable.join[List](x => fr"1 = 1").map(t => (().pure[DoobieIntegraion.Query.Select], t))
        }
      )
    )

    Schema.simple(ss).flatMap { schema =>
      gql
        .Compiler[IO]
        .compile(
          schema,
          """
            query {
              contract(contractId: "1ff0ca77-c13f-4af8-9166-72373f309247") {
                name
                id
                fastEntities {
                  name
                  age
                  pets {
                    name
                    id
                  }
                }
              }
              contract2 {
                name
                id
              }
            }
          """
        ) match {
        case Right(Application.Query(run)) => run.flatMap(IO.println)
        case x                             => IO.println(x)
      }
    }
  }
}
