package gql.http4s

import io.circe.syntax._
import org.http4s.blaze.server.BlazeServerBuilder
import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe._
import gql.parser.ParserUtil
import gql.Main
import cats.data.Kleisli

trait InterpretationVisitor[F[_]] {
  def onBatch(name: String, size: Int) = ???
}

object Http4sMain extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    type G[A] = Kleisli[IO, Main.Deps, A]
    ???
    // gql.Schema.stateful(Main.testSchemaShape[G]).flatMap { s =>
    //   BlazeServerBuilder[IO]
    //     .withHttpApp(
    //       HttpRoutes
    //         .of[IO] {
    //           case req @ POST -> Root / "graphql" =>
    //             req
    //               .as[Json]
    //               .map(_.asObject.get)
    //               .flatMap { jo =>
    //                 val query = jo("query").get.asString.get
    //                 val variabels = jo("variables").flatMap(_.asObject).map(_.toMap).getOrElse(Map.empty)
    //                 gql.parser.parse(query) match {
    //                   case Left(err) =>
    //                     println(err.prettyError.value)
    //                     BadRequest(
    //                       JsonObject(
    //                         "errors" -> Seq(
    //                           JsonObject(
    //                             "message" -> "could not parse query".asJson,
    //                             "locations" -> Seq(
    //                               JsonObject(
    //                                 "line" -> err.caret.line.asJson,
    //                                 "column" -> err.caret.col.asJson
    //                               ).asJson
    //                             ).asJson
    //                           ).asJson
    //                         ).asJson
    //                       ).asJson
    //                     )
    //                   case Right(ed) =>
    //                     gql.PreparedQuery.prepare(ed, s, variabels) match {
    //                       case Left(err) => BadRequest(err.toString())
    //                       case Right(x) =>
    //                         gql.interpreter.Interpreter
    //                           .runSync[G]((), x, s.state)
    //                           .run(Main.Deps("lol"))
    //                           .flatMap { case (errs, res) =>
    //                             val e = null //gql.Execute.formatErrors(errs)
    //                             Ok(
    //                               JsonObject(
    //                                 "data" -> res.asJson
    //                                 // "errors" -> e.asJson
    //                               ).asJson
    //                             )
    //                           }
    //                     }
    //                 }
    //               }
    //           case GET -> Root => Ok()
    //           case x           => Ok()
    //         }
    //         .orNotFound
    //     )
    //     .withoutSsl
    //     .bindHttp()
    //     .serve
    //     .compile
    //     .lastOrError
    // }
  }

  println("holla world")
}
