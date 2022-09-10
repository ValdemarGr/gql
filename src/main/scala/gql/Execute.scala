package gql

import gql.parser.{QueryParser => P}
import fs2.Stream
import cats.implicits._
import cats.effect._
import io.circe._
import io.circe.syntax._
import cats.data._
import gql.interpreter._

object Execute {

  // def getExceptions(xs: Chain[EvalFailure]) =
  //   xs.map ( ef => )
  sealed trait ExecutorOutcome[F[_], Q, M, S]
  object ExecutorOutcome {
    final case class ValidationError[F[_], Q, M, S](msg: String) extends ExecutorOutcome[F, Q, M, S]
    final case class Query[F[_], Q, M, S](run: Q => F[JsonObject]) extends ExecutorOutcome[F, Q, M, S]
    final case class Mutation[F[_], Q, M, S](run: M => F[JsonObject]) extends ExecutorOutcome[F, Q, M, S]
    final case class Stream[F[_], Q, M, S](run: S => fs2.Stream[F, JsonObject]) extends ExecutorOutcome[F, Q, M, S]
  }

  // def executor[F[_]: Statistics, Q, M, S](
  //     query: NonEmptyList[P.ExecutableDefinition],
  //     schema: Schema[F, Q],
  //     variables: Map[String, Json],
  //     schemaState: SchemaState[F]
  // )(implicit F: Async[F]): ExecutorOutcome[F, Q, M, S] = {
  //   PreparedQuery.prepare2(query, schema, variables) match {
  //     case Left(err) => ExecutorOutcome.ValidationError(err)
  //     case Right(x) =>
  //       x match {
  //         case PreparedQuery.OperationType.Query(rootFields) =>
  //           ExecutorOutcome.Query[F, Q, M, S](Interpreter.runSync(_, rootFields, schemaState))
  //         case PreparedQuery.OperationType.Mutation(rootFields) =>
  //           ExecutorOutcome.Mutation[F, Q, M, S](Interpreter.runSync(_, rootFields, schemaState))
  //         case PreparedQuery.OperationType.Subscription(dataStream, root) =>
  //           ExecutorOutcome.Stream[F, Q, M, S] { s =>
  //             dataStream(s).switchMap(Interpreter.runStreamed[F](_, NonEmptyList.one(root), schemaState))
  //           }
  //       }
  //   }
  // }
  def formatErrors(xs: Chain[EvalFailure]) =
    Json.arr(

      // xs.flatMap { ef =>
      //   ef.meta.map { nm =>
      //     JsonObject(
      //       "message" -> Json.fromString(ef.error.getOrElse("internal error")),
      //       "locations" -> Json.arr(nm.absolutePath.path.map(x => Json.fromString(x.toString())).toList: _*)
      //       // "path" -> nm.asJson
      //     ).asJson
      //   }
      // }.toList: _*
    )
}
