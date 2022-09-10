package gql

import cats.data._
import cats.implicits._
import gql.interpreter._
import io.circe._
import io.circe.syntax._

object ResultEmitter {
  final case class Result(
    outputObject: Json,
    exceptions: Chain[Throwable],
  )

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

  // def getExceptions(xs: Chain[EvalFailure]) =
  //   xs.map ( ef => )
}
